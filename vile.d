/**
 * The Vile programming language. Bootstrapped from the D programming language.
 *
 * Features:
 * - Symbolic Expressions, homoiconicity
 * - Gradual typing, fully dynamic to fully static
 * - Extensible compiler, full power of defmacro
 * - FFI with automatic bindings to C, C++ and Objective-C
 * - Language server, expose app internals to most editors
 *
 * FIX METHOD SIGNATURE PRINT
 * TODO:
 * - C structure field access
 * - file loading
 * - Finish ffi
 * - objc: id in Any, Class in Struct :: remove casts
 * - fix toString
 * - alloc structs
 * - ffi struct types
 * - store FFI internals in special namespaces, Vilify before exposing to NS
 * - generate stack traces on SIGSEGV, handle more signals
 * - use levenshteinDistance over env for unresolved symbols
 * - literate text
 * - Type vars, intern built-in types
 * - C char vs byte, c strings
 * - Define exprs
 * - Cache output of clang
 * - CXX mangle
 * - Type qualifiers (constant, nullable, etc)
 */
module vile;

// Platform-dependent Library
version (Posix) {
  import dl  = core.sys.posix.dlfcn;    // Dynamic symbol linking
  import mem = core.sys.posix.sys.mman; // Memory management
  import pwd = core.sys.posix.pwd;
  import sig = core.sys.posix.signal;   // Signal handling
  import std = core.sys.posix.unistd;
}
else version (Windows) {
  import win32  = core.sys.windows.winbase;
  import wincon = core.sys.windows.wincon;
  import windef = core.sys.windows.windef;
  import winerr = core.sys.windows.winerror;
  import winlm  = core.sys.windows.lm;
  import winnls = core.sys.windows.winnls;
  import winnt  = core.sys.windows.winnt;
  import winsec = core.sys.windows.secext;
  import winshl = core.sys.windows.shlobj;
  import wintyp = core.sys.windows.basetyps;

  debug (TEST_VISUAL_STUDIO) enum win32LibRoot = "../../";
  else enum win32LibRoot = "";
}
else static assert(0, "Unsupported bootstrap platform");

// C Standard Library
import cstdio = core.stdc.stdio : EOF, fprintf, printf;
import cerror = core.stdc.errno;

import core.stdc.stdlib : alloca, exit, malloc, free, EXIT_SUCCESS;
import core.stdc.string : memcpy, memmove, strlen;

// D Runtime Library
import core.atomic  : atomicLoad, atomicStore;
import core.memory  : GC;
import core.runtime : defaultTraceHandler;
import core.time    : MonoTime;
import core.thread  : Thread;

import core.sync.semaphore : Semaphore;

// D Standard Library
import net = std.socket : Socket;
import opt = std.getopt : getopt;

import std.algorithm.comparison : levenshteinDistance, max, min;
import std.algorithm.searching  : countUntil;
import std.algorithm.sorting    : sort;
import std.container.rbtree     : RedBlackTree;
import std.digest.murmurhash    : MurmurHash3;
import std.range.primitives     : isOutputRange;

import std.array     : Appender, appender, empty, join;
import std.ascii     : isDigit, isHexDigit, isUpper;
import std.bigint    : BigInt;
import std.bitmanip  : bitfields, swapEndian;
import std.conv      : text, to;
import std.exception : assumeUnique, enforce;
import std.file      : readText, write;
import std.format    : format, formattedWrite;
import std.json      : JSONType, JSONValue, parseJSON;
import std.math      : abs;
import std.meta      : AliasSeq;
import std.path      : absolutePath;
import std.process   : execute, pipeProcess, wait;
import std.regex     : Regex, ctRegex, matchAll, matchFirst, regex, replaceAll;
import std.stdio     : stderr, stdout, writefln, writeln;
import std.string    : endsWith, indexOf, lastIndexOf, replace, split, splitLines;
import std.string    : startsWith, strip, toStringz;
import std.typecons  : Flag;
import std.utf       : decode, toUTF16z;
import std.uni       : isWhite;
import std.uuid      : parseUUID, UUID;
import std.stdio : File; // TODO remove
import d_compiler = std.compiler;

enum pathPattern = ctRegex!(`\\+`);

version (Windows) {
  enum oldIntegerSuffix = ctRegex!(`u?i(?:8|16|32|64)$`);
}

// Objective-C

version (OSX) extern (C) {
  pragma(lib, "objc");

  struct objc_class;
  alias Class = objc_class*;

  struct objc_object { Class isa; }
  alias id = objc_object*;

  struct objc_selector;
  alias SEL = objc_selector*;
  alias IMP = void function();
  alias BOOL = bool;

  const(char)* sel_getName(SEL sel);
  SEL sel_registerName(const(char)* str);
  const(char)* object_getClassName(id obj);
  void* object_getIndexedIvars(id obj);
  BOOL sel_isEqual(SEL lhs, SEL rhs);
  BOOL sel_isMapped(SEL sel);
  SEL sel_getUid(const(char)* str);

  struct objc_method;
  alias Method = objc_method*;
  struct objc_ivar;
  alias Ivar = objc_ivar*;
  struct objc_category;
  alias Category = objc_category*;
  struct objc_property;
  alias objc_property_t = objc_property*;

  alias Protocol = objc_object;

  id object_copy(id obj, size_t size);
  id object_dispose(id obj);
  Class object_getClass(id obj);
  id object_setClass(id obj, Class cls);
  id object_isClass(id obj);
  id object_getIvar(id obj, Ivar ivar);
  id object_setIvar(id obj, Ivar ivar, id value);
  void object_setIvarWithStrongDefault(id obj, Ivar ivar, id value);
  Ivar object_setInstanceVariable(id obj, const(char)* name, void* value);
  Ivar object_setInstanceVariableWithStrongDefault
    (id obj, const(char)* name, void* value);
  Ivar object_getInstanceVariable(id obj, const(char)* name, void** outValue);

  Class objc_getClass(const(char)* name);
  Class objc_getMetaClass(const(char)* name);
  Class objc_lookUpClass(const(char)* name);
  Class objc_getRequiredClass(const(char)* name);
  int objc_getClassList(Class* buffer, int bufferCount);
  Class* objc_copyClassList(uint* outCount);

  const(char)* class_getName(Class cls);
  BOOL class_isMetaClass(Class cls);
  Class class_getSuperClass(Class cls);
  int class_getVersion(Class cls);
  void class_setVersion(Class cls, int version_);
  size_t class_getInstanceSize(Class cls);
  Ivar class_getInstanceVariable(Class cls, const(char)* name);
  Ivar class_getClassVariable(Class cls, const(char)* name);
  Ivar class_copyIvarList(Class cls, uint* outCount);
  Method class_getInstanceMethod(Class cls, SEL name);
  Method class_getClassMethod(Class cls, SEL name);
  IMP class_getMethodImplementation(Class cls, SEL name);
  IMP class_getMethodImplementation_stret(Class cls, SEL name);
  BOOL class_respondsToSelector(Class cls, SEL sel);
  Method* class_copyMethodList(Class cls, uint* outCount);
  BOOL class_conformsToProtocol(Class cls, Protocol* protocol);
  Protocol** class_copyProtocolList(Class cls, uint* outCount);
  objc_property_t class_getProperty(Class cls, const(char)* name);
  objc_property_t* class_copyPropertyList(Class cls, uint* outCount);
  const(ubyte)* class_getIvarLayout(Class cls);
  const(ubyte)* class_getWeakIvarLayout(Class cls);
  BOOL class_addMethod(Class cls, SEL name, IMP imp, const(char)* types);
  IMP class_replaceMethod(Class cls, SEL name, IMP imp, const(char)* types);
  BOOL class_addIvar(Class cls, const(char)* name, size_t size,
                     ubyte alignment, const(char)* types);
  BOOL class_addProtocol(Class cls, Protocol* protocol);
  // BOOL class_addProperty(Class cls, const(char)* name,
  //                        const(objc_property_attribute_t)* attributes,
  //                        uint attributeCount);
  // void class_replaceProperty(Class cls, const(char)* name,
  //                            const(objc_property_attribute_t)* attributes,
  //                            uint attributeCount);
  void class_setIvarLayout(Class cls, const(ubyte)* layout);
  void class_setWeakIvarLayout(Class cls, const(ubyte)* layout);
  Class objc_getFutureClass(const(char)* name);

  id class_createInstance(Class cls, size_t extraBytes);
  id objc_constructInstance(Class cls, void* bytes);
  void* objc_destructInstance(id obj);

  Class objc_allocateClassPair(Class superclass, const(char)* name,
                               size_t extraBytes);
  void objc_registerClassPair(Class cls);
  Class objc_duplicateClass(Class original, const(char)* name,
                            size_t extraBytes);
  void objc_disposeClassPair(Class cls);

  SEL method_getName(Method m);
  IMP method_getImplementation(Method m);
  const(char)* method_getTypeEncoding(Method m);
  uint method_getNumberOfArguments(Method m);
  char* method_copyReturnType(Method m);
  char* method_copyArgumentType(Method m, uint index);
  void method_getReturnType(Method m, char* dst, size_t dst_len);
  void method_getArgumentType(Method m, uint index, char* dst, size_t dst_len);
  // objc_method_description* method_getDescription(Method m);
  IMP method_setImplementation(Method m, IMP imp);
  void method_exchangeImplementation(Method m1, Method m2);

  const(char)* ivar_getName(Ivar v);
  const(char)* ivar_getTypeEncoding(Ivar v);
  ptrdiff_t ivar_getOffset(Ivar v);

  const(char)* property_getName(objc_property_t property);
  const(char)* property_getAttributes(objc_property_t property);
  // objc_property_attribute_t* property_copyAttributeList
  //   (objc_property_t property, uint* outCount);
  char* property_copyAttributeValue(objc_property_t property,
                                    const(char)* attributeName);

  Protocol* objc_getProtocol(const(char)* name);
  Protocol** objc_copyProtocolList(uint* outCount);
  BOOL protocol_conformsToProtocol(Protocol* proto, Protocol* other);
  BOOL protocol_isEqual(Protocol* proto, Protocol* other);
  const(char)* protocol_getName(Protocol* proto);
  // TODO

}

// Classes used in the ObjC integration. Note: eval only uses the C objc API.
version (D_ObjectiveC) extern (Objective-C) {
  extern class NSObject {
    nothrow @nogc:
    void retain() @selector("retain");
    void release() @selector("release");
    NSString description() @selector("description");
    NSString debugDescription() @selector("debugDescription");
    // BOOL isEqual(id) @selector("isEqual:");
    uint hash() @selector("hash");
  }

  extern class NSString : NSObject {
    nothrow @nogc:
    static NSString stringWithCharacters(const(wchar)*, uint) @selector("stringWithCharacters:length:");
    const(char)* UTF8String() @selector("UTF8String");
  }
}
version (none)
version (D_ObjectiveC) {
  __gshared {
    NSString[Sym*] nsStringInterns;
  }
  NSString nsString(Sym* s) { // TODO: poor man's Sym->NSString
    if (auto p = s in nsStringInterns) return *p;
    if (!nsStringClass)
      nsStringClass = cast(NSString_Class)objc_getClass("NSString").enforce();
    auto ws  = s.name.to!wstring;
    auto str = nsStringClass.stringWithCharacters(ws.ptr, ws.length.to!uint);
    return nsStringInterns[s] = str.enforce();
  }
}

// Linenoise
extern (C) nothrow {
  version (Windows) pragma(lib, win32LibRoot ~ "lib/windows/x86_64/linenoise.lib");

  struct linenoiseCompletions {
    size_t len;
    char** cvec;
  }

  alias linenoiseCompletionCallback =
    void function(const(char)*, linenoiseCompletions*);
  alias linenoiseHintsCallback =
    char* function(const(char)*, int* color, int* bold);
  alias linenoiseFreeHintsCallback = void function(void*);

  void linenoiseSetCompletionCallback(linenoiseCompletionCallback);
  version(Windows) {}
  else {
    void linenoiseSetHintsCallback(linenoiseHintsCallback);
    void linenoiseSetFreeHintsCallback(linenoiseFreeHintsCallback);
  }
  void linenoiseAddCompletion(linenoiseCompletions*, const(char)*);

  char* linenoise(const(char)* prompt);

  version(Windows) alias linenoiseFree = free;
  else void  linenoiseFree(void* ptr);

  int   linenoiseHistoryAdd(const(char)* line);
  int   linenoiseHistorySetMaxLen(int len);
  int   linenoiseHistorySave(const(char)* filename);
  int   linenoiseHistoryLoad(const(char)* filename);
  void  linenoiseClearScreen();
  void  linenoiseSetMultiLine(int ml);
  void  linenoisePrintKeyCodes();
}

// FFI
extern (C) nothrow @nogc {
  // TODO: relative to lib path (remove "lib/" prefix)
  version (Java) {} // HACK: temp
  else version (OSX) pragma(lib, "lib/darwin/x86_64/libffi.a");
  else version (linux) pragma(lib, "lib/linux/x86_64/libffi.a");
  else version (Windows) pragma(lib, win32LibRoot ~ "lib/windows/x86_64/libffi.lib");
  else static assert(0);

  // FFI Target
  alias uint ffi_abi;
  version (X86_64) {
    alias ulong ffi_arg;
    alias long  ffi_sarg;
    enum FFI_TRAMPOLINE_SIZE = 24;
    enum FFI_NATIVE_RAW_API  = false;
  }
  else version (X86) {
    enum FFI_TRAMPOLINE_SIZE = 12;
    enum FFI_NATIVE_RAW_API  = true;
  }
  else static assert(0, "TODO");

  // TODO: dependent on target ^
  version (Windows) {
    enum {
      FFI_FIRST_ABI = 0,
      FFI_SYSV,
      FFI_STDCALL,
      FFI_DEFAULT_ABI = FFI_SYSV,
    }
  }
  else version (X86_64) {
    enum {
      FFI_FIRST_ABI = 1,
      FFI_UNIX64,
      FFI_WIN64,
      FFI_GNUW64,
      FFI_DEFAULT_ABI = FFI_UNIX64
    }
  }
  else {
    enum FFI_DEFAULT_ABI = FFI_SYSV;
  }

  // FFI
  struct ffi_type {
    size_t size;
    ushort alignment;
    ushort type;
    ffi_type** elements;
  }

  alias ffi_type_uchar = ffi_type_uint8;
  alias ffi_type_schar = ffi_type_sint8;

  alias ffi_type_ushort = ffi_type_uint16;
  alias ffi_type_sshort = ffi_type_sint16;

  alias ffi_type_uint = ffi_type_uint32;
  alias ffi_type_sint = ffi_type_sint32;

  static if (size_t.sizeof == ulong.sizeof) {
    alias ffi_type_ulong = ffi_type_uint64;
    alias ffi_type_slong = ffi_type_sint64;
  }
  else {
    static assert(size_t.sizeof == uint.sizeof);
    alias ffi_type_ulong = ffi_type_uint32;
    alias ffi_type_slong = ffi_type_sint32;
  }

  extern __gshared ffi_type
    ffi_type_void,
    ffi_type_uint8,
    ffi_type_sint8,
    ffi_type_uint16,
    ffi_type_sint16,
    ffi_type_uint32,
    ffi_type_sint32,
    ffi_type_uint64,
    ffi_type_sint64,
    ffi_type_float,
    ffi_type_double,
    ffi_type_longdouble,
    ffi_type_pointer;

  alias uint ffi_status;
  enum : ffi_status {
    FFI_OK,
    FFI_BAD_TYPEDEF,
    FFI_BAD_API
  }

  struct ffi_cif {
    ffi_abi abi;
    uint nargs;
    ffi_type** arg_types;
    ffi_type* rtype;
    uint bytes;
    uint flags;
  }

  enum FFI_SIZEOF_ARG = size_t.sizeof;

  union ffi_raw {
    ffi_sarg sint;
    ffi_arg  uint_;
    float    flt;
    void*    ptr;
    char[FFI_SIZEOF_ARG] data;
  }

  // ffi_raw_call
  // ffi_ptrarray_to_raw
  // ffi_raw_to_ptrarray
  // ffi_raw_size

  struct ffi_closure {
    char[FFI_TRAMPOLINE_SIZE] tramp;
    ffi_cif* cif;
    void function(ffi_cif*, void*, void**, void*) fun;
    void* user_data;
  }

  void* ffi_closure_alloc(size_t size, void** code);
  void ffi_closure_free(void*);

  ffi_status ffi_prep_closure
    (ffi_closure* closure, ffi_cif* cif,
     typeof(ffi_closure.fun) fun, void* user_data);

  ffi_status ffi_prep_closure_loc
    (ffi_closure* closure, ffi_cif* cif,
     typeof(ffi_closure.fun) fun, void* user_data, void* codeloc);

  struct ffi_raw_closure {
    char[FFI_TRAMPOLINE_SIZE] tramp;
    ffi_cif* cif;
    version(NO_FFI_NATIVE_RAW_API) {}
    else {
      void function(ffi_cif*, void*, void**, void*) translate_args;
      void* this_closure;
    }
    void function(ffi_cif*, void*, ffi_raw*, void*) fun;
    void* user_data;
  }

  ffi_status ffi_prep_raw_closure
    (ffi_raw_closure* closure, ffi_cif* cif,
     typeof(ffi_raw_closure.fun) fun, void* user_data);

  ffi_status ffi_prep_cif
    (ffi_cif* cif, ffi_abi abi, uint nargs, ffi_type* rtype, ffi_type** atypes);
  ffi_status ffi_prep_cif_var
    (ffi_cif* cif, ffi_abi abi, uint nfixedargs, uint ntotalargs,
     ffi_type* rtype, ffi_type** atypes);
  void ffi_call(ffi_cif* cif, void* fn, void* rvalue, void** avalue);

  enum {
    FFI_TYPE_VOID,
    FFI_TYPE_INT,
    FFI_TYPE_FLOAT,
    FFI_TYPE_DOUBLE,
    FFI_TYPE_LONGDOUBLE,
    FFI_TYPE_UINT8,
    FFI_TYPE_SINT8,
    FFI_TYPE_UINT16,
    FFI_TYPE_SINT16,
    FFI_TYPE_UINT32,
    FFI_TYPE_SINT32,
    FFI_TYPE_UINT64,
    FFI_TYPE_SINT64,
    FFI_TYPE_STRUCT,
    FFI_TYPE_POINTER,
    FFI_TYPE_COMPLEX
  }
}


// Misc Utilities
// -----------------------------------------------------------------------------

/// Extract a boolean from a JSONValue. Mostly used by the language server.
pragma(inline, true)
bool isTrue0(ref const JSONValue v) { return v.type == JSONType.true_; }
bool isTrue1(ref const JSONValue v, string key = "dynamicRegistration") {
  if (auto p = key in v) return (*p).isTrue0;
  return false;
}
bool isTrue2(ref const JSONValue v, string feat,
             string key = "dynamicRegistration")
{
  if (auto p = feat in v) return (*p).isTrue1(key);
  return false;
}

/// Fast ASCII lowercase transform to use in case-insensitive tests.
enum asciiUpperCaseBit = 1 << 5;
pragma(inline, true) {
  dchar toLower(dchar ch) { return ch |  asciiUpperCaseBit; }
  dchar toUpper(dchar ch) { return ch & ~asciiUpperCaseBit; }
}
static assert('A'.toLower == 'a');

/// Returns the Murmur3 32-bit hash of the given string.
uint murmurHash3(in string s) nothrow @nogc {
  static union Magic {
    ubyte[4] data;
    uint     hash;
  }
  MurmurHash3!32 h;
  h.put(cast(const ubyte[])s);
  return Magic(h.finish()).hash;
}

pragma(inline, true)
static void alignAddr(ref size_t offset, size_t alignment) pure nothrow @nogc @safe {
  offset = (offset + (alignment - 1)) & -alignment;
}

enum tctrl = "\u001b["; // Terminal control code prefix
enum TCol : string { // Terminal color
  Name = "38;5;171m",
  Addr = "38;5;227m",
  Type = "38;5;160m",
  Form = "38;5;92m",
  Num  = "38;5;124m",
  Str  = "38;5;34m",
  Doc  = "38;5;34m",
  Meta = "38;5;102m",
  Gray = "38;5;243m",
  Perf = "38;5;169m",
  None = "0m"
}

pragma(inline, true) {
  void tcol(W)(ref W w, TCol col = TCol.None) if (isOutputRange!(W, char)) {
    w.put(tctrl);
    w.put(cast(string)col);
  }

  void tcol(W)(ref W w, TCol col, string s) if (isOutputRange!(W, char)) {
    w.tcol(col);
    w.put(s);
    w.tcol();
  }

  void tcolOpen(W)(ref W w, TCol col) if (isOutputRange!(W, char)) {
    w.tcol(TCol.Gray);
    w.put("#<");
    w.tcol(col);
  }

  void tcolClose(W)(ref W w) if (isOutputRange!(W, char)) {
    w.tcol(TCol.Gray, ">");
  }

  void tcolGray(W)(ref W w, string s) if (isOutputRange!(W, char)) {
    w.tcol(TCol.Gray, s);
  }

  void tcolOpaque(W)(ref W w, string t, void* p) if (isOutputRange!(W, char)) {
    w.tcolOpen(TCol.Type);
    w.put(t);
    w.tcol(TCol.Addr);
    w.formattedWrite(" 0x%08x", p);
    w.tcolClose();
  }
}

version (Windows) {
  string win32ErrorStr(winerr.HRESULT hr) {
    char* msg;
    auto size = win32.FormatMessageA
		  (win32.FORMAT_MESSAGE_ALLOCATE_BUFFER |
			 win32.FORMAT_MESSAGE_FROM_SYSTEM |
			 win32.FORMAT_MESSAGE_IGNORE_INSERTS,
       null, hr, winnt.MAKELANGID(winnt.LANG_NEUTRAL, winnt.SUBLANG_DEFAULT),
       cast(char*)&msg, 0, null);
    auto s = msg[0..size].idup;
    win32.LocalFree(msg);
    return s;
  }
  string win32Error() {
    auto e = win32.GetLastError();
    return e == 0 ? "" : win32ErrorStr(cast(winerr.HRESULT)e);
  }
  string comError(winerr.HRESULT hr) {
    if (winerr.HRESULT_FACILITY(hr) == winerr.FACILITY_WINDOWS)
      hr = winerr.HRESULT_CODE(hr);
    return win32ErrorStr(hr);
  }
}

// Base Definitions
// -----------------------------------------------------------------------------

// Unit value
enum nil = Any(cast(size_t)0, Any.Tag.Nil);
static assert(nil.isNil);
static assert(nil.raw == nil.raw);

// Boolean values
enum boolTrue  = Any(cast(size_t)true,  Any.Tag.Bool);
enum boolFalse = Any(cast(size_t)false, Any.Tag.Bool);
static assert(boolTrue .isBool);
static assert(boolFalse.isBool);
static assert(boolTrue.raw == boolTrue.raw);

class InternalMarker {}

/// Dynamically typed immediate value. Implemented as a tagged pointer.
struct Any {
  private union {
    size_t raw; /// Pointer-sized tagged value. The low 4 bits contain the tag.
    void* _ptr; /// Unused but required to prevent the GC of referenced values.
  }

  enum Tag : ubyte {
    Nil,   /// NULL pointer, unit value; *MUST* be first
    Bool,  /// True or false
    SInt,  /// Signed 60-bit integer
    UInt,  /// Unsigned 60-bit integer
    Float, /// Floating-point number, 32-bit
    Char,  /// Unicode character, 32-bit
    Key,   /// Keyword pointer (Val shortcut)
    Sym,   /// Symbol pointer (Val shortcut)
    Val,   /// Typed heap value (requires 16-byte alignment)
    Ptr,   /// Untyped raw pointer (doesn't require 16-byte alignment)
    Obj,   /// D object instance (requires 16-byte alignment)
    ObjC,  /// NSObject instance (doesn't require 16-byte alignment)
    // Meta,  /// TODO need traits first (meta lifts traits of boxed type, Functor!)
    // TODO can fit 3 more, profile!
  }

  enum tagBits = 4; /// Assume 16-byte heap alignment. Tag.Ptr can hold any ptr!
  enum tagMask = (1 << tagBits) - 1;   /// Bits making up the tag part of `raw`.
  enum tagHigh = size_t.max - tagMask; /// Maximum tagged unsigned value.

  static assert(Tag.max <= tagMask);
  static assert(Any.sizeof == size_t.sizeof);

  // Constructors
  nothrow {
    this(void* p, Tag t) {
      assert(p);
      auto v = cast(size_t)p;
      assert((v & tagMask) == 0);
      raw = v | t;
    }
    this(size_t v, Tag t) {
      if (v <= tagHigh) raw = (v << tagBits) | t;
      else {
        Type* type;
        switch (t) with (Tag) {
        default: assert(0);
        case Ptr: type = typeVoidPtr; break;
        }
        raw = cast(size_t)&new Prim(type, v).val | Tag.Val;
      }
    }
    this(ptrdiff_t v, Tag t) { // FIXME negative numbers
      raw = (v << tagBits) | t;
    }
    this(Val*   v) { this(v, Tag.Val); }
    this(Key*   k) { this(k, Tag.Key); }
    this(Sym*   s) { this(s, Tag.Sym); }
    // this(Meta*  m) { this(m, Tag.Meta); }
    this(long   l) { this(l, Tag.SInt); }
    this(ulong  l) { this(l, Tag.UInt); }
    this(bool   b) { this(cast(size_t)b, Tag.Bool); }
    this(dchar  c) { this(cast(size_t)c, Tag.Char); }
    this(float  f) { this(*cast(size_t*)&f, Tag.Float); }
    this(Object o) { this(cast(void*)o,  Tag.Obj); }
    this(void*  p) { this(cast(size_t)p, Tag.Ptr); }

    version (OSX)
    this(id o) { this(cast(ulong)o, Tag.ObjC); }
  }

  // Accessors
  const nothrow @nogc {
    Tag tag   () { return cast(Tag)(raw &  tagMask); }
    T*  get(T)() { return cast(T*) (raw & ~tagMask); }

    bool isNil       () { return raw == 0; }
    bool isTag(Tag t)() { return tag == t; }

    alias isPtr   = isTag!(Tag.Ptr);
    alias isVal   = isTag!(Tag.Val);
    alias isKey   = isTag!(Tag.Key);
    alias isSym   = isTag!(Tag.Sym);
    // alias isMeta  = isTag!(Tag.Meta);
    alias isBool  = isTag!(Tag.Bool);
    alias isChar  = isTag!(Tag.Char);
    alias isSInt  = isTag!(Tag.SInt);
    alias isUInt  = isTag!(Tag.UInt);
    alias isFloat = isTag!(Tag.Float);

    T* safeGet(alias type, T)() {
      if (!isVal) return null;
      auto v = val;
      return (v.type !is type) ? null : cast(T*)v;
    }

    alias safeStr = safeGet!(typeStr, Str);
    alias safeVec = safeGet!(typeVec, Vec);
    alias safeMap = safeGet!(typeMap, Map);
    // alias safeSet = safeGet!(typeSet, Set);

    dchar character() { return cast(dchar)uinteger; }
    bool  boolean  () { return cast(bool)uinteger; }
    long  sinteger () { return cast(ptrdiff_t)raw >> tagBits; }
    ulong uinteger () { return raw >>> tagBits; }
    float floating () { auto n = sinteger; return *(cast(float*)&n); }

    bool isFalse() { return raw == boolFalse.raw; }
    bool isTrue () { return raw == boolTrue .raw; }

    Object obj() { return cast(Object)get!void; }
    void*  ptr() { return cast(void*)uinteger; }

    alias key  = get!Key;
    alias sym  = get!Sym;
    alias val  = get!Val;
    // alias meta = get!Meta;

    bool toBool() { return !(isNil || isFalse); }
  }

  // Show
  string toString() const {
    Appender!string a;
    toString(a);
    return a.data;
  }
  void toString(W)(ref W w) const if (isOutputRange!(W, char)) {
    final switch (tag) with (Tag) {
    case Nil:   w.tcol(TCol.Form, "nil");                      break;
    case Bool:  w.tcol(TCol.Form, boolean ? "true" : "false"); break;
    case SInt:  w.tcol(TCol.Num,  sinteger.to!string);         break;
    case UInt:  w.tcol(TCol.Num,  uinteger.to!string);         break;
    case Float: w.tcol(TCol.Num,  floating.to!string);         break;
    case Sym:   w.tcol(TCol.Name, sym.toString);               break;
    case Key:   w.tcol(TCol.Form, key.toString);               break;
    case Val:   val.toString(w);                               break;
    // case Meta: assert(0); // TODO
    case Char:
      w.tcol(TCol.Str);
      w.formattedWrite("\\U%08x", character);
      w.tcol();
      break;
    case Ptr:
      w.tcolOpen(TCol.Addr);
      w.formattedWrite("0x%08x", ptr);
      w.tcolClose();
      break;
    case Obj:
      w.tcolOpen(TCol.Type);
      w.put(obj.classinfo.name);
      w.put(' ');
      w.tcol();
      w.put(obj.toString);
      w.tcolClose();
      break;
    case ObjC:
      version (OSX) {
        auto o = cast(id)ptr;
        assert(o);
        version(none)
        version (D_ObjectiveC) if (dynamicPrettyPrint) {
          auto obj   = cast(NSObject)o;
          auto nsStr = obj.debugDescription();
          auto utf   = nsStr.UTF8String;
          auto s     = utf[0..utf.strlen];
          if (s.length && s[0] == '<') {
            auto idx = s.indexOf('\n');
            if (idx == -1) s = "";
            else s = s[idx + 1..$];
          }
          if (s.length) {
            w.tcol(TCol.Doc);
            w.put(s);
            w.put('\n');
          }
          nsStr.release();
        }
        if (object_isClass(o)) {
          w.tcol(TCol.Type);
          w.put(class_getName(cast(Class)o).to!string);
          w.tcol();
        }
        else {
          auto cls = class_getName(cast(Class)object_getClass(o));
          w.tcolOpaque(cls.to!string, o);
        }
        break;
      }
      else assert(0);
    }
  }

  // Operations
  hash_t toHash() const nothrow {
    final switch (tag) with(Tag) {
    case Nil:
    case Bool:
    case Char:
    case Ptr:
    case SInt:
    case UInt:
    case Float: return raw; // TODO: weak!
    case Sym:
    case Key:
    case Val:
      /*case Meta:*/ return val.toHash;
    case Obj:  return obj.toHash;
    case ObjC:
      version (D_ObjectiveC) return (cast(NSObject)ptr).hash;
      else version (OSX) static assert(0, "TODO");
      else assert(0);
    }
  }

  int opCmp(Any rhs) const {
    if (rhs.raw == raw)   return 0;
    if (rhs.type != type) return type < rhs.type ? -1 : 1;

    final switch (tag) with (Tag) {
    case Nil: assert(0);
    case Ptr:
    case Bool:
    case Char:
    case SInt:
    case UInt:
    case Float: return raw < rhs.raw ? -1 : 1;
    case Sym:
    case Key:
    case Val:
    /*case Meta:*/ return val.opCmp(*rhs.val);
    case Obj:  return obj.opCmp(rhs.obj);
    case ObjC: assert(0);
    }
  }

  // Information
  Type* type() const {
    final switch (tag) with (Tag) {
    case Nil:   return typeNil;
    case Ptr:   return typeVoidPtr;
    case Bool:  return typeBool;
    case Char:  return typeChar;
    case SInt:  return typeSInt64;
    case UInt:  return typeUInt64;
    case Float: return typeFloat32;
    case Sym:   return typeSym;
    case Key:   return typeKey;
    case Val:   return val.type;
    case Obj:   return typeObj;
    // case Meta:  return typeMeta;
    case ObjC:
      version (OSX) {
        auto o = object_getClass(cast(id)ptr);
        auto p = o in FFI.Analyzer.classes;
        enforce(p, "Vile class not found: " ~ class_getName(o).to!string);
        return &(*p).type;
      }
      else return typeVoid;
    }
  }

  size_t toUInt() {
    if (isUInt) return uinteger;
    enforce(isVal, "array expects unsigned count");
    auto val = cast(Prim*)val;
    auto nat = val.type.aliased.native;
    enforce(nat >= Type.Native.UInt8 && nat <= Type.Native.UInt64);
    return val.ulongVal;
  }

  Array* toArray() {
    enforce(isVal);
    auto a = cast(Array*)val;
    enforce(a.type.native == Type.Native.DynArray);
    return a;
  }

  Type* toType() {
    enforce(isVal);
    auto t = cast(Type*)val;
    enforce(t.type is typeType);
    return t;
  }

  static assert(Any(true).isTrue);
  static assert(Any(false).isFalse);
  static assert(Any(0) .uinteger == 0);
  static assert(Any(42).sinteger == 42);

  unittest {
    assert(nil < boolFalse);
    assert(boolFalse < boolTrue);
    // assert(Any(3.14).floating == 3.14);
  }
}

// TODO: actually decouple reader/printer from values. print C, print Gfx, read I
__gshared string function(ref Val)[Type*] showTable;

/// Dynamic heap value.
struct Val {
  Type* type; /// The type of this value. Does not change.
  // Data follows, use protocols on type to interact with it.

  @disable this();

  this(Type* ty) nothrow @nogc {
    assert(ty || !typeType); // Only null when bootstrapping the Type type.
    assert(!ty || !ty.ti || ty.ti.xtoHash, ty.ti.name);
    type = ty;
  }

  int opCmp(ref Val rhs) const {
    if (rhs.type != type) return type.opCmp(*rhs.type);
    if (type.ti) return type.ti.compare(&this, &rhs);
    // TODO: cmp trait
    return &this < &rhs; // TODO: weak
  }

  string toString() {
    Appender!string w;
    toString(w);
    return w.data;
  }
  void toString(W)(ref W w) if (isOutputRange!(W, char)) {
    if (type.ti && type.ti.xtoString) w.put(type.ti.xtoString(&this));
    else if (auto f = type in showTable) w.put((*f)(this));
    else {
      switch (type.aliased.native) with (Type.Native) {
      case Bool:..
      case Char:
      case BigInt:
      case Pointer: (cast(.Prim*) &this).toString(w); break;
      case Meta:    (cast(.Meta*) &this).toString(w); break;
      case Fn:      (cast(.Fn*)   &this).toString(w); break;
      case Array:   (cast(.Array*)&this).toString(w); break;
      default: w.tcolOpaque(type.sym.toString, cast(void*)&this + 16); break;
      }
    }
  }

  hash_t toHash() const nothrow {
    if (type.ti) return type.ti.getHash(&this);
    return cast(hash_t)&this; // TODO: weak
  }
}

struct Prim {
  alias val this;
  Val val;
  union {
    void*  ptr;
    long   slongVal;
    ulong  ulongVal;
    double doubleVal;
    BigInt bigIntVal;
  }

  @disable this();

  nothrow {
    private this(Type* type) { val = Val(type); }
    this(Type* type, void* ptr) {
      this(type);
      this.ptr = ptr;
    }
    this(Type* type, long x) {
      this(type);
      this.ulongVal = x;
    }
    this(double x) {
      this(typeFloat64);
      doubleVal = x;
    }
    this(BigInt big) {
      this(typeBigInt);
      bigIntVal = big;
    }

    hash_t toHash() { return ulongVal; }
  }

  string toString() {
    Appender!string w;
    toString(w);
    return w.data;
  }
  void toString(W)(ref W w) if (isOutputRange!(W, char)) {
    switch (type.aliased.native) with (Type.Native) {
    case SInt8:  ..case SInt64:  w.put(slongVal .to!string); break;
    case UInt8:  ..case UInt64:  w.put(ulongVal .to!string); break;
    case Float16:..case Float80: w.put(doubleVal.to!string); break;
    case Bool:    w.put(ulongVal ? "true" : "false"); break;
    case Char:    w.put(cast(dchar)ulongVal);         break;
    case BigInt:  w.put(bigIntVal.to!string);         break;
    case Pointer: w.tcolOpaque(type.sym.name, ptr);   break;
    default: assert(0);
    }
  }
}

/// UTF-8 character string.
struct Str {
  alias val this;
  Val    val;
  char[] data;

  this(char[] data) {
    this.val  = Val(typeStr);
    this.data = data;
  }

  string toString() {
    Appender!string w;
    toString(w);
    return w.data;
  }
  void toString(W)(ref W w) if (isOutputRange!(W, char)) {
    w.tcol(TCol.Str);
    w.put('"');
    w.put(data); // TODO: escapes
    w.put('"');
    w.tcol();
  }

  hash_t toHash() const nothrow { return data.assumeUnique.murmurHash3; }
}

// Data Definitions
// -----------------------------------------------------------------------------

/// Code shared between symbols and keywords.
mixin template Interning(alias ty, dchar skipChar = dchar.init) {
  enum isSym = skipChar == dchar.init;

  enum sepUnknown = -2;
  alias Duplicate = Flag!"Dup";

  alias T = typeof(this);

  static T* intern(string s) { return intern(s, sepUnknown); }
  static T* intern(in char[] s) {
    return intern(s.assumeUnique, sepUnknown, Duplicate.yes);
  }
  static T* intern(in char[] ns, in char[] name) {
    return intern(ns.assumeUnique, name.assumeUnique);
  }
  static T* intern(string ns, string name) {
    auto s = new char[ns.length + name.length + 1];
    s[0..ns.length]   = ns;
    s[ns.length]      = '/';
    s[ns.length+1..$] = name;
    return intern(s.assumeUnique, ns.length);
  }
  static T* intern(string s, ptrdiff_t sep, Duplicate dup = Duplicate.no) {
    if (auto psym = s in interns) return *psym;
    assert(s.lastIndexOf(' ') == -1, text('"', s, '"')); // TODO: better whitespace check
    auto sym = new T(dup ? s.idup : s, sep == -2 ? s.lastIndexOf('/') : sep);
    interns[s] = sym;
    return sym;
  }

  private __gshared T*[char[]] interns;

  alias val this;
  Val    val;
  string ns;
  string name;
  string str;
  hash_t hash;

  private enum skipChars = skipChar == dchar.init ? 0 : 1;

  // nothrow @nogc:

  private this(string str, ptrdiff_t sep) {
    static if (skipChars) assert(str[0] == skipChar);

    this.val = Val(ty);
    this.str = str;

    if (sep < 1) {
      name = str[skipChars..$];
    }
    else {
      ns   = str[skipChars..sep];
      name = str[sep+1..$];
    }

    hash = murmurHash3(str);
  }

  @disable this();

  hash_t toHash()   const { return hash; }
  string toString() const { return str; }
}

/// Symbol.
struct Sym {
  mixin Interning!(typeSym);

  static __gshared uint nextGenId = 0;

  static Sym* gen(string prefix) {
    return Sym.intern(text(prefix, '#', ++nextGenId));
  }
}

/// Keyword.
struct Key { mixin Interning!(typeKey, ':'); }

/// Meta-data wrapper.
struct Meta {
  alias val this;
  Val  val;
  Any  data;
  Map* meta;

  @disable this();

  this(Any data, Map* meta = null) {
    this.val = Val(typeMeta);
    this.meta = meta;
    this.data = data;
  }

  string toString() {
    Appender!string w;
    toString(w);
    return w.data;
  }
  void toString(W)(ref W w) if (isOutputRange!(W, char)) {
    data.toString(w);
  }
}

struct Array {
  alias val this;
  Val    val;
  size_t length;
  void*  ptr;

  @disable this();

  this(Type* type, size_t length, void* ptr) {
    assert(type && type.native == Type.Native.DynArray);
    this.val    = Val(type);
    this.length = length;
    this.ptr    = ptr;
  }

  string toString() {
    Appender!string w;
    toString(w);
    return w.data;
  }
  void toString(W)(ref W w) if (isOutputRange!(W, char)) {
    // TODO
    w.put("#<");
    w.put(type.next.sym.name);
    w.put('[');
    w.put(length.to!string);
    w.put("]>");
  }
}

/// Singly-linked lists.
struct List {
  alias val this;
  Val   val;
  List* next;
  Any   data;

  __gshared List* emptyValue;

  @disable this();

  this(Any data, List* next = null) {
    this.val  = Val(typeList);
    this.next = next;
    this.data = data;
  }

  this(Any[] data) {
    this.val = Val(typeList);

    switch (data.length) {
    case 0: break;
    default:
      this.next = new List(data[1..$]);
      goto case;
    case 1:
      this.data = data[0];
    }
  }

  size_t length() {
    if (&this is emptyValue) return 0;
    auto len  = 1;
    auto head = this.next;
    while (head) {
      len++;
      head = head.next;
    }
    return len;
  }

  string toString() {
    Appender!string a;
    toString(a);
    return a.data;
  }
  void toString(W)(ref W w) if (isOutputRange!(W, char)) {
    auto save = dynamicPrettyPrint;
    dynamicPrettyPrint = false; scope (exit) dynamicPrettyPrint = save;
    w.tcolGray("(");
    if (&this !is emptyValue) {
      data.toString(w);
      auto head = next;
      while (head) {
        w.put(' ');
        head.data.toString(w);
        head = head.next;
      }
    }
    w.tcolGray(")");
  }

  hash_t toHash() const nothrow { return cast(hash_t)&this; }
}

///
struct Vec {
  alias val this;
  Val   val;
  Any[] data;

  this(Any[] values) {
    this.val  = Val(typeVec);
    this.data = values;
  }

  string toString() {
    Appender!string a;
    toString(a);
    return a.data;
  }
  void toString(W)(ref W w) if (isOutputRange!(W, char)) {
    auto save = dynamicPrettyPrint;
    dynamicPrettyPrint = false; scope (exit) dynamicPrettyPrint = save;
    w.tcolGray("[");
    if (!data.empty) {
      data[0].toString(w);
      foreach (v; data[1..$]) {
        w.put(' ');
        v.toString(w);
      }
    }
    w.tcolGray("]");
  }

  hash_t toHash() const nothrow { return cast(hash_t)&this; }
}

/// A dictionary of key-values.
struct Map {
  alias Data = typeof(data);
  alias val this;
  Val val;
  Any[Any] data;

  this(Data data) {
    val = Val(typeMap);
    this.data = data;
  }
  this(Any[] keyVals) {
    val = Val(typeMap);
    enforce(keyVals.length % 2 == 0, "Invalid map initializer length");
    for (int i; i < keyVals.length; i += 2) data[keyVals[i]] = keyVals[i + 1];
  }

  string toString() const {
    Appender!string a;
    toString(a);
    return a.data;
  }
  void toString(W)(ref W w) const if (isOutputRange!(W, char)) {
    auto save = dynamicPrettyPrint;
    dynamicPrettyPrint = false; scope (exit) dynamicPrettyPrint = save;
    w.tcolGray("{");
    bool first = true;
    foreach (k, v; data) {
      if (first) first = false;
      else w.put(", ");
      w.formattedWrite("%s %s", k, v);
    }
    w.tcolGray("}");
  }

  hash_t toHash() const nothrow { return cast(hash_t)&this; }

  Any get(Key* k, Any defaultValue = nil) {
    assert(k);
    if (auto p = Any(k) in data) return *p;
    return defaultValue;
  }
}

version(BUG) // https://issues.dlang.org/show_bug.cgi?id=19877
struct Set {
  alias val this;
  Val              val;
  RedBlackTree!Any data;

  alias Data = typeof(data);

  this(Data data) {
    assert(data);
    this.val  = Val(typeSet);
    this.data = data;
  }
  this(Any[] values) {
    this.val  = Val(typeSet);
    this.data = new Data(values);
  }

  string toString() const {
    Appender!string a;
    toString(a);
    return a.data;
  }
  void toString(W)(ref W w) const if (isOutputRange!(W, char)) {
    auto save = dynamicPrettyPrint;
    dynamicPrettyPrint = false; scope (exit) dynamicPrettyPrint = save;
    w.tcolGray("#{");
    bool first = true;
    foreach (Any v; data) {
      if (first) first = false;
      else w.put(' ');
      v.toString(w);
    }
    w.tcolGray("}");
  }

  hash_t toHash() const nothrow { return cast(hash_t)&this; }
}

// Code Definitions
// -----------------------------------------------------------------------------

bool  dynamicInsideFn    = false;
bool  dynamicPrettyPrint = true; /// Thread-local, whether to PP definitions.
ubyte dynamicIndentLevel = 0;    /// Thread-local, pretty-print indent level.

/// Definition available to the compiler.
struct Def {
  alias val this;
  Val    val;
  Sym*   sym;    /// Identifier to resolve this definition.
  string file;   /// File containing the source code.
  uint   line;   /// Starts at 1, line 0 means no information.
  uint   column; /// Only used if line is also used.
  mixin(bitfields!(bool, "isPrivate",  1,
                   uint, "__padding", 63));

  @disable this();

  this(Type* ty) {
    assert(!typeType || !typeSym);
    this.val = Val(ty);
  }
  this(Type* ty, Sym* sym) {
    assert(sym);
    this.val = Val(ty);
    this.sym = sym;
  }

  string toString() const {
    Appender!string s;
    toString(s);
    return s.data;
  }
  void toString(W)(ref W w) const if (isOutputRange!(W, char)) {
    writeMeta(w);
    w.tcolOpen(TCol.Type);
    w.put(type.sym.toString);
    w.put(' ');
    w.tcol(TCol.Name);
    w.put(sym.toString);
    w.tcolClose();
  }

  void writeMeta(W)(ref W w) const {
    if (!line) return;
    w.tcol(TCol.Meta);
    w.formattedWrite("%s:%s:%s\n", file, line, column);
    w.tcol();
  }
}

/// Dynamic type information.
struct Type {
  alias TypeInfo_Struct TIS;
  alias def this;
  Def   def;
  Type* next; /// For composed types such as 'Ptr Void'
  union {
    version (OSX)
    Class objc; /// Native.NSObject: Objective-C class reference
    TIS   ti;   /// Native.Struct:   Optional D struct runtime information
  }
  Def*[] params; /// Template parameters
  enum alignBits = 10;
  enum invalidAlign = (1 << alignBits) - 1;
  mixin(bitfields!(Native, "native",       5,
                   uint,   "size",        24,
                   uint,   "alignment", alignBits,
                   bool,   "isCategory",   1,
                   bool,   "isProtocol",   1,
                   bool,   "isTemplate",   1, // Template specialization
                   ushort, "arrayLength", 16,
                   uint,   "__padding",    6));

  bool signed  () { with (Native) return native >= SInt8 && native <= SInt64; }
  bool unsigned() { with (Native) return native >= UInt8 && native <= UInt64; }
  bool floating() { with (Native) return native >= Float16 && native <= Float80; }

  /// Enumeration of Vile base types used by the emitter and interpreter.
  enum Native : ubyte {
    Type,
    Void,    /// Bottom type: undefined
    Nil,     /// Unit type:   C NULL, C++ nullptr, Objective-C nil
    Any,     /// Top type:    Vile tagged pointer
    Meta,
    Bool,    /// 1 byte, true/false
    SInt8,   /// 1 byte, signed
    SInt16,  /// 2 byte, signed
    SInt32,  /// 4 byte, signed
    SInt64,  /// 8 byte, signed
    UInt8,   /// 1 byte, unsigned
    UInt16,  /// 2 byte, unsigned
    UInt32,  /// 4 byte, unsigned
    UInt64,  /// 8 byte, unsigned
    Float16, /// 2 byte, floating
    Float32, /// 4 byte, floating
    Float64, /// 8 byte, floating
    Float80, /// 10 byte, floating
    Char,    /// 4 byte, Unicode scalar value
    Enum,    /// Value, enumerated
    BigInt,  /// Reference, BigInt instance
    Pointer, /// Reference, Memory address
    Array,   /// Value,
    DynArray,/// Reference, Memory address, number of elements
    Struct,  /// Aggregate, Fields
    Trait,   /// Aggregate, Methods
    Alias,   /// Reference, type
    Fn,      /// Reference, return type, parameter types
    DObject, /// Reference, D object instance
    NSObject,/// Reference, Objective C object instance
    Generic, /// Generic type
    Temp     /// Temporary type used to resolve cycles, see Placeholder
  }

  static void nativeInfo(Native n, out size_t size, out size_t alignment) {
    final switch (n) with (Native) {
    case Meta:
    case Temp:
    case Generic:
    case Array:  // TODO
    case DynArray:
    case Struct:
    case Trait:
    case Enum:
    case Alias:
    case Void:
    case Type: size = 0, alignment = 0; break;
    case Nil:
    case Any:
    case BigInt:
    case Pointer:
    case Fn:
    case DObject:
    case NSObject: size = size_t.sizeof, alignment = size_t.sizeof; break;
    case Bool:
    case SInt8:
    case UInt8: size = 1, alignment = 1; break;
    case SInt16:
    case UInt16:
    case Float16: size = 2, alignment = 2; break;
    case SInt32:
    case UInt32:
    case Float32:
    case Char: size = 4, alignment = 4; break;
    case SInt64:
    case UInt64:
    case Float64: size = 8, alignment = 8; break;
    case Float80: size = 10, alignment = 10; break;
    }
  }

  @disable this();

  private this(Type* ty, Sym* sym, size_t size, size_t alignment) {
    def = Def(ty, sym);
    this.size = size.to!ushort;
    this.alignment = alignment.to!ushort;
  }

  this(Native name) {
    this(name, Sym.intern(name.to!string));
    this.native = name;
  }
  this(Native name, Sym* sym) {
    size_t size, alignment;
    nativeInfo(name, size, alignment);
    this(typeType, sym, size, alignment);
    this.native = name;
  }
  this(TypeInfo_Struct ti, Type* ty) {
    assert(ti);
    def = Def(ty);
    this.ti = ti;
    this.native = Native.Type;
  }
  this(TypeInfo_Struct ti, string name) {
    assert(ti);
    this(typeType, Sym.intern(name), ti.tsize, size_t.sizeof);
    this.ti = ti;
    this.native = Native.Type;
  }
  // this(Fn* fn) {
  //   assert(fn);
  //   this(typeType, Sym.intern("Fn"), 0, 0);
  //   this.fn = fn;
  //   this.native = Native.Fn;
  // }

  string toString() {
    if (!dynamicPrettyPrint) return sym.toString;
    dynamicPrettyPrint = false; scope (exit) dynamicPrettyPrint = true;
    Appender!string a;
    writeMeta(a);
    toString(a);
    return a.data;
  }
  void toString(W)(ref W w) if (isOutputRange!(W, char)) {
    switch (native) with (Native) {
    case NSObject:
    case Struct: (cast(.Struct*)&this).toString(w); break;
    case Enum:   (cast(.Enum*)  &this).toString(w); break;
    case Fn:     (cast(.FnType*)&this).toString(w); break;
    case Alias:
      w.tcol(TCol.Type);
      w.put(sym.toString);
      if (dynamicIndentLevel == 0) {
        w.tcolGray(" => ");
        next.toString(w);
      }
      break;
    default:
      assert(sym);
      w.put(sym.toString);
    }
  }

  int opCmp(ref const Type rhs) const {
    return def.sym.toString < rhs.def.sym.toString; // TODO: better
  }

  hash_t toHash() const nothrow { return cast(hash_t)&this; }

  static Type* indirect(string prefix, Type.Native native)(Type* next) {
    static __gshared Type*[Type*] pointers; // memoize
    assert(next);
    if (auto p = next in pointers) return *p;
    auto ptr   = new Type(native, Sym.intern(prefix ~ next.sym.name));
    ptr.next   = next;
    pointers[next] = ptr;
    return ptr;
  }

  alias pointer   = indirect!("@",  Native.Pointer);
  alias reference = indirect!("&",  Native.Pointer);
  alias array     = indirect!("[]", Native.DynArray);

  Struct.Member* lookup(Sym* sym) {
    assert(sym);
    assert(native == Native.Struct, native.to!string);
    return (cast(Struct*)&this).lookup(sym);
  }

  Type* lookupType(Sym* sym) {
    assert(sym);
    switch (native) {
    default: assert(0, "TODO");
    case Native.Struct:
      auto m = (cast(Struct*)&this).lookup(sym);
      if (!m) return null;
      enforce(m.kind == Struct.Member.Kind.Type,
              text("Not a type: ", def.sym.name, ".", sym.name));
      return m.type;
    }
  }

  Type* aliased() {
    return native == Native.Alias ? next.aliased : &this;
  }

  bool isAssignableFrom(Type* other) {
    auto a = other.aliased;
    if (typeVoid is &this || a is &this) return true;
    if (native != Native.Struct || a.native != Native.Struct) return false; // TODO coercions
    auto s = cast(Struct*)a;
    while (true) {
      if (s.protos.length == 0) return false;
      s = cast(Struct*)s.protos[0].aliased; // TODO multiple inheritance, output vtable offset
      if (s is cast(Struct*)&this) return true;
    }
  }
}

/// Enumerated type definition.
struct Enum {
  alias type this;
  Type    type;
  Value[] values;

  static struct Value {
    Sym* sym;
    Any  data;
  }

  @disable this();

  this(Sym* sym) { type = Type(Type.Native.Enum, sym); }

  string toString() {
    Appender!string a;
    toString(a);
    return a.data;
  }
  void toString(W)(ref W w) if (isOutputRange!(W, char)) {
    assert(next);
    if (dynamicIndentLevel > 0) {
      w.put(sym.name);
      return;
    }
    w.tcol(TCol.Name, sym.name);
    w.tcolGray(" :: ");
    next.toString(w);
    foreach (ref v; values) {
      w.formattedWrite("\n  %s", v.sym.name);
      w.tcolGray(" = ");
      v.data.toString(w);
    }
  }

  Any lookup(Sym* s) {
    assert(s);
    foreach (ref v; values) {
      if (v.sym is s) return v.data;
    }
    enforce(0, "Enum member not found: "~sym.name~"."~s.name);
    assert(0);
  }
}

/// Composite type definition.
struct Struct {
  alias type this;
  Type     type;   // Type definition header
  Member[] fields; // Variable and method members
  Type* [] protos; // Objective-C protocols or C++ parent classes
  version (OSX)
  Type* [] exts;   // Objective-C categories for extension
  // TODO objc type params
  ffi_type* ffi;
  ushort numVirtuals; // Including superclasses
  mixin(bitfields!(bool, "isUnion",     1,
                   bool, "objcGeneric", 1,
                   bool, "win32Com",    1,
                   byte, "__padding",   5));

  @disable this();

  this(Sym* sym, Type.Native n = Type.Native.Struct) {
    this.type = Type(n, sym);
  }

  static struct Member {
    Sym* sym;     /// Field identifier, used in lookups.
    union {
      Type* type; /// Kind.Data: Type of data at `object_ptr + offset`.
      Prop* prop; /// Kind.Prop: Property getter and setter, both optional.
      Fn*   fn;   /// Kind.Fn:   Method implementation.
    }
    ushort offset; /// Var byte offset, method vtable index otherwise ushort.max
    mixin(bitfields!(Kind,  "kind",      2,
                     bool,  "isStatic",  1,
                     bool,  "readonly",  1,
                     ubyte, "bitOffset", 6,
                     ubyte, "bitSize",   6));

    enum Kind {
      Type, // Type definition.
      Data, // Direct value of `type` at `offset` from the start of objects.
      Prop, // Indirect value, access through `prop.get` and `prop.set`.
      Func  // Method function, first parameter depend on `isStatic`.
    }

    @disable this();

    this(string name, Type* type, bool isAlias, size_t offset) {
      assert(type);
      this.sym  = Sym.intern(name);
      this.type = type;
      this.kind = isAlias ? Kind.Type : Kind.Data;
      this.offset = offset.to!ushort;
    }
    this(Prop* prop) {
      assert(prop);
      this.sym  = prop.sym;
      this.prop = prop;
      this.kind = Kind.Prop;
    }
    this(Fn* fn) {
      assert(fn);
      this.sym  = fn.sym;
      this.fn   = fn;
      this.kind = Kind.Func;
    }
  }

  string toString() {
    Appender!string s;
    toString(s);
    return s.data;
  }
  void toString(W)(ref W w) if (isOutputRange!(W, char)) {
    if (dynamicIndentLevel > 0) {
      w.put(sym.name);
      return;
    }
    w.tcol(TCol.Type, sym.name);
    w.tcolGray(format(" size=%x align=%x", size, alignment));
    // if (next) {
    //   w.put(" ⋲ ");
    //   w.tcol(TCol.Type, next.sym.name);
    // }
    dynamicIndentLevel++;
    writeFields(w);
    version (OSX)
    foreach (e; exts) {
      writeType(w, e);
      (cast(Struct*)e).writeFields(w);
    }
    dynamicIndentLevel--;
    foreach (p; protos) writeType(w, p);
  }
  void writeType(W)(ref W w, Type* t) if (isOutputRange!(W, char)) {
    w.put("\n  ");
    // w.tcol(TCol.Type, t.sym.name);
    t.toString(w);
  }
  void writeFields(W)(ref W w) if (isOutputRange!(W, char)) {
    foreach (ref f; fields) {
      w.put("\n  ");
      switch (f.kind) with (Member.Kind) {
      case Func:
        if (f.fn.isMember) w.tcolGray(format("[%s] ", f.offset));
        break;
      case Data:
        w.tcolGray(format("%04x ", f.offset));
        break;
      default:
      }
      w.tcol(TCol.Name, f.sym.name);
      if (f.type == null) continue;
      w.tcolGray(" ∷ ");
      final switch (f.kind) with (Member.Kind) {
      case Type:
      case Data: f.type.toString(w); break;
      case Prop: f.prop.toString(w); break;
      case Func: f.fn.toString(w); break;
      }
    }
  }

  Member* lookup(Sym* sym, bool isCat = false) {
    foreach (ref f; fields) {
      // writeln("MEMBER LOOKUP ", f.sym.name);
      if (f.sym is sym) return &f;
    }
    foreach (proto; protos) {
      if (auto p = (cast(Struct*)proto).lookup(sym)) return p;
    }
    // if (next && !isCat) {
    //   if (auto p = (cast(Struct*)next).lookup(sym)) return p;
    // }
    version (OSX) {
      foreach (c; exts) {
        if (auto p = (cast(Struct*)c).lookup(sym, true)) return p;
      }
    }
    return null;
  }

  size_t numDataFields() {
    auto n = 0;
    auto i = 0;
    while (nextDataField(i)) n++;
    return n;
  }

  Member* nextDataField(ref int i) {
    if (i >= fields.length) return null;
    while (fields[i].kind != Member.Kind.Data) i++;
    if (i >= fields.length) return null;
    return &fields[i++];
  }
}

struct FnType {
  alias type this;
  Type type;
  Type* ret;
  Type*[] args;
  FFI.Fn* ffi;
  ubyte argsSize;
  ABI abi;
  mixin(bitfields!(ubyte, "totalArgs",    5,
                   bool,  "structRetFix", 1,
                   byte,  "__padding",    2));

  enum ABI : ubyte {
    Vile,
    D,
    C,
    StdCall,
  }

  @disable this();

  this(Type* ret, Type*[] args) {
    assert(ret);
    this.type = Type(Type.Native.Fn, Sym.intern("Fn"));
    this.ret  = ret;
    this.args = args;
    this.abi  = abi;
  }

  static union Opts {
    mixin(bitfields!(ABI,  "abi",          2,
                     bool, "objc",         1,
                     bool, "self",         1,
                     bool, "structRetFix", 1,
                     byte, "__padding",    3));
    ubyte value;
  }

  static __gshared FnType*[hash_t] cache;

  static FnType* intern(Type* ret, Type*[] args, Opts opts) {
    // auto hash = 17*37 + ret.toHash + opts.value * 19; // TODO hash collisions
    // foreach (a; args) hash = hash*37 + a.toHash;
    // if (auto p = hash in cache) return *p;

    auto totalArgs = args.length;
    size_t argsSize;
    if (opts.objc) {
      totalArgs += 2;
      argsSize += ffi_arg.sizeof * 2;
    }
    else if (opts.self) {
      totalArgs += 1;
      argsSize += ffi_arg.sizeof;
    }
    if (opts.structRetFix) {
      totalArgs += 1;
      argsSize += ffi_arg.sizeof;
    }
    foreach (a; args) argsSize += max(ffi_arg.sizeof, a.size);
    auto fnt = new FnType(ret, args);
    fnt.abi = opts.abi;
    fnt.structRetFix = opts.structRetFix;
    fnt.totalArgs = totalArgs.to!ubyte;
    fnt.argsSize = argsSize.to!ubyte;

    // writefln("%x %s", hash, fnt.toString);
    // cache[hash] = fnt;
    return fnt;
  }

  string toString() {
    Appender!string a;
    toString(a);
    return a.data;
  }
  void toString(W)(ref W w) if (isOutputRange!(W, char)) {
    auto prevInFn  = dynamicInsideFn;
    auto prevState = dynamicPrettyPrint;
    dynamicInsideFn    = true;  scope (exit) dynamicInsideFn = prevInFn;
    dynamicPrettyPrint = false; scope (exit) dynamicPrettyPrint = prevState;
    dynamicIndentLevel++;       scope (exit) dynamicIndentLevel--;
    if (prevInFn) w.tcolGray("(");
    foreach (a; args) {
      w.tcol(TCol.Type);
      a.toString(w);
      w.tcolGray(" → ");
    }
    w.tcol(TCol.Type);
    ret.toString(w);
    if (prevInFn) w.tcolGray(")");
    else if (prevState) w.tcolClose();
  }
}

struct Generic {
  alias type this;
  Type type;
  Appender!(Instance[]) instances;

  static struct Instance {
    Type*  def;
    string lookup;
    Def*[] params;
  }

  @disable this();

  this(Sym* sym) {
    this.type = Type(Native.Generic, sym);
  }

  Instance* addInstance(Type* def, string lookup) {
    instances.put(Instance(def, lookup));
    return &instances.data[$-1];
  }

  Type* lookupInstance(string lookup) {
    foreach (ref inst; instances.data) {
      if (inst.lookup == lookup) return inst.def;
    }
    return null;
  }

  Type* lookupInstance(Def*[] params) {
  search:
    foreach (ref inst; instances.data) {
      if (inst.params.length != params.length) continue;
      foreach (i, def; inst.params) {
        if (def !is params[i]) continue search;
      }
      return inst.def;
    }
    return null;
  }
}

struct Identity {
  alias def this;
  Def def;
  string home;

  string user() { return sym.name; }

  @disable this();

  this(string user, string home) {
    this.def  = Def(typeId, Sym.intern(user));
    this.home = home;
  }

  static Identity* localUser() {
    string user, home;
    version (Posix) {
      auto uid = std.getuid();
      auto pw  = pwd.getpwuid(uid);
      user = pw.pw_name.to!string;
      home = pw.pw_dir .to!string;
    }
    else version (Windows) {
      char[max(winlm.UNLEN + 1, windef.MAX_PATH + 1)] buf = void;
      auto len = buf.length.to!uint;
      auto rc = win32.GetUserNameA(buf.ptr, &len);
      // auto rc  = winsec.GetUserNameExA(winsec.EXTENDED_NAME_FORMAT.NameDisplay, buf.ptr, &len);
      enforce(rc != 0, win32Error);
      user = buf[0..len].idup;

      winerr.SUCCEEDED(winshl.SHGetFolderPathA(null, winshl.CSIDL_COMMON_APPDATA, null, 0, buf.ptr))
        .enforce("TODO COM ERROR");
      home = buf[0..buf.ptr.strlen].idup;
    }
    else static assert(0);
    return new Identity(user, home);
  }
}

/// Hierarchy of project containers. Used to resolve dependencies.
struct Registry {
  alias def this;
  Def def;

  // TODO: project lookup
  // TODO: artifact lookup
  // TODO: direct lookup (fetch only required definitions)

  @disable this();

  this(Sym* name) {
    assert(name);
    def = Def(typeReg, name);
  }
}

/// Unit of development.
struct Project {
  alias ns this;
  NS         ns;
  Registry*  reg;
  Project*[] subProjects;
  NS*[]      namespaces;

  string[] srcPaths; // testPaths, resourcePaths
  string[] srcFiles;

  // TODO targets (ios, android, x86, x64)

  Any meta; /// Authors, contributors, license, etc.
  uint versionId;

  // TODO min vile version
  // TODO dependencies
  // TODO registries
  // TODO deployment
  // TODO signing
  // TODO certificates
  // TODO profiles (debug, release, profile, test, etc)
  // TODO tasks

  @disable this();

  this(Registry* parent, Sym* name) {
    assert(parent);
    this.ns = NS(name, typeReg);
  }

  // Directories
  // Dependencies
  // Targets
  // Build tasks
}

/// A named definition inside a namespace.
struct Var {
  alias def this;
  Def  def;
  Any  data;

  @disable this();

  this(Sym* sym, Any data) {
    assert(sym);
    assert(sym.ns.length);
    this.def  = Def(typeVar, sym);
    this.data = data;
  }

  string toString() { return text("#'", sym.toString); }
}

/// A named collection of definitions. Used to organize code.
struct NS {
  alias def this;
  Def      def;
  Project* project; /// Build project owning this namespace
  string[] files;   /// Source files making up this namespace (main, repl, ...)

  Var*[hash_t] vars;
  NS* [hash_t] aliases;
  NS*[] imports;

  @disable this();

  private this(Sym* name, Type* type = typeNS) { def = Def(type, name); }

  string toString() const { return def.toString; }

  Var* define(Sym* sym, Any val) {
    assert(sym);
    assert(sym.ns is null);
    auto hash = sym.toHash;
    Var* v;
    if (auto p = hash in vars) {
      v = *p;
      v.data = val;
    }
    else {
      v = new Var(Sym.intern(def.sym.name, sym.name), val);
      vars[hash] = v;
    }
    return v;
  }

  bool tryGet(Sym* name, ref Var* v) {
    assert(name);
    auto h = name.toHash;
    if (auto p = h in vars) {
      v = *p;
      return true;
    }
    bool found;
    foreach (imp; imports) {
      if (auto p = h in imp.vars) {
        enforce(!found, text("Multiple matches for ", name.name));
        v = *p;
        found = true;
      }
    }
    return found;
  }

  static NS* intern(string name) { return intern(Sym.intern(name)); }
  static NS* intern(Sym* sym) {
    assert(sym);
    assert(sym.ns is null);
    if (auto pns = sym in interns) return *pns;
    auto ns = new NS(sym);
    interns[sym] = ns;
    return ns;
  }

  __gshared NS*[Sym*] interns;
}

struct Unbound {
  alias def this;
  Def def;
  uint __pad; // Struct must be at least 16 bytes

  static __gshared Any val;

  static Def* getDef() { return cast(Def*)val.val; }
}

struct Prop {
  alias def this;
  Def   def; // Properties are first-class Vile definitions
  Type* arg; // Return type of getter, argument type of setter
  Fn*   get; // Read the current value of the property
  Fn*   set; // Write the current value of the property
  short ivarOffset; // -1 if no ivar
  mixin(bitfields!(bool,  "readonly",   1,
                   bool,  "atomic",     1,
                   bool,  "isStatic",   1,
                   uint,  "__padding", 13));

  this(Sym* sym, Type* type) {
    assert(sym);
    assert(type);
    def = Def(typeProp, sym);
    arg = type;
  }

  string toString() {
    Appender!string w;
    toString(w);
    return w.data;
  }
  void toString(W)(ref W w) if (isOutputRange!(W, char)) {
    w.put(isStatic ? "+ " : "- ");
    arg.toString(w);
    if (ivarOffset != -1) {
      w.tcolGray(format(" %04x", ivarOffset));
    }
    if (get) w.tcolGray(" get");
    if (readonly) w.tcolGray(" readonly");
  }
}

/**
 * Function definition. Represents any callable code.
 */
struct Fn {
  alias def this;
  Def   def;
  Env   env;
  Fn*   next;   /// in overload set
  Sym*[] params;
  union {
    Native native;   // Native D function pointer.
    Extern external; // Extern called from FFI.
  }
  List* form;      // Vile evaluation form.
  void* ffi;
  mixin(bitfields!(bool, "isSpecial",  1,
                   bool, "isMacro",    1, // Eval output instead of input
                   bool, "isNative",   1, // All D natives are macros!
                   bool, "isFFI",      1, // Call using FFI cif
                   bool, "isClosure",  1,
                   bool, "isMember",   1,
                   bool, "isObjC",     1,
                   bool, "isStatic",   1,
                   bool, "isVariadic", 1,
                   byte, "__padding",  7));

  FnType* fnType() { return cast(FnType*)type; }

  alias Native = Any function(ref Env env, List* args);

  static struct Extern {
    version (OSX) SEL sel; // Objective-C selector
  }

  @disable this();

  this(ref Env env, Sym* name, FnType* type, Sym*[] params, List* _body, bool objc = false) {
    auto extra = objc ? 1 : 0;
    assert(type.args.length + extra == params.length);
    this.def = Def(cast(Type*)type, name);
    this.env = env;
    this.params = params;
    this.form = _body;
  }
  this(Sym* name, FnType* type, Native fn) {
    assert(fn);
    this.def    = Def(cast(Type*)type, name);
    this.env    = rt.rootEnv;
    this.native = fn;
  }
  this(Sym* name) {
    this.def   = Def(typeFn, name);
    this.env   = rt.rootEnv;
    this.isFFI = true;
  }

  string toString() {
    Appender!string s;
    toString(s);
    return s.data;
  }
  void toString(W)(ref W w) if (isOutputRange!(W, char)) {
    assert(fnType);
    auto prevInFn  = dynamicInsideFn;
    auto prevState = dynamicPrettyPrint;
    dynamicInsideFn    = true;  scope (exit) dynamicInsideFn = prevInFn;
    dynamicPrettyPrint = false; scope (exit) dynamicPrettyPrint = prevState;
    dynamicIndentLevel++;       scope (exit) dynamicIndentLevel--;
    if (prevState) {
      writeMeta(w);
      w.tcolOpen(TCol.Name);
      w.put(sym.toString);
      dynamicPrettyPrint = false;
      w.tcolGray(" ∷ ");
    }
    version (OSX) if (isObjC) w.put(isStatic ? "+ " : "- ");
    foreach (i, p; params) {
      w.tcol(TCol.Type);
      fnType.args[i].toString(w);
      w.tcol(TCol.None);
      w.put(" ");
      w.put(p.str);
      w.tcolGray(" → ");
    }
    w.tcol(TCol.Type);
    fnType.ret.toString(w);
    if (prevState) w.tcolClose();
  }

  static struct Builder {
    FnType* fnt;
    Sym*[] argSyms;
    Type*[] argTypes;
    Type* ret;
    FnType.Opts opts;

    void init(Vec* argsVec) {
      argSyms = new Sym*[argsVec.data.length];
      foreach (i, arg; argsVec.data) {
        switch (arg.tag) with (Any.Tag) {
        case Val:
          auto m = cast(Meta*)arg.val;
          enforce(m.type is typeMeta);
          enforce(m.data.tag is Sym);
          if (auto p = Any(keyType) in m.meta.data) {
            enforce(p.tag == Val);
            auto t = cast(Type*)p.val;
            enforce(t.type is typeType);
            if (!argTypes) argTypes = new Type*[argSyms.length];
            argTypes[i] = t;
          }
          arg = m.data;
          goto case;
        case Sym:
          argSyms[i] = arg.sym;
          break;
        default:
          enforce(0, "wrong arg decl type");
        }
      }
    }

    Fn* finish(ref Env env, Sym* name, List* form, bool objc = false) {
      assert(name);
      if (!fnt) {
        if (!ret) ret = typeAny;
        if (!argTypes) argTypes = new Type*[argSyms.length];
        foreach (ref t; argTypes) if (!t) t = typeAny;
        opts.objc = objc;
        fnt = FnType.intern(ret, argTypes, opts);
      }

      assert(argSyms.length == fnt.totalArgs, name.name);
      auto fn = new Fn(env, name, fnt, argSyms, form, objc);
      if (fnt.abi != FnType.ABI.Vile) {
        version (OSX) auto call = objc ? &Eval.ffiObjC : &Eval.ffiCall;
        else auto call = &Eval.ffiCall;
        fn.ffi = FFI.closure(fn, call, objc);
        fn.isClosure = true;
      }
      return fn;
    }
  }

  static Fn* create(ref Env env, Any[Any] meta, Sym* name, Vec* args, List* form) {
    // TODO general type'ing
    Builder b;
    b.init(args);
    if (auto p = Any(keyCDecl) in meta) {
      enforce(p.isBool);
      if (p.boolean) b.opts.abi = FnType.ABI.C;
    }
    if (auto p = Any(keyType) in meta) {
      enforce(p.type is typeType);
      auto type = (cast(Type*)p.val).aliased;
      if (type.native == Type.Native.Fn) b.fnt = cast(FnType*)type;
      else b.ret = type; // TODO return functions
    }
    return b.finish(env, name, form);
  }

  Any call(ref Env env, List* args) {
    if (isNative) {
      assert(native);
      return native(env, args);
    }
    if (isFFI) {
      assert(ffi);
      assert(fnType.ffi);
      return fnType.ffi.call(env, &this, args);
    }

    auto frame = Env(&env);
    foreach (param; params) {
      enforce(args);
      frame.symbols[param.toHash] = args.data.eval(env);
      args = args.next;
    }
    enforce(!args);
    return Eval.prog(frame, form);
  }
}

/// Partial definition.
struct Trait {
  alias def this;
  Def def;
  Any params;
  Def*[] members;
  Any impls;

  // TEMPLATE ARGUMENTS! Run code during trait "expansion"
  // - PASS STUFF AS METADATA FROM IMPL
  //   - merge Rust traits with D templates
}


// Vile Runtime
// -----------------------------------------------------------------------------

/// Graceful exit. Message is printed if non-empty and exit code is non-zero.
class ExitCode : Exception {
  int exitCode;

  this(int exitCode, string msg) {
    super(msg);
    this.exitCode = exitCode;
  }

  static void raise(int exitCode = 0, string msg = null) {
    throw new ExitCode(exitCode, msg);
  }
}

/// Bridge between host and Vile.
struct Runtime {
  version (Java) import core.runtime : rt_init, rt_term;

  Identity* identity;
  Registry* registry; /// Root of the runtime registry.
  Project*  project;
  Env*      rootEnv;

  int id;          /// Current iteration number of the REPL.

  int  exitCode;       /// Value to return from main().
  bool running = true; /// Set to false when shutting down.
  bool stderr  = true; /// Whether to use standard error for failures.
  bool daemon;         /// Background service mode.
  bool runTask;

  bool showStatusTrace    = false;
  bool showExceptionTrace = false;
  bool showErrorTrace     = true;
  bool enableAssertions   = true;

  version (Windows) {
    string tempPath;
  }

  // TODO: get current here, compiler has full supported targets list:
  // TODO: arch/cpu info
  // TODO: platform info
  // TODO: language info

  alias Modules = AliasSeq!(Perf, Builder, Reader, Eval, REPL, JIT, FFI);

  ubyte* foo;

  /// Initialize the Vile language. Calls init() on all registered modules.
  void init() {
    version (Java) rt_init();
    else version (Posix) {
      // sig.signal(sig.SIGSEGV, &onPosixSignal);
      // sig.signal(sig.SIGBUS,  &onPosixSignal);
    }
    else version (Windows) {
      char[windef.MAX_PATH + 1] buf = void;
      auto len = win32.GetTempPathA(buf.length.to!uint, buf.ptr);
      enforce(len != 0, win32Error);
      tempPath = buf[0..len].idup;
    }
    else static assert(0);
    // auto a = *foo;

    // GC.disable(); // TODO: ?
    List.emptyValue = new List(nil); // TODO: here?

    auto unbound = Sym.intern("#<Unbound>");
    Unbound.val = Any(&new Unbound(Def(typeVoid, unbound)).val);

    // Minimum requirement for a Vile environment.
    identity = Identity.localUser();
    registry = new Registry(Sym.intern("$root"));
    project  = new Project(registry, Sym.intern("vile.boot"));
    rootEnv  = new Env(null);
    rootEnv.ns = nsVileBase; // vile.base is "imported" in every module
    // TODO: use these ^ need to move shared static this to inits first

    foreach (m; Modules) m.init();

    // TODO: decouple from runtime
    // if (daemon) {
    //   langServer = new LangServer();
    //   langServer.start();
    // }
  }

  /// Terminates the Vile language. Calls term() on all initialized modules.
  void term() {
    // if (langServer) {
    //   langServer.destroy();
    //   langServer = null;
    // }
    foreach_reverse (m; Modules) guard!(m.term, true)();

    version (Java) rt_term();
  }

  /// Runs the Vile language.
  void exec() {
    assert(rootEnv);
    if (rt.runTask) exit(0);
    else if (!rt.daemon) REPL.run();
    else {
      writeln("Running");
      new Semaphore().wait(); // TODO
    }
  }

  /// Handle OS signals. Try and recover from errors first.
  version (Posix) static extern(C)
  void onPosixSignal(int signal) nothrow @nogc {
    sig.sigset_t set;
    sig.sigemptyset(&set);
    sig.sigaddset(&set, signal);
    sig.sigprocmask(sig.SIG_UNBLOCK, &set, null);
    printf("CRASH\n");
    exit(-1); // TODO
  }
}

/// Terminates the execution of Vile.
void quit() {
  rt.running = false;
}

/// Simple Read-Eval-Print-Loop over stdin using linenoise.
struct REPL {
  @disable this();
static:
  enum historyFile = "vile_history";

  __gshared Var*    lastError;
  __gshared Var*[3] lastResult;

  void init() {
    linenoiseSetCompletionCallback(&onCompletion);
    version(Windows) {}
    else {
      linenoiseSetHintsCallback(&onHints);
      linenoiseSetFreeHintsCallback(&free);
    }
    linenoiseHistorySetMaxLen(512);
    linenoiseHistoryLoad(historyFile);

    auto ns = vc.currentNS;
    lastError = ns.define(Sym.intern("!e"), Unbound.val);
    foreach (n, ref v; lastResult)
      v = ns.define(Sym.intern(text('!', n + 1)), Unbound.val);
  }
  void term() {
    linenoiseHistorySave(historyFile);
  }

  void run() {
    auto env = Env(rt.rootEnv);
    while (rt.running) {
      env.ns = vc.currentNS;
      tryEval(read, env).print;
    }
  }

  /// Safe version of eval() guarding against exceptions.
  Any tryEval(Any form, ref Env env) nothrow {
    try {
      auto v = eval(form, env);
      lastResult[2].data = lastResult[1].data;
      lastResult[1].data = lastResult[0].data;
      return lastResult[0].data = v;
    }
    catch (Exception e) return lastError.data = Any(e);
  }

  extern (C) nothrow:

  void onCompletion(const(char)* s, linenoiseCompletions* completions) {
    auto len   = s.strlen;
    auto start = len;
    while (start > 0) {
      if (Reader.invalidSymbolChar(s[start - 1])) break;
      start--;
    }

    auto prefix = s[0..start];
    auto search = s[start..len];
    // printf("%.*s\n", cast(uint)prefix.length, prefix.ptr);
    try {
      foreach (v; vc.currentNS.vars.values.sort!"a.sym.name < b.sym.name") {
        if (v.sym.name.startsWith(search)) {
          auto name = v.sym.name;
          auto buf = cast(char*)alloca(prefix.length + name.length + 1);
          auto a = prefix.length;
          auto b = name.length + a;
          buf[0..a] = prefix;
          buf[a..b] = name;
          buf[b] = '\0';
          linenoiseAddCompletion(completions, buf);
        }
      }
    }
    catch (Exception e) printThrowable(e);
  }

  char* onHints(const(char)* s, int* color, int* bold) {
    auto prefix = s[0..s.strlen];
    *bold  = 1;
    *color = 34;
    return null;
  }
}


// Vile Reader
// -----------------------------------------------------------------------------

/// Data -> Text
alias print = writeln;

/// Exception thrown on read errors. Contains range information and source code.
class ReadException : Exception {
  this(string msg, string file, uint line, uint col) {
    super(text(file, ':', line, ':', col, ": ", msg));
  }
  // TODO: start pos and source lines
}

/**
 * Reader for Vile data structures.
 */
struct Reader {
  /// Type of a reader macro function. Used to implement the entirety of the
  /// reader, with the exception of the number and symbol literal forms.
  alias Macro = Any function(ref Reader);

  /// Information about a point in the source text.
  static struct Point {
    size_t pos;    /// Byte offset in the UTF-8 source text. Used as an index.
    uint   line;   /// Line number, starts at 1. Line 0 means an empty file.
    uint   column; /// Unicode character column number in the current line.
                   /// Starts at 0 on a new line but indexing starts at 1.
                   /// Column 0 means end of the previous line, or empty file.
  }

  string text;     /// The full source text being read. TODO: support encodings
  string file;     /// Location of the text on disk (file://) or memory (#<x>)
  Point  start;    /// Location where the current form starts in the source.
  Point  current;  /// Location of the front character in the source.
  size_t prevPos;  /// Byte offset of the previous UTF-8 character.
  size_t nextPos;  /// Byte offset of the next UTF-8 character from peek().
  dchar  front;    /// Current Unicode character being read.

  alias current this;

  @disable this();

  this(string text, string file, uint line = 1, uint column = 0) {
    assert(text.length);
    assert(file.length);
    this.text = text;
    this.file = file;
    this.line = line;
    this.column = column;
    this.prevPos = -1;
    next(); // init this.front
  }

  /// Consumes the currently peak()'d character.
  private dchar consume(dchar ch) {
    assert(ch != EOF);
    if (ch == '\n') {
      line++;
      column = 0;
    }
    else {
      column++;
    }
    return ch;
  }

  /// Consumes the next character in the source code.
  dchar next() {
    prevPos = pos;
    if (pos == text.length) return front = cast(dchar)EOF;
    return front = consume(text.decode(pos));
  }
  /// Peeks at the next character in the source code without consuming it.
  dchar peek() {
    nextPos = pos;
    if (pos == text.length) return EOF;
    return text.decode(nextPos);
  }
  /// Consumes a character after peeking at it.
  void peekEnd(dchar ch) {
    prevPos = pos;
    pos     = nextPos;
    if (ch != EOF) consume(ch);
    front = ch;
  }

  /// Creates an error value to describe a read failure at the current position.
  Any error(string msg) {
    // TODO: use start to locate the origin of the read error
    throw new ReadException(msg, file, line, column);
  }

  /// Returns the next symbolic value in the source text.
  Any read(dchar end = dchar.init, Any endValue = nil) {
    while (true) {
      if (pos > text.length) return error("EOF while reading");

      auto ch = front;
      while (ch.isWhite) ch = next();
      if (end != dchar.init && ch == end) return endValue;

      start.pos = prevPos; // TODO: line/column

      // Dispatch all characters to reader macros, except for numbers and symbols.
      // They are used as fallback cases when no read macro would otherwise match.
      // TODO: . + and - read macros, dispatch readTable before readNumber()
      auto maybeDecimal = ch == '.' || ch == '+' || ch == '-';
      if (maybeDecimal) return readNumberMaybeDecimal();
      if (ch.isDigit)   return readNumber();
      if (auto f = findMacro!readTable(ch)) {
        auto form = f(this);
        if (form == readSkip) continue;
        return form;
      }
      return readSymbol();
    }
  }

  Any readNumberMaybeDecimal() {
    auto ch = peek;
    if (!ch.isDigit) return readSymbol();

    auto decimal  = front == '.';
    auto negative = front == '-';
    peekEnd(ch); // Mutates this.front.
    return readNumber(decimal, negative);
  }

  static dchar readNumber(dchar maxDigit, bool hex = false)
    (ref Reader r, ref Appender!string n)
  {
    n.put(r.front);
    while (true) {
      auto ch = r.next;
      if (ch == '_') continue;

      static if (hex) { if (ch.isHexDigit) goto loop; }
      else { if (ch >= '0' && ch <= maxDigit) goto loop; }

      return ch;
    loop: n.put(ch);
    }
    assert(0);
  }

  Any readNumber(bool dec = false, bool neg = false) {
    dchar ch = void;
    dchar lc = void;
    auto base = 10;
    bool bin, hex, oct, zero;
    bool big, exp, sign = true;
    // TODO char[32] stackAlloc; then fallback to dyn for large numbers?
    Appender!string n;

    dchar function(ref Reader, ref Appender!string) readFn = &readNumber!'9';

    // Prefix
    if (dec) n.put('.');
    else if (front == '0') {
      ch = next;
      lc = ch.toLower;
      if (lc == 'b') {
        ch = next;
        bin = true;
        base = 2;
        readFn = &readNumber!'1';
      }
      else if (lc == 'x') {
        ch = next;
        hex = true;
        base = 16;
        readFn = &readNumber!('9', true);
      }
      else if (ch == '_') {
        ch = next;
        oct = true;
        base = 8;
        readFn = &readNumber!'8';
      }
      else if (ch.isDigit) {
        oct = true;
        base = 8;
        readFn = &readNumber!'8';
      }
      else {
        zero = true;
        n.put('0');
      }
    }

    // Integer
    if (!zero) ch = readFn(this, n);

    // Decimal
    if (ch == '.') {
      // TODO: allow 0x12.34 but also 0x12.prn
      // if (dec || bin || oct) goto done; // Allow 12.someFn
      // else {
        dec = true;
        n.put('.');
        next();
        ch = readFn(this, n);
      // }
    }

    // Exponent
    if (dec) {
      exp = ch.toLower == (hex ? 'p' : 'e');
      if (exp) {
        n.put(ch);
        ch = next();
        if (ch == '+' || ch == '-') {
          n.put(ch);
          next();
        }
        auto save = pos;
        ch = readFn(this, n);
        if (save == pos && ch != EOF && !text[pos].isDigit)
          return error("malformed exponent");
      }
    }

    // Suffix
    lc = ch.toLower;
    if (lc == 'f') {
      dec = true;
      ch = next;
    }
    if (lc == 'u') {
      ch = next;
      sign = false;
      if (ch == 'L') {
        ch = next;
      }
    }
    else if (ch == 'L') {
      ch = next;
      if (lc == 'u') {
        ch = next;
        sign = false;
      }
    }
    else if (ch == 'N') {
      if (dec) return error("malformed number");
      big = true;
      ch = next;
    }
    else if (ch == 'M') {
      big = true;
      dec = true;
      ch = next;
    }

    // Validate
    if (ch != EOF && ch != ')' && ch != ']' && ch != '}' && !ch.isWhite)
      return error("malformed number");

  done:
    enforce(!neg || sign, "Only signed numbers can be negative");
    auto s = n.data;
    Val* v;
    if (big) {
      if (!dec) return error("TODO: bignum");
      if (base != 10) return error("BigInt must be in base 10");
      enforce(sign); // TODO ?
      v = &new Prim(BigInt(s)).val;
    }
    else if (dec) {
      auto x = s.to!float * (neg ? -1 : 1);
      return Any(x); // TODO
    }
    else if (sign) {
      auto x = s.to!long(base) * (neg ? -1 : 1);
      if (abs(x) < (Any.tagHigh >> 1)) return Any(x);
      assert(0); // TODO
    }
    else {
      auto x = s.to!ulong(base);
      if (x < Any.tagHigh) return Any(x);
      assert(0); // TODO
    }
    return Any(v);
  }

  /// String of characters delimiting readable forms. Whitespace/EOF implicit.
  static immutable delims = "()[]{}";

  static bool invalidSymbolChar(dchar ch) pure nothrow @nogc {
    return ch == EOF || ch.isWhite || delims.indexOf(ch) != -1;
  }

  /// Read characters until the next form delimiter and return its text slice.
  string readIdent() {
    while (true) if (invalidSymbolChar(next())) break;
    return text[start.pos..prevPos];
  }

  /// Read an identifier as a symbol. Handles literal nil, true and false.
  Any readSymbol() {
    auto id = readIdent();
    switch (id) {
    case "nil":   return nil;
    case "true":  return boolTrue;
    case "false": return boolFalse;
    default:      return Any(Sym.intern(id.idup));
    }
  }

  static Any readKeyword(ref Reader r) {
    auto ch = r.peek;
    if (ch == EOF || ch == ':' || ch.isWhite) { // TODO ::nskey but :: is a sym
      r.peekEnd(ch);
      return Any(Sym.intern(r.readIdent()));
    }
    return Any(Key.intern(r.readIdent()));
  }

  /// Read an escaped Unicode character in UTF-8/16/32 form.
  dchar readEscapeHex(int count) {
    uint n;
    foreach (_; 0..count) {
      auto ch = next;
      dchar base, high;
      if (ch >= 'a') {
        base = 'a';
        high = 'f';
      }
      else if (ch >= 'A') {
        base = 'A';
        high = 'F';
      }
      else if (ch >= '0') {
        base = '0';
        high = '9';
      }
      else goto error;
      if (ch > high) goto error;
      n = n * 0xf + (ch - base);
    }
    return n;

  error: return dchar.init;
  }

  /// Read an escaped Unicode character in Octal form.
  dchar readEscapeOctal() {
    assert(0);
  }

  /// Read an escaped Unicode character.
  dchar readEscape() {
    auto ch = next;
    switch (ch) {
    // Literal
    case '\\': return '\\';
    case '\'': return '\'';
    case '"':  return '"';
    // Special
    case '0':  return '\0';
    case 'a':  return '\a';
    case 'b':  return '\b';
    case 'f':  return '\f';
    case 'n':  return '\n';
    case 'r':  return '\r';
    case 't':  return '\t';
    case 'v':  return '\v';
    // Octal
    case '1':..case '7': return readEscapeOctal();
    // Hexadecimal (UTF-8/16/32 respectively)
    case 'x': return readEscapeHex(2);
    case 'u': return readEscapeHex(4);
    case 'U': return readEscapeHex(8);
    // Error
    default: return dchar.init;
    }
  }

  /// Read a string literal.
  static Any readString(ref Reader r) {
    assert(r.front == '"');

    bool escape;
    while (true) {
      auto ch = r.next;
      if (ch == EOF) return r.error("unexpected EOF while reading string");
      else if (ch == '"') break;
      else if (ch == '\\') {
        auto newCh = r.readEscape();
        if (newCh == dchar.init) return r.error("invalid escape sequence");
        escape = true;
      }
    }
    assert(r.front == '"');

    auto str = new Str(r.text[r.start.pos+1 .. r.pos-1].dup);
    return Any(&str.val);
  }

  /// Read a comment form. TODO
  static Any readComment(ref Reader r) {
    do {
      r.next();
      if (r.front == '\n') {
        r.next;
        break;
      }
    } while (r.front != EOF);
    return readSkip;
  }

  /// Apply a reader macro from the given dispatch table.
  static Any readDispatch(alias table)(ref Reader r) {
    if (auto pr = r.next in hashTable) return (*pr)(r);
    return r.error(.text("invalid dispatch character #", r.front));
  }

  static void setMeta(Meta* m, Any k, Any v) {
    if (!m.meta) m.meta = new Map(cast(Map.Data)null);
    m.meta.data[k] = v;
  }

  /// Associate meta-data with the next form read.
  static Any readMeta(ref Reader r) {
    r.next();
    auto env = Env(rt.rootEnv);
    env.ns = vc.currentNS;
    auto meta = r.read().eval(env);
    auto data = r.read();
    Meta* m;
    if (data.type is typeMeta) {
      m = cast(Meta*)data.val;
    }
    else {
      m = new Meta(data);
      data = Any(&m.val);
    }
    if (meta.isKey) {
      setMeta(m, meta, boolTrue);
    }
    else if (meta.type is typeType) {
      setMeta(m, Any(keyType), meta);
    }
    else {
      enforce(meta.type is typeMap);
      enforce(!m.meta, "TODO merge meta");
      m.meta = cast(Map*)meta.val;
    }
    return data;
  }

  /// TODO \ is no longer meta but document macros!
  static Any readMetaOrChar(ref Reader r) {
    auto ch = r.next();
    auto p  = r.peek();
    if (invalidSymbolChar(p)) {
      r.peekEnd(p);
      return Any(ch);
    }
    return nil; // TODO
  }

  /// Expand a prefix character and the next form into a (<sym> <form>) list.
  static Any readExpand(alias sym)(ref Reader r) if (is(typeof(sym) : Sym*)) {
    r.next();
    return Any(new List(Any(sym), new List(r.read())), Any.Tag.Val);
  }

  /// Read a sequence of forms until the end character and call new T on them.
  static Any readCollection(dchar end, T)(ref Reader r) {
    Appender!(Any[]) tmp;
    while (true) {
      auto ch = r.next;
      while (ch.isWhite) ch = r.next;
      auto form = r.read(end, readFinished);
      if (form == readFinished) break;
      tmp.put(form);
      if (r.front == end) break;
    }
    assert(r.front == end);
    r.next();

    static if (is(typeof(T.emptyValue)))
      if (tmp.data.empty)
        return Any(T.emptyValue, Any.Tag.Val);

    return Any(new T(tmp.data), Any.Tag.Val);
  }

  /// Handle dangling closing delimiters.
  static Any readInvalid(ref Reader r) {
    return r.error(.text("invalid dispatch character ", r.front));
  }

  /// Mark the next form as unused. This is a compiler hint to avoid warnings.
  static Any readUnused(ref Reader r) {
    return r.readSymbol();
  }

  static Any readPragma(ref Reader r) {
    r.next();
    if (r.front == '(') {
      auto list = cast(List*)readCollection!(')', List)(r).val;
      if (list.length & 1) return r.error("odd list length for #?");
      while (list) {
        assert(list.next);
        if (!list.data.isKey) return r.error("#? expects features to be keys");
        if (list.data.key in features) return list.next.data;
        list = list.next.next;
      }
    }
    else {
      auto k = r.read();
      auto v = r.read();
      if (!k.isKey) return r.error("#? expects features to be keys");
      if (k.key in features) return v;
    }
    return readSkip;
  }

  /// Process a form template, allowing evaluation through ~ and ~@.
  static Any readSyntaxQuote(ref Reader r) {
    assert(0);
  }

  /// Perform evaluation inside a form template.
  static Any readUnquote(ref Reader r) {
    assert(0);
  }

  /// Read a regular expression literal form.
  static Any readRegexp(ref Reader r) {
    auto s = readString(r);

    // TODO: Regex type
    return nil;
  }

  /// Read the first line of a file as an interpreter directive.
  static Any readShebang(ref Reader r) {
    if (r.line != 1 || r.column != 0) return r.error("unexpected shebang");
    return readComment(r); // TODO: shebang meta
  }

  /// Create an error to describe an unreadable form. Such as #<Ptr 12345678>
  static Any readUnreadable(ref Reader r) { return r.error("unreadable form"); }

  /// Skip the next form in the source code and read the one after instead.
  static Any readDiscard(ref Reader r) {
    r.next();
    r.read();
    return readSkip;
  }

  /// Lookups a macro in the given dispatch table. Maps are used for Unicode.
  static Macro findMacro(alias table)(dchar ch) {
    auto pmacro = ch in table;
    return pmacro ? *pmacro : null;
  }

  /*
   * READ is primary dispatch table, directly off characters in source text.
   *
   * HASH is the extended dispatch table, called from the '#' primary dispatch.
   *
   * POST is the symbol dispatch table, called from literal identifiers.
   * Rationale: Complement the other reader macros with post-identifier logic.
   * For example 'foo#' uses gensym as a post-reader macro
   */
  private __gshared Macro[dchar] readTable, hashTable, postTable;

  private __gshared Macro[string] metaTable;

  private __gshared Any[Key*] features;

  static void init() {
    /**/ version (Posix)   features[Key.intern(":posix")]   = boolTrue;

    /**/ version (linux)   features[Key.intern(":linux")]   = boolTrue;
    else version (OSX)     features[Key.intern(":macos")]   = boolTrue;
    else version (Windows) features[Key.intern(":windows")] = boolTrue;
    else static assert(0, "Unsupported platform");

    /**/ version (X86)    features[Key.intern(":x86")] = boolTrue;
    else version (X86_64) features[Key.intern(":x64")] = boolTrue;
    else static assert(0, "Unsupported arch");

    /**/ version (BigEndian)    features[Key.intern(":big-endian")]    = boolTrue;
    else version (LittleEndian) features[Key.intern(":little-endian")] = boolTrue;

    features[Key.intern(":default")] = boolTrue;
    features[Key.intern(":debug")]   = boolTrue;

    readTable = ['"':  &readString,
                 ':':  &readKeyword, // TODO: part of symbol?
                 ';':  &readComment, // till end of line (TODO: what about '# ')
                 // '?':  &readExpand!symNil, // TODO: ?foo means nilable foo?
                 '&':  &readExpand!symRef,
                 '@':  &readExpand!symDeref,
                 '\'': &readExpand!symQuote,
                 '`':  &readSyntaxQuote,
                 '~':  &readUnquote,
                 '\\': &readMetaOrChar, // LaTeX-like (with character literals)
                 '^':  &readMeta,       // Clojure-like
                 '#':  &readDispatch!hashTable,
                 '(':  &readCollection!(')', List),
                 '[':  &readCollection!(']', Vec),
                 '{':  &readCollection!('}', Map),
                 ')':  &readInvalid,
                 ']':  &readInvalid,
                 '}':  &readInvalid,
                 '_':  &readUnused];
    hashTable = [' ':  &readComment, // till end of line
                 '#':  &readComment, // TODO: whats ##
                 '!':  &readShebang,
                 '"':  &readRegexp,
                 '\'': &readExpand!symVar,
                 '\\': &readMeta, // TODO: whats #\
                 // '(':  &readStuff, // TODO: whats #(hello), dont force () on fn
                 // '{':  &readCollection!('}', Set), // TODO BUG https://issues.dlang.org/show_bug.cgi?id=19877
                 // ':':  &readNSMap,
                 '?':  &readPragma, // conditional
                 '+':  &readPragma,
                 '-':  &readPragma,
                 '<':  &readUnreadable,
                 '_':  &readDiscard];
    // postTable = ['#': &readGensym, // TODO: foo# is (gensym "foo")
    //              '?': &readMaybe, // TODO: foo? means boolean result?
    //              '*': &readPointer // TODO: Int* is a pointer to an int?
    //              ];

    metaTable = ["region":    &readMeta,
                 "endregion": &readMeta];
  }

  static void term() {
    readTable.clear(), hashTable.clear(), postTable.clear();
    metaTable.clear();
  }
}

/// Reads a single form from the given source text.
Any read(string text, string file) {
  try return Reader(text, file).read();
  catch (Exception e) return Any(e); // No ambiguity; exceptions are unreadable!
}
/// Reads a single form from the line read out of standard input.
Any read() {
  auto prompt = text("\x1b[36m", vc.currentNS.sym.toString, "\x1b[0m ",
                     ++rt.id, "\x1b[32m=>\x1b[0m \0");
  cerror.errno = 0; // Linenoise doesn't clear the last error.
  auto line = linenoise(prompt.ptr);
  if (line is null) {
    if (cerror.errno == cerror.EAGAIN) return nil; // Ctrl-C
    quit(); // Ctrl-D
    auto a = new List(Any(Sym.intern("Bye!")), null);
    auto b = new List(Any(symQuote), a);
    return Any(&b.val);
  }

  if (line[0] == '\0') return nil;
  if (line[0] != ' ') line.linenoiseHistoryAdd();
  scope (exit) line.linenoiseFree();

  auto lineString = line[0..line.strlen()].assumeUnique;
  if (lineString == ":quit") {
    quit();
    return nil;
  }

  return lineString.read("#<stdin>");
}


// Vile Compiler
// -----------------------------------------------------------------------------

// Actual syntax is irrelevant! We want In -> Vile -> Out. Can print in any way.

// Support literate
// Support notebook
// Support dumb terminal
// Support smart terminal
// Support gui (see notebook but more IDE-ish)
// Output LLVM, C, basic ASM (for live coding)
//
// AST describes expressions independently of their presentation

// Decouple expressions from presentations
// M-expressions? could use i-expressions from scheme, or just decouple ^

// TODO: not from bootstrap! implement in Vile and call with analyze, compile or build
struct AST {
  // TODO: go more basic, CL style; labels, blocks, etc
  /// Semantics of an AST node.
  enum Kind {
    Bind,
    Case,
    CaseTest,
    CaseThen,
    Catch,
    Const,
    Def,
    DefType,
    Do,
    Fn,
    FnMethod,
    If,
    Import,
    Invoke,
    KeywordInvoke,
    Let,
    LetFn,
    Local,
    Map,
    Method,
    Quote,
    Set,
    Call,
    Throw,
    Try,
    Var,
    VarRef,
    Vector,
    WithMeta
  }

  struct Binding {

  }
}

// Meta Any :: Reader output
// - Line
// - Column
// + user specified meta data in the source (^{:foo "hello"} 12)


/// Data -> AST
struct Analyzer {
  // Def var
  // Let var
  // Fn Call
}

struct TypeChecker {
}

/// Lexical environment used to resolve symbols
struct Env {
  Any[hash_t] symbols; /// Symbol hash to value lookup table.

  NS*  ns;     /// Namespace of declaration, for definition forms
  Env* parent; /// Direct owner (ns.env, )

  @disable this();

  this(Env* parent) {
    this.parent = parent;
  }

  bool tryGet(ref Sym sym, ref Any v) {
    if (auto p = sym.toHash in symbols) {
      v = *p;
      return true;
    }
    return false;
  }

  Any lookup(ref Sym sym) {
    Any x;
    if (tryGet(sym, x)) return x;
    Var* v;
    if (ns && ns.tryGet(&sym, v)) return v.data;
    if (parent) return parent.lookup(sym);
    throw new Exception("Symbol not found: " ~ sym.toString);
  }
}

/// AST -> Code
struct Compiler {
  NS* currentNS;
}

void compile() {
  // TODO
}

/// Code -> Program
struct Linker {

}

void link() {
  // TODO
}

version(none)
final struct Externs {
  @disable this();
  enum dmdVileJsonMaxDepth = 12;

static:

  void init() {
    enum error = "Invalid vile.json";
    auto vileJson = "./vile.json".readText.parseJSON(dmdVileJsonMaxDepth);
    enforce(vileJson.array.length == 1, error);

    vileJson = vileJson[0];
    enforce(vileJson["name"].str == "vile",     error);
    enforce(vileJson["kind"].str == "module",   error);
    enforce(vileJson["file"].str == "./vile.d", error);

    auto handle = dl.dlopen(null, dl.RTLD_LAZY);
    enforce(handle, dl.dlerror.to!string);

    foreach (m; vileJson["members"].array) {
      if (auto p = "protection" in m) if (p.str == "private") continue;

      auto kind = m["kind"].str;
      switch (kind) {
      case "enum": parseEnum(m);     break;
      case "class": parseClass(m);   break;
      case "struct": parseStruct(m); break;
      case "function": parseFunc(m, handle); break;
      case "variable": parseVar(m);  break;
      case "alias":
      case "import":
      case "value":
      case "static import":
      case "template": break; // ignore
      default: enforce(0, "Invalid member kind: " ~ kind);
      }
    }

    dl.dlclose(handle);
  }

  void term() {}

  void parseEnum(JSONValue m) {
    writeln("enum ", m["name"].str);
  }

  void parseClass(JSONValue m) {
    writeln("class ", m["name"].str);
  }

  void parseStruct(JSONValue m) {
    writeln("struct ", m["name"].str);

  }

  void parseFunc(JSONValue m, void* handle) {
    // if ()
    // writeln("func ", m["name"].str);

    bool isC;
    if (auto p = "linkage" in m) isC = p.str == "c";

    string name;
    if (isC) {
      name = "_" ~ m["name"].str ~ "\0";
    }
    else {
      return; // TODO
    }

    auto p = dl.dlsym(handle, name.ptr);
    enforce(p, dl.dlerror.to!string);
    writeln(m["name"].str, " = ", p);
  }

  void parseVar(JSONValue m) {
    writeln("var ", m["name"].str);

  }
}

class EvalException : Exception {
  this(string msg, Exception next) {
    super(msg, next);
  }

  override string toString() {
    // return msg;
    Appender!string e;
    e.put(simpleStr);
    auto n = next;
    while (n) {
      e.put("\nCaused by:\n");
      if (auto ex = cast(EvalException)n) e.put(ex.simpleStr);
      else e.put(n.toString);
      n = n.next;
    }
    return e.data;
  }

  string simpleStr() { return super.toString(); }
}

final class Eval {
  @disable this();
static:

  /// Interns a variable.
  Any def(ref Env v, List* args) {
    enforce(args, "def: too few arguments");
    enforce(args.data.isSym, "def: expecting symbol");
    auto sym = args.data.sym;
    auto val = Unbound.val;
    if (auto rest = args.next) {
      enforce(!rest.next, "def: too many arguments");
      val = rest.data.eval(v);
    }
    return Any(&vc.currentNS.define(sym, val).val);
  }

  /// Create bindings in the current environment
  Any let(ref Env v, List* args) {
    enforce(args, "let: too few arguments");
    auto bindings = args.data.safeVec.data;
    enforce(bindings.length % 2 == 0, "let: invalid binding length");
    auto env = Env(&v);
    for (int i; i < bindings.length; i += 2) {
      enforce(bindings[i].isSym, "let: expecting binding symbol");
      env.symbols[bindings[i].sym.toHash] = bindings[i + 1].eval(env);
    }
    return prog(env, args.next); // TODO: redefine let to not require prog?
  }

  /// Create a new function.
  Any fn(ref Env v, List* args) {
    enforce(args, "fn: too few arguments");
    Sym* sym;
    auto meta = meta(args.data);
    auto asym = meta ? meta.data : args.data;
    if (!asym.isSym) sym = Sym.gen("fn");
    else {
      sym  = asym.sym;
      args = args.next;
      enforce(args, "fn: too few arguments");
    }
    return Any(&Fn.create(v, meta ? meta.meta.data : null, sym, args.data.safeVec, args.next).val);
  }

  /// Create a new trait.
  Any trait(ref Env v, List* args) {
    return nil; // TODO
  }

  /// Specialized cond for a single test.
  Any ifCond(ref Env v, List* args) {
    enforce(args && args.next, "if: too few arguments");
    auto consThen = args.next;
    auto consElse = consThen.next;
    enforce(!consElse || !consElse.next, "if: too many arguments");
    if (args.data.eval(v).toBool) return consThen.data.eval(v);
    if (consElse) return consElse.data.eval(v);
    return nil;
  }

  /// Evaluate all expressions, use the last one as the result.
  Any prog(ref Env v, List* args) {
    if (!args) return nil;
    while (args.next) {
      args.data.eval(v);
      args = args.next;
    }
    return args.data.eval(v);
  }

  /// Create or update a module or project.
  Any ns(ref Env v, List* args) {
    enforce(args.next is null, "ns: too many arguments");
    enforce(args.data.isSym, "ns: symbol expected");
    auto ns = NS.intern(args.data.sym);
    vc.currentNS = ns; // TODO: atomic
    return Any(&ns.val);
  }

  /// Defer evaluation of a form.
  Any quote(ref Env v, List* args) {
    enforce(args.next is null, "quote: too many arguments");
    return args.data;
  }

  /// Refer to a definition instead of its implementation.
  Any var(ref Env v, List* args) {
    enforce(args, "var: too few arguments");
    enforce(args.next is null, "var: too many arguments");
    enforce(args.data.isSym, "var: expecting symbol");
    Var* var;
    if (vc.currentNS.tryGet(args.data.sym, var))
      return Any(&var.val);
    enforce(0, "var not found " ~ args.data.sym.toString);
    assert(0);
  }

  Any reference(ref Env v, List* args) {
    enforce(args, "ref: too few arguments");
    enforce(args.next is null, "ref: too many arguments");
    auto x = args.data.eval(v);
    enforce(x.isVal, "TODO");
    auto val = x.val;
    switch (val.type.aliased.native) with (Type.Native) {
    default:
      enforce(0, "TODO");
      assert(0);
    case Bool:..
    case Char:
    case Pointer:
      return .Any(&new Prim(.Type.pointer(val.type), &(cast(Prim*)val).ptr).val);
    case Struct:
      return .Any(&new Prim(.Type.pointer(val.type), cast(void*)val + 16).val);
    case Type:
      return .Any(&.Type.reference(cast(.Type*)val).val);
    }
  }

  Any deref(ref Env v, List* args) {
    enforce(args, "ref: too few arguments");
    enforce(args.next is null, "deref: too many arguments");
    auto x = args.data.eval(v);
    switch (x.tag) {
    default: enforce(0); assert(0);
    case Any.Tag.Val:
      auto val = x.val;
      auto a = val.type.aliased;
      switch (a.native) with (Type.Native) {
      default:
        enforce(0, "TODO");
        assert(0);
      case Type: return .Any(&.Type.pointer(cast(.Type*)val).val);
      case Pointer:
        auto p = (cast(Prim*)val).ptr;
        switch (a.next.aliased.native) {
        default: enforce(0); assert(0);
        case SInt8:  return .Any(cast(long)*cast(byte*)p);
        case SInt16: return .Any(cast(long)*cast(short*)p);
        case SInt32: return .Any(cast(long)*cast(int*)p);
        case SInt64: return .Any(*cast(long*)p);
        case UInt8:  return .Any(cast(ulong)*cast(ubyte*)p);
        case UInt16: return .Any(cast(ulong)*cast(ushort*)p);
        case UInt32: return .Any(cast(ulong)*cast(uint*)p);
        case UInt64: return .Any(*cast(ulong*)p);
        case Float32: return .Any(*cast(float*)p);
        case Float64: return .Any(*cast(double*)p);
        case Float80: return .Any(*cast(real*)p);
        }
      }
    }
  }

  // Simple C struct. No inheritance, no properties, no methods.
  //
  // (data Point
  //   UInt32 x
  //   UInt32 y)
  //
  // (let [pt (Point. 1 2)]
  //  (. pt x))

  // Objective-C "category". (Adds methods to an existing class)
  //
  // (data NSStringExtensionsOfDoom :for NSString
  //  (NSString oh-my-me [self NSObject o] (. o description)))
  //
  // (. (. NSString stringWithUTF8String: "Hello") oh-my-me)

  // Objective-C interface. (Extends from another Objective-C interface.)
  //
  // (data AppDelegate :extends NSObject
  //  (NSWindow* window :strong :nonatomic)
  //  NSApplicationDelegate
  //  )
  //
  // (new AppDelegate)

  // Windows COM interface. (Extends from another COM interface.)
  //
  // (data SomeIFace :extends IUnknown
  //  )

  // C++ class. (Extends from another C++ class.)
  //
  // (data MyVector :extends ImGui::Vector2
  //  )

  /// Create a new data type
  Any data(ref Env v, List* args) {
    enforce(args, "data: too few arguments");
    enforce(args.data.isSym, "data: expecting symbol");
    auto sym = args.data.sym;
    args = args.next;
    enforce(sym.name[0].isUpper, "data: expecting name to be capitalized");
    enforce(sym.ns.empty, "data: expecting unqualified name");
    Appender!(Struct.Member[]) fields;
    Appender!(Proto[]) protos;
    Struct* extends;
    version (OSX) bool objc;
    // Type information
    while (args && args.data.isKey) {
      auto k = args.data.key;
      args = args.next;
      if (k is keyExtends) {
        auto base = args.data.eval(v);
        enforce(base.type is typeType, "Invalid base type");
        extends = cast(Struct*)base.val;
        args = args.next;
        // TODO: more than objc classes
        version (OSX) {
          objc = extends.native == Type.Native.NSObject;
          enforce(!objc || !extends.isProtocol && !extends.isCategory,
                  text("Not a class: ", base));
        }
      }
      else enforce(0, text("Unknown data key ", k.toString));
    }
    // Members
    // while (args && args.data.type is typeList) {
    //   fields.put(parseMember(v, cast(List*)args.data.val));
    //   args = args.next;
    // }
    // Conforms
    while (args && args.data.isSym) {
      auto t = args.data.eval(v);
      enforce(t.isVal);
      auto s = cast(Struct*)t.val;
      enforce(s.type.type is typeType, text("Not a type: "));
      enforce(s.native == Type.Native.NSObject);
      enforce(s.isProtocol);
      args = args.next;
      Appender!(Struct.Member[]) methods;
      while (args && args.data.type is typeList) {
        methods.put(parseMember(s, v, cast(List*)args.data.val));
        args = args.next;
      }
      protos.put(Proto(s, methods.data));
    }
    enforce(!args, "data: too many arguments");
    // Type
    auto type = new Struct(sym);
    type.fields = fields.data;
    version (OSX) if (objc) { // Objective-C class
      auto base = cast(Struct*)extends;
      type.native = Type.Native.NSObject;
      auto cls = objc_allocateClassPair(cast(Class)base.objc,
                                        sym.name.toStringz, 0);
      type.objc = cls;
      enforce(type.objc, "Failed to allocate Objective-C class");
      scope (failure) objc_disposeClassPair(cls); // TODO also clear members
      scope (success) objc_registerClassPair(cls);
      // objcAddMembers(type, fields.data, null);
      foreach (ref proto; protos.data) {
        if (proto.type.objc) class_addProtocol(cls, cast(id)proto.type.objc);
        objcAddMembers(type, proto.methods, proto.type);
      }
      FFI.Analyzer.classes[cls] = type;
      goto result;
    }
    // { // Simple C struct
    //   type.fields = fields.data;
    //   // TODO: protos -> traits?
    // }
  result:
    return Any(&vc.currentNS.define(sym, Any(&type.val)).val);
  }

  struct Proto {
    Struct*         type;
    Struct.Member[] methods;
  }

  Struct.Member parseMember(Struct* proto, ref Env v, List* args) {
    enforce(args !is List.emptyValue, "Empty data member");
    enforce(args.data.isSym, "Data member: expecting symbol");
    enforce(args.next, "Data member: too few arguments");
    auto sym = args.data.sym;
    args = args.next;
    auto m = proto.lookup(sym);
    if (args.data.type is typeVec) { // sym []
      Fn.Builder b;
      b.fnt = m.fn.fnType;
      b.init(cast(Vec*)args.data.val);
      auto fn = b.finish(v, sym, args.next, true);
      version (OSX) {
        fn.isObjC = true;
        fn.external.sel = m.fn.external.sel;
      }
      return Struct.Member(fn);
    }
    enforce(0, "invalid member");
    assert(0);
    // enforce(args.data.isSym, "Data member: expecting symbol");
    // auto result = sym.val.eval(v);
    // enforce(result.type is typeType, "Not a type");
    // auto type = cast(Type*)result.val;
    // sym  = args.data.sym;
    // args = args.next;
    // if (args && args.data.type is typeVec) { // Type sym []
    //   auto fn = Fn.create(sym, cast(Vec*)args.data.val, args.next);
    //   fn.ret = type;
    //   return Struct.Member(fn);
    // }
    // else { // Type sym :keys
    //   assert(0, "TODO");
    // }
  }

  version (OSX)
  void objcAddMembers(Struct* type, Struct.Member[] members, Struct* proto) {
    auto cls = cast(Class)type.objc;
    foreach (ref m; members) {
      final switch (m.kind) with (Struct.Member.Kind) {
      case Data: assert(0); // Parsed as properties instead.
      case Type: break;
      case Prop:
        version(none) // TODO
        class_addProperty(cls);
        break;
      case Func:
        // SEL sel;
        // if (!proto) goto user;
        // if (auto parent = proto.lookup(m.sym)) {
        //   enforce(parent.kind == Func, "Protocol member is not a method");
        //   // Parent has `self` implied in its parameter list.
        //   auto fnt = parent.fn.fnType;
        //   enum fmt = "Protocol fn arg count mismatch: %s, expected %s got %s";
        //   enforce(parent.fn.params.length + 1 == m.fn.params.length,
        //           format(fmt, m.fn.sym.name, parent.fn.params.length + 1,
        //                  m.fn.params.length));
        //   sel = parent.fn.external.sel;
        //   // infer(m.fn.ret, parent.fn.ret);
        //   // m.fn.type = cast(Type*)fnt;
        //   m.fn.params[0].type = cast(.Type*)type;
        //   // foreach (n, ref arg; parent.fn.params)
        //   //   infer(m.fn.params[n+1].type, arg.type);
        //   // writeln(*m.fn);
        // }
        // else { user:
        //   sel = sel_registerName(m.sym.name.toStringz);
        //   assert(0, "TODO: " ~ m.sym.name);
        // }
        // m.fn.isClosure    = true;
        // m.fn.isObjC       = true;
        // m.fn.external.sel = sel;
        // auto imp = cast(IMP)FFI.closure(m.fn, &ffiObjC, true);
        auto ret = class_addMethod(cls, m.fn.external.sel, cast(IMP)m.fn.ffi, objcTypes(m.fn.fnType));
        enforce(ret, text("Failed to add method: ", *m.sym));
        break;
      }
    }
    type.fields ~= members;
  }

  version (OSX)
  void infer(ref Type* type, Type* newType) {
    assert(type);
    enforce(type is typeAny || type is newType, "Type mismatch");
    type = newType;
  }

  version (OSX)
  immutable(char)* objcTypes(FnType* fn) {
    Appender!string s;
    objcType(s, fn.ret);
    s.put("@:"); // id and SEL
    foreach (t; fn.args[1..$]) objcType(s, t); // Skip `self`
    s.put('\0'); // C string
    // writeln(s.data);
    return s.data.ptr;
  }

  version (OSX)
  void objcType(ref Appender!string s, Type* t) {
    if (!t) {
      s.put('?');
      return;
    }
    switch (t.native) with (Type.Native) {
    default:       s.put('?'); break;
    case SInt8:    s.put('c'); break;
    case SInt16:   s.put('s'); break;
    case SInt32:   s.put('i'); break;
    case SInt64:   s.put('q'); break;
    case UInt8:    s.put('C'); break;
    case UInt16:   s.put('S'); break;
    case UInt32:   s.put('I'); break;
    case UInt64:   s.put('Q'); break;
    case Float32:  s.put('f'); break;
    case Float64:  s.put('d'); break;
    case Bool:     s.put('B'); break;
    case Void:     s.put('v'); break;
    case NSObject: s.put('@'); break;
    case Pointer:
      if (t.next is typeSInt8) s.put('*');
      else if (t.next.native == Pointer)  objcType(s, t.next);
      else if (t.next.native == NSObject) s.put('@');
      else {
        s.put('^');
        objcType(s, t.next);
      }
      break;
      // TODO: [array] {structure} (union) b<bits> : @ (SEL and Class)
    }
  }

  /// Handles a method call from Objective-C to Vile.
  version (OSX)
  extern (C) void ffiObjC(ffi_cif* _, void* ret, void** args, void* pfn) {
    auto fn = cast(Fn*)pfn;
    assert(fn);
    assert(fn.type.native == Type.Native.Fn);
    assert(fn.isClosure);
    assert(fn.isObjC);
    assert(fn.external.sel is *cast(SEL*)args[1]);
    auto fnt = fn.fnType;
    auto env = Env(&fn.env);
    env.symbols[fn.params[0].toHash] = Any(cast(ulong)*cast(ffi_arg*)args[0], Any.Tag.ObjC);
    foreach (n; 1..fn.params.length) {
      env.symbols[fn.params[n].toHash] =
        FFI.Fn.toAny(*cast(ffi_arg*)args[n+1], fnt.args[n-1]);
    }
    auto result = prog(env, fn.form);
    if (fnt.ret.type.native != Type.Native.Void) {
      fromAny(result, ret, fnt.ret);
    }
  }

  extern (C) void ffiCall(ffi_cif* _, void* ret, void** args, void* pfn) {
    auto fn = cast(Fn*)pfn;
    assert(fn);
    assert(fn.type.native == Type.Native.Fn);
    assert(fn.isClosure);
    auto fnt = fn.fnType;
    auto env = Env(&fn.env);
    foreach (n, t; fnt.args) {
      env.symbols[fn.params[n].toHash] = FFI.Fn.toAny(*cast(ffi_arg*)args[n], t);
    }
    auto result = prog(env, fn.form);
    if (fnt.ret.native != Type.Native.Void) {
      fromAny(result, ret, fnt.ret);
    }
  }

  /// Create a new type alias
  Any type(ref Env v, List* args) {
    // (alias NewType (Ptr UInt32))
    // (alias VertexArray (ImGUI::Vector Vertex))
    return nil;
  }

  /// Call a member method or access a property of a C++, ObjC or D object.
  Any self(ref Env v, List* args) {
    enforce(args && args.next,    ".: too few arguments");
    enforce(args.next.data.isSym, ".: expecting method symbol");
    auto obj = args.data.eval(v);
    auto sym = args.next.data.sym;
    enforce(sym.ns.empty, "Member symbols can't have namespace qualifiers");
    switch (obj.tag) with (Any.Tag) {
    case ObjC:
      version (OSX) return selfObjC(v, obj, sym, args, false);
      else assert(0);
    case Val:
      auto val = cast(.Val*)obj.val;
      if (val.type is typeType) {
        auto type = (cast(.Type*)val).aliased;
        switch (type.native) with (Type.Native) {
        case Enum:  return selfEnum(cast(.Enum*)type, sym);
        case Struct: enforce(0, "TODO static method calls"); break;
        case NSObject:
          version (OSX) return selfObjC(v, obj, sym, args, true); // TODO pass Struct, dont use reflection
          else assert(0);
        default:
        }
      }
      else {
        auto a = val.type.aliased;
        switch (a.native) with (Type.Native) {
        case Struct:
          return selfThis(v, (cast(void*)val + 16), cast(.Struct*)a, sym, args);
        case Pointer:
          auto next = cast(.Struct*)a.next.aliased;
          enforce(next.type.native == Struct);
          return selfThis(v, (cast(.Prim*)val).ptr, next, sym, args);
        default:
        }
      }
      goto default;
    default:
      enforce(0, text("Invalid member target: ", obj.tag));
      assert(0);
    }
  }

  Any selfEnum(Enum* e, Sym* sym) {
    foreach (ref v; e.values) if (v.sym is sym) return v.data;
    enforce(0, format("No member %s in enum %s", sym.name, e.sym.name));
    assert(0);
  }

  Any selfThis(ref Env v, void* self, Struct* s, Sym* sym, List* args) {
    assert(self);
    auto m = s.lookup(sym);
    enforce(m, text("No such member: ", sym.name));
    enforce(m.kind == Struct.Member.Kind.Func);
    enforce(m.fn.isMember);

    return m.fn.fnType.ffi.callThis(v, m.fn, (*cast(void***)self)[m.offset], self, args.next.next);
  }

  version (OSX)
  Any selfObjC(ref Env v, Any obj, Sym* sym, List* args, bool isStatic) {
    id target;
    Struct* cls;
    if (!isStatic) { // instance
      cls = cast(Struct*)obj.type;
      target = cast(id)obj.ptr;
    }
    else { // class
      cls = cast(Struct*)obj.val;
      target = cast(id)cls.objc;
    }
    auto member = cls.lookup(sym);
    if (!member) {
      auto n = sym.name; // min length setter is "setA:"
      if (n.length >= 5 && n.startsWith("set") && n[3].isUpper && n[$-1] == ':') {
        member = cls.lookup(Sym.intern(text(n[3].toLower, n[4..$-1]))); // TODO lookup by hash, hash without alloc
      }
    }
    enforce(member, text("No such member: ", sym.name));
    args = args.next.next;
    Fn* fn;
    switch (member.kind) with (Struct.Member.Kind) {
    case Prop: // Property getter or setter
      auto prop = member.prop;
      if (args) { // Setter
        enforce(!args.next, ".: too many arguments");
        fn = prop.set;
      }
      else { // Getter
        fn = prop.get;
      }
      break;
    case Func: // Method call
      fn = member.fn;
      break;
    default:
      enforce(0, text("Member not callable: ", sym.name));
      assert(0);
    }
    enforce(fn);
    enforce(fn.isStatic == isStatic,
            format("static mismatch: expected %s got %s",
                   fn.isStatic, isStatic));
    assert(fn.isObjC);
    return fn.fnType.ffi.callObjC(v, fn, target, args);
  }

  /// Load foreign symbol definitions.
  Any ffi(ref Env v, List* args) {
    enforce(args && args.next, "ffi: too few arguments");
    enforce(args.data.isSym, "ffi: expecting language symbol");
    auto lang = args.data.sym;
    args = args.next;

    FFI.Opts opts;
    /**/ if (lang is symC)         opts.mode = FFI.ParseMode.C;
    else if (lang is symCPlusPlus) opts.mode = FFI.ParseMode.CPlusPlus;
    else if (lang is symObjC)      opts.mode = FFI.ParseMode.ObjC;
    else enforce(0, "Unknown FFI language: " ~ lang.toString);

    Appender!(string[]) files;
    Appender!(FFI.Lib[]) libs;
    do {
      auto map = args.data.safeMap;
      auto lib = map.get(keyLib);
      auto inc = map.get(keyInclude);
      auto imp = map.get(keyImport);
      if (lib.isNil && inc.isNil && imp.isNil) {
        enforce(opts.libs.empty, "ffi: unexpected options");
        enforce(args.next, "ffi: too few arguments");

        // auto cpp = map.get(keyCPP, boolFalse);
        // if (!cpp.isTrue) opts.mode |= FFI.ParseMode.NoCPP; // FIXME

        auto wrap = map.get(keyWrap);
        if (wrap.isTrue) opts.mode |= FFI.ParseMode.Wrap;

        auto full = map.get(keyFull);
        if (full.isTrue) opts.mode |= FFI.ParseMode.Full;

        auto flags = map.get(keyFlags);
        if (!flags.isNil) {
          auto vec = flags.safeVec.data;
          opts.flags.length = vec.length;
          foreach (i, f; vec) opts.flags[i] = f.safeStr.data.assumeUnique;
        }
      }
      else {
        enforce(!inc.isNil, "ffi: missing :include");
        enforce(!imp.isNil, "ffi: missing :import");

        string libFile;
        if (lib.isVal) {
          auto val = lib.val;
          if (val.type is typeStr) libFile = (cast(Str*)val).data.assumeUnique;
        }

        auto libIncs = inc.safeVec.data;
        auto libImps = imp.safeVec.data;
        foreach (_, f; libIncs) files.put(f.safeStr.data.assumeUnique);

        auto symbols = new Sym*[libImps.length];
        foreach (i, s; libImps) {
          enforce(s.isSym, "ffi: import symbol expected");
          symbols[i] = s.sym;
        }

        libs.put(FFI.Lib(libFile, symbols));
      }

      args = args.next;
    } while (args);

    opts.files = files.data;
    opts.libs  = libs.data;
    return FFI.include(&opts);
  }

  Any typeOf(ref Env env, List* args) {
    enforce(args, "type: too few arguments");
    enforce(!args.next, "type: too many arguments");
    return Any(&args.data.eval(env).type.val);
  }

  Any sizeOf(ref Env env, List* args) {
    enforce(args, "type: too few arguments");
    enforce(!args.next, "type: too many arguments");
    auto v = args.data.eval(env);
    enforce(v.isVal);
    auto val = v.val;
    if (val.type is typeType) {
      return Any(cast(size_t)(cast(Type*)val).aliased.size);
    }
    else {
      return Any(cast(size_t)v.val.type.aliased.size);
    }
  }

  Any meta(ref Env env, List* args) {
    enforce(args, "meta: too few arguments");
    enforce(!args.next, "meta: too many arguments");
    if (auto m = meta(args.data.eval(env))) return Any(&m.meta.val);
    return nil;
  }

  Meta* meta(Any v) {
    if (!v.isVal) return null;
    auto val = v.val;
    if (val.type !is typeMeta) return null;
    return cast(Meta*)val;
  }

  Any op1(string operator)(ref Env env, List* args) {
    enforce(args);
    enforce(!args.next);
    auto a = args.data.eval(env);
    switch (a.tag) with (Any.Tag) {
    case Bool: return Any(mixin(operator~" a.boolean"));
    case SInt: return Any(mixin(operator~" a.sinteger"));
    case UInt: return Any(mixin(operator~" a.uinteger"));
    case Float: return Any(mixin(operator~" a.floating"));
    default: enforce(0); assert(0);
    }
  }

  enum floatOps = ["+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">=", "&&", "||"];
  Any op(string operator)(ref Env env, List* args) {
    immutable hasFloat = floatOps.countUntil(operator) != -1;
    enforce(args);
    enforce(args.next);
    enforce(!args.next.next);
    auto a = args.data.eval(env);
    auto b = args.next.data.eval(env);
    switch (a.tag) with (Any.Tag) {
    // case Val:
    //   break; // TODO
    case Bool: return op!operator(a.boolean, b);
    case SInt: return op!operator(a.sinteger, b);
    case UInt: return op!operator(a.uinteger, b);
    case Float: static if (hasFloat) return op!operator(a.floating, b);
    default: enforce(0); assert(0);
    }
  }
  Any op(string operator, T)(T a, Any b) {
    immutable hasFloat = floatOps.countUntil(operator) != -1;
    switch (b.tag) with (Any.Tag) {
    case Bool: return Any(mixin("a "~operator~" b.boolean"));
    case SInt: return Any(mixin("a "~operator~" b.sinteger"));
    case UInt: return Any(mixin("a "~operator~" b.uinteger"));
    case Float: static if (hasFloat) return Any(mixin("a "~operator~" b.floating"));
    default: enforce(0); assert(0);
    }
  }

  Any eq(ref Env env, List* args) {
    assert(0);
  }
  Any neq(ref Env env, List* args) {
    assert(0);
  }
  Any greater(ref Env env, List* args) {
    assert(0);
  }
  Any less(ref Env env, List* args) {
    assert(0);
  }
  Any greaterEq(ref Env env, List* args) {
    assert(0);
  }
  Any lessEq(ref Env env, List* args) {
    assert(0);
  }

  Any not(ref Env env, List* args) {
    assert(0);
  }
  Any and(ref Env env, List* args) {
    assert(0);
  }
  Any or(ref Env env, List* args) {
    assert(0);
  }
  Any bitNot(ref Env env, List* args) {
    assert(0);
  }

  Any typeCast(ref Env env, List* args) {
    enforce(args);
    enforce(args.next);
    enforce(!args.next.next);
    auto x = args.data.eval(env);
    enforce(x.isVal);
    auto t = cast(Type*)x.val;
    enforce(t.type is typeType);
    auto v = args.next.data.eval(env);
    ulong bits;
    switch (v.tag) with (Any.Tag) {
    case Ptr:
      return ptrCast(v.ptr, t);
    case Val:
      auto val = cast(Prim*)v.val;
      enforce(val.type.native == Type.Native.Pointer, "TODO "~val.type.sym.name);
      return ptrCast(val.ptr, t);
    case SInt:
      bits = cast(size_t)v.sinteger;
      goto intcast;
    case UInt:
      bits = v.uinteger;
      intcast:
      switch (t.aliased.native) {
      case Type.Native.Pointer: return ptrCast(cast(void*)bits, t);
      default: return v; // TODO int casts
      }
    default:
      enforce(0, "TODO");
      return nil;
    }
  }

  Any ptrCast(void* ptr, Type* type) {
    switch (type.aliased.native) with (Type.Native) {
    case Pointer: return .Any(&new Prim(type, ptr).val);
    case SInt8:
    case SInt16:
    case SInt32:
    case SInt64: return .Any(cast(ptrdiff_t)ptr);
    case UInt8:
    case UInt16:
    case UInt32:
    case UInt64: return .Any(cast(size_t)ptr);
    default:
      enforce(0, "TODO "~type.sym.name);
      assert(0);
    }
  }

  FnType* dynFnType;

  Any debugPrint(ref Env env, List* args) {
    import std.stdio : write;
    while (args) {
      write(args.data.eval(env).toString, " ");
      args = args.next;
    }
    writeln();
    return nil;
  }

  Any array(ref Env env, List* args) {
    enforce(args);
    enforce(args.next);
    enforce(!args.next.next);
    auto t = args.data.eval(env).toType;
    auto v = args.next.data.eval(env);
    size_t l;
    void* p;
    if (v.isVal) {
      auto vec = cast(Vec*)v.val;
      enforce(vec.type is typeVec);
      l = vec.data.length;
      p = new ubyte[t.size * l].ptr;
      auto ptr = p;
      for (auto i = 0; i < l; i++) {
        fromAny(vec.data[i], ptr, t);
        ptr += t.size;
      }
      writeln((cast(float*)p)[0..21]);
    }
    else {
      l = v.toUInt;
      p = new ubyte[t.size * l].ptr;
    }
    auto a = new Array(Type.array(t), l, p);
    return Any(&a.val);
  }

  Any aget(ref Env env, List* args) {
    enforce(args);
    enforce(args.next);
    enforce(!args.next.next);
    auto a = args.data.eval(env).toArray;
    auto n = args.next.data.eval(env).toUInt;
    auto t = a.type.next;
    enforce(n < a.length);
    return toAny(a.ptr + t.size * n, t);
  }
  Any aset(ref Env env, List* args) {
    enforce(args);
    enforce(args.next);
    enforce(args.next.next);
    enforce(!args.next.next.next);
    auto a = args.data.eval(env).toArray;
    auto n = args.next.data.eval(env).toUInt;
    auto x = args.next.next.data.eval(env);
    auto t = a.type.next;
    enforce(n < a.length);
    fromAny(x, a.ptr + t.size * n, t);
    return nil;
  }

  Any whileLoop(ref Env env, List* args) {
    enforce(args);
    while (true) {
      if (!args.data.eval(env).toBool) break;
      prog(env, args.next);
    }
    return nil;
  }

  Any fromCStr(ref Env env, List* args) {
    enforce(args);
    enforce(!args.next);
    auto v = args.data.eval(env);
    enforce(v.isVal);
    auto p = cast(Prim*)v.val;
    auto a = p.type.aliased;
    enforce(a.native == Type.Native.Pointer, a.native.to!string);
    enforce(a.next.aliased is typeUInt8, a.next.sym.name);
    auto s = cast(char*)p.ptr;
    auto l = strlen(s);
    return Any(&Str(s[0..l]).val);
  }

  Any tmp(ref Env env, List* args) {
    auto val = cast(UUID*)(cast(void*)args.data.eval(env).val+16);
    writeln(*val);
    return nil;
  }

  Any cond(ref Env env, List* args) {
    while (args) {
      if (!args.next) return args.data.eval(env);
      if (args.data.eval(env).toBool) {
        return args.next.data.eval(env);
      }
      args = args.next.next;
    }
    return nil;
  }
  Any caseCond(ref Env env, List* args) {
    enforce(args);
    auto v = args.data.eval(env);
    args = args.next;
    while (args) {
      if (!args.next) return args.data.eval(env);
      if (args.data.eval(env) == v) {
        return args.next.data.eval(env);
      }
      args = args.next.next;
    }
    return nil;
  }

  void init() {
    FnType.Opts opts;
    opts.abi = FnType.ABI.D;
    dynFnType = FnType.intern(typeAny, null, opts);

    intern("tmp",   &tmp);    // Development helper
    intern("def",   &def);    // Intern a variable in the current ns
    intern("let",   &let);    // Bind variables in a new env
    intern("ns",    &ns);     // Create a namespace
    symFn = intern("fn",    &fn);     // Create a function
    intern("trait", &trait);  // Create a type trait
    intern("if",    &ifCond); // Single test, branch on failure
    intern("do",    &prog);   // Evaluate a sequence of expressions
    intern("type",  &typeOf); // Evaluate and return the type of a form
    intern("sizeof", &sizeOf);
    intern("quote", &quote);  // Defer the evaluation of a form
    intern("var",   &var);    // Refer to an interned variable
    intern("ref",   &reference);
    intern("deref", &deref);
    intern("data",  &data);
    intern("alias", &type);
    intern("meta",  &meta);
    intern("print", &debugPrint);
    intern("array", &array);
    intern("aget",  &aget);
    intern("aset",  &aset);
    intern("c->str", &fromCStr);
    // intern("new",   &alloc);
    intern(".",     &self);
    intern("ffi",   &ffi);    // Refer to external definitions
    intern("while", &whileLoop);
    intern("case",  &caseCond);
    intern("cond",  &cond);

    symCast = intern("cast", &typeCast);

    symAdd = intern("+", &op!"+");
    symSub = intern("-", &op!"-");
    symMul = intern("*", &op!"*");
    symDiv = intern("/", &op!"/");
    symMod = intern("mod", &op!"%");
    // symInc = intern("inc", &inc); TODO FFI code want mutations
    // symDec = intern("dec", &dec);
    symEq        = intern("=",  &op!"==");
    symNeq       = intern("!=", &op!"!=");
    symGreater   = intern(">",  &op!">");
    symLess      = intern("<",  &op!"<");
    symGreaterEq = intern(">=", &op!">=");
    symLessEq    = intern("<=", &op!"<=");

    symNot = intern("not", &op1!"!");
    symAnd = intern("and", &op!"&&");
    symOr  = intern("or",  &op!"||");

    symBitNot = intern("bit-not", &bitNot);
    symBitAnd = intern("bit-and", &op!"&");
    symBitOr  = intern("bit-or",  &op!"|");
    symBitXor = intern("bit-xor", &op!"^");
    symBitShl = intern("bit-shift-left",  &op!"<<");
    symBitShr = intern("bit-shift-right", &op!">>");
  }
  void term() {}

  Sym* intern(string name, Fn.Native f) {
    auto sym = Sym.intern(name);
    auto fn  = new Fn(sym, dynFnType, f);
    fn.isMacro  = true;
    fn.isNative = true;
    nsVileBase.define(sym, Any(&fn.val));
    return sym;
  }
}

/// Evaluates a form in a given environment.
Any eval(Any form, ref Env env) {
  final switch (form.tag) with (Any.Tag) {
  case Nil:
  case Ptr:
  case Bool:
  case Char:
  case SInt:
  case UInt:
  case Float:
  case Key:
  case Obj:
  case ObjC: return form; // Direct Value
  case Sym:
  case Val:
  /*case Meta:*/ return eval(*form.val, env); // E-Value-ate
  }
}

Array* evalArray(List* args, ref Env env) {
  enforce(args && !args.next);
  auto x = args.data.eval(env);
  enforce(x.isVal);
  auto a = cast(Array*)x.val;
  enforce(a.type.native == Type.Native.Array);
  return a;
}
Any evalArrayLength(List* args, ref Env env) {
  return Any(evalArray(args, env).length);
}
Any evalArrayPtr(List* args, ref Env env) {
  auto a = evalArray(args, env);
  return Any(&new Prim(Type.pointer(a.type.next), a.ptr).val);
}

/// ditto
Any eval(ref Val x, ref Env env) {
  // writeln("EVAL ", x.toString);
  try {
    if (x.type is typeSym) {
      auto sym = cast(Sym*)&x;
      return sym.name.length > 1 && sym.name[0] == '.' ? Any(&x) : env.lookup(*sym);
    }
    if (x.type is typeList) {
      auto call = cast(List*)&x;
      if (call != List.emptyValue) {
        // TODO internal structs
        //if (call.data is symArrayLength) return evalArrayLength(call.next, env);
        //if (call.data is symArrayPtr)    return evalArrayPtr(call.next, env);
        auto func = call.data.eval(env);
        auto args = call.next;
        if (func.isVal) {
          auto v = func.val;
          if (v.type.native == Type.Native.Fn) {
            return (cast(Fn*)v).call(env, args);
          }
          else if (v.type is typeType) {
            auto type = cast(Type*)v;
            auto t = type.aliased;
            switch (t.native) {
            default:
              writeln("EVAL: ", t.native);
              break;
            case Type.Native.Bool:..
            case Type.Native.Char:
              return Any(&new Prim(type, 0).val);
            case Type.Native.Pointer:
              return Any(&new Prim(type, null).val);
            case Type.Native.Struct:
              auto s = cast(Struct*)t;
              if (s.size == 0) throw new Exception("Cannot instantiate opaque struct");
              auto m = cast(Val*)new ubyte[16 + s.size].ptr;
              m.type = cast(Type*)v;
              if (args) {
                auto p = cast(void*)m + 16;
                if (args.data.isKey) {
                  // TODO
                }
                else {
                  auto n = 0;
                  do {
                    auto mem = s.nextDataField(n);
                    enforce(mem, "more initializer than fields");
                    fromAny(args.data.eval(env), p + mem.offset, mem.type);
                    args = args.next;
                  } while (args);
                }
              }
              return Any(m);
            case Type.Native.NSObject:
              version (OSX) {
                auto s = cast(Struct*)t;
                enforce(!s.isCategory && !s.isProtocol, "Not a class");
                enforce(s.objc, "Missing objc class reference");
                auto id = class_createInstance(cast(Class)s.objc, 0);
                enforce(id, "Failed to create objc instance");
                return Any(cast(ulong)id, Any.Tag.ObjC);
              }
              else assert(0);
            }
          }
          else if (v.type is typeSym) {
            auto s = cast(Sym*)v;
            if (s.name[0] == '.') {
              return evalMember(s, env, args);
            }
          }
        }

        throw new Exception("Not a function: " ~ func.toString);
      }
    }
    // TODO: generalize over list/vec/set/map
    else if (x.type is typeVec) {
      auto vec = cast(Vec*)&x;
      auto results = new Any[vec.data.length];
      foreach (i, expr; vec.data) results[i] = expr.eval(env);

      auto result = new Vec(results);
      return Any(&result.val);
    }
    else if (x.type is typeMap) {
      Map.Data results;
      foreach (k, v; (cast(Map*)&x).data) results[k.eval(env)] = v.eval(env);

      auto result = new Map(results);
      return Any(&result.val);
    }
    // https://issues.dlang.org/show_bug.cgi?id=19877
    // else if (x.type is typeSet) {
    //   auto results = new Set.Data();
    //   foreach (v; (cast(Set*)&x).data) results.insert(v.eval(env));
    //   auto result = new Set(results);
    //   return Any(&result.val);
    // }

    return Any(&x);
  }
  catch (EvalException e) {
    // TODO: build stack trace
    throw e;
  }
  catch (Exception e) {
    throw new EvalException(e.msg, e); // TODO: file/line/col
  }
}

Any evalMember(Sym* dotted, ref Env env, List* args) {
  enforce(dotted.ns.empty);
  enforce(args, "too few arguments");
  enforce(!args.next);
  auto sym = Sym.intern(dotted.name[1..$]);
  auto any = args.data.eval(env);
  enforce(any.isVal);
  auto v = any.val;
  auto p = cast(void*)v + 16;
  auto a = v.type.aliased;
  switch (a.native) {
  default: enforce(0, "TODO: " ~ a.native.to!string); assert(0);
  case Type.Native.Pointer:
  loop: while (true) {
      p = (cast(Prim*)v).ptr;
      a = a.next.aliased;
      switch (a.native) {
      default: enforce(0, "TODO: " ~ a.native.to!string); assert(0);
      case Type.Native.Struct: break loop;
      case Type.Native.Pointer: p = *cast(void**)p; break;
      }
    }
    goto case;
  case Type.Native.Struct:
    auto s = cast(Struct*)a;
    auto m = s.lookup(sym);
    enforce(m, "No such member: " ~ sym.name);
    if (m.type.native != Type.Native.Fn) {
      return toAny(p + m.offset, m.type);
    }
    enforce(m.fn.isMember);

    enforce(0, "TODO merge with . path");
    assert(0);
  case Type.Native.DynArray:
    auto array = cast(Array*)v;
    if (sym is symArrayPtr) {
      return Any(&new Prim(Type.pointer(array.type.next), array.ptr).val);
    }
    if (sym is symArrayLength) {
      return Any(array.length);
    }
    enforce(0, "No such array member: " ~ sym.name);
    assert(0);
  }
}

string unexpected(Type* expected, Any x) {
  return text("Expected ", expected.sym.name, " but got ", x.type.sym.name);
}

void fromAny(Any x, void* p, Type* expected) {
  // writeln("ASSIGN ", expected.sym.name, " ", x.tag);
  auto a = expected.aliased;
  final switch (a.native) with (Type.Native) {
  case Bool:
    enforce(x.isBool, unexpected(expected, x));
    *cast(bool*)p = x.boolean;
    break;
  case Char:
    enforce(x.isChar, unexpected(expected, x));
    *cast(dchar*)p = x.character;
    break;
  case SInt8:
    enforce(x.isSInt, unexpected(expected, x));
    *cast(byte*)p = x.uinteger.to!byte;
    break;
  case SInt16:
    enforce(x.isSInt, unexpected(expected, x));
    *cast(short*)p = x.uinteger.to!short;
    break;
  case SInt32:
    enforce(x.isSInt, unexpected(expected, x));
    *cast(int*)p = x.uinteger.to!int;
    break;
  case SInt64:
    enforce(x.isSInt, unexpected(expected, x));
    *cast(long*)p = x.uinteger;
    break;
  case UInt8:
    enforce(x.isUInt, unexpected(expected, x));
    *cast(ubyte*)p = x.uinteger.to!ubyte;
    break;
  case UInt16:
    enforce(x.isUInt, unexpected(expected, x));
    *cast(ushort*)p = x.uinteger.to!ushort;
    break;
  case UInt32:
    enforce(x.isUInt, unexpected(expected, x));
    *cast(uint*)p = x.uinteger.to!uint;
    break;
  case UInt64:
    enforce(x.isUInt, unexpected(expected, x));
    *cast(ulong*)p = x.uinteger;
    break;
  case Float16: assert(0);
  case Float32:
    enforce(x.isFloat, unexpected(expected, x));
    *cast(float*)p = x.floating;
    break;
  case Float64:
    enforce(x.isFloat, unexpected(expected, x));
    *cast(double*)p = x.floating;
    break;
  case Float80: assert(0);
  case Fn:
    if (x.isNil) {
      *cast(void**)p = null;
    }
    else {
      enforce(x.isVal);
      auto v = x.val;
      enforce(v.type is a);
      enforce(v.type.native == Fn);
      auto fn = cast(.Fn*)v;
      if (fn.fnType.abi != FnType.ABI.Vile) {
        enforce(fn.isClosure);
        enforce(fn.ffi);
        *cast(void**)p = fn.ffi;
      }
      else {
        writeln("TODO FN PTR ASSIGN");
      }
    }
    break;
  case Pointer:
    switch (x.tag) {
    case .Any.Tag.Nil:
      *cast(void**)p = null;
      break;
    case .Any.Tag.Ptr:
      enforce(a.next.aliased is typeVoid, unexpected(expected, x));
      *cast(void**)p = x.ptr;
      break;
    case .Any.Tag.Val:
      auto v = x.val;
      auto t = v.type.aliased;
      auto anext = a.next.aliased;
      if (t.native == Pointer) {
        enforce(anext.isAssignableFrom(t.next), unexpected(expected, x));
        *cast(void**)p = (cast(Prim*)v).ptr;
      }
      else if (t is typeStr) {
        if (anext is typeUInt8 || anext is typeVoid) {
          *cast(immutable(char)**)p = (cast(Str*)v).data.toStringz;
        }
        else if (anext is typeChar16) {
          *cast(immutable(wchar)**)p = (cast(Str*)v).data.toUTF16z;
        }
        else {
          unexpected(expected, x);
        }
      }
      else {
        enforce(0, t.toString);
      }
      break;
    case .Any.Tag.ObjC:
      version (OSX) {
        // enforce(a.next.native == NSObject); ??
        *cast(id*)p = cast(id)x.uinteger;
        break;
      }
      else assert(0);
    default:
      enforce(0, x.tag.to!string);
    }
    break;
  case Struct:
    enforce(x.isVal);
    auto val = x.val;
    enforce(val.type.aliased.native == Struct, val.type.aliased.native.to!string);
    auto s = cast(.Struct*)a;
    auto v = cast(void*)val + 16;
    p[0..s.size] = v[0..s.size];
    break;
  case Enum:
    // TODO check enum types
    auto t = a.next.aliased;
    fromAny(x, p, t);
    break;
  case Any:
    *cast(.Any*)p = x;
    break;
  case Meta:
    //fromAny((*cast(.Meta*)x.val).data, p, expected);
    enforce(0, "TODO meta");
    break;
  case NSObject:
    version (OSX) {
      enforce(x.tag == .Any.Tag.ObjC);
      *cast(id*)p = cast(id)x.uinteger;
      break;
    }
    else assert(0);
  case Array:
    enforce(x.isVal);
    auto from = cast(.Array*)x.val;
    enforce(from.type.type is typeType);
    enforce(from.type.native == DynArray);
    enforce(a.next is from.type.next);
    auto length = a.size / a.next.size;
    enforce(length >= from.length);
    memcpy(p, from.ptr, a.next.size * from.length);
    break;
  case Type:
  case Void:
  case Nil:
  case BigInt:
  case DynArray:
  case Trait:
  case DObject:
  case Generic:
  case Temp:
  case Alias: assert(0, a.native.to!string);
  }
}

Any toAny(void* p, Type* expected) {
  auto a = expected.aliased;
  final switch (a.native) with (Type.Native) {
  case Bool:   return *cast(bool*)p ? boolTrue : boolFalse;
  case SInt8:  return .Any(cast(long)*cast(byte*)p);
  case SInt16: return .Any(cast(long)*cast(short*)p);
  case SInt32: return .Any(cast(long)*cast(int*)p);
  case SInt64: return .Any(*cast(long*)p);
  case UInt8:  return .Any(cast(ulong)*cast(ubyte*)p);
  case UInt16:  return .Any(cast(ulong)*cast(ushort*)p);
  case UInt32:  return .Any(cast(ulong)*cast(uint*)p);
  case UInt64:  return .Any(cast(ulong)*cast(ulong*)p);
  case Float16: assert(0, "TODO");
  case Float32: return .Any(*cast(float*)p);
  case Float64: return .Any(cast(float)*cast(double*)p); // TODO doubles
  case Float80: assert(0, "TODO");
  case Fn: // TODO
  case Pointer:
    auto pp = cast(void**)p;
    if (!*pp) return nil;
    if (a.next.aliased is typeVoid) return .Any(pp);
    return .Any(&new Prim(expected, *pp).val);
  case Struct:
    auto s = cast(.Struct*)a;
    // TODO check struct is copyable
    auto sz = 16 + s.size;
    auto m = cast(.Val*)new ubyte[sz].ptr;
    m.type = expected;
    (cast(void*)m)[16..sz] = p[0..s.size];
    return .Any(m);
  case Enum:
    // TODO preserve enum type
    return toAny(p, a.next);
  case Meta:
  case Type:
  case Void:
  case Any:
  case Nil:
  case Char:
  case BigInt:
  case Array:
  case DynArray:
  case Trait:
  case DObject:
  case NSObject:
  case Generic:
  case Temp:
  case Alias: assert(0);
  }
}

void load() {
  // TODO actually write in Vile, but provide I/O primitives to escape sandbox
}

/// Simple perf counters
struct Perf {
static:
  void init() {}
  void term() {
    assert(top == 0);
  }

  private __gshared {
    MonoTime[16] stack;
    size_t       top;
  }

  immutable space = "                ";

  void enter() {
    stack[top++] = MonoTime.currTime;
  }
  void leave(string label) {
    auto time = MonoTime.currTime - stack[--top];
    stderr.lockingBinaryWriter
      .formattedWrite("%s%s%s%s%s%s: %s\n", space[0..top*2],
                      tctrl, cast(string)TCol.Perf, label,
                      tctrl, cast(string)TCol.None, time);
  }

  scope struct Scoped {
    string label;

    @disable this();

    this(string label) {
      this.label = label;
      enter();
    }
    ~this() { leave(label); }
  }
}

/// Build tasks
struct Builder {
  static void init() {
    vc.currentNS = NS.intern("user");
    vc.currentNS.imports ~= nsVileCore;
  }

  static void term() {

  }
}

struct Core {
  // meta
  // traits
}

/// Simple IO
struct IO {
  // file
  // file stream
}

/// Simple math
struct Math {
  // arithmetic
}

/// Simple JIT
struct JIT {
  @disable this();
static:
  ubyte* memory;

  void init() {}
  void term() {}

  version(none)
  void init() {
    memory = cast(ubyte*)
      mem.mmap(null, // address
               4096,
               mem.PROT_READ | mem.PROT_WRITE | mem.PROT_EXEC,
               mem.MAP_PRIVATE | mem.MAP_ANON,
               -1, // FILE*
               0); // offset
    enforce(memory, "Failed to map JIT memory");

    int i;

    // mov %rdi, %rax
    memory[i++] = 0x48;           // REX.W prefix
    memory[i++] = 0x8b;           // MOV opcode, register/register
    memory[i++] = 0xc7;           // MOD/RM byte for %rdi -> %rax

    // ret
    memory[i++] = 0xc3;           // RET opcode

    alias F = size_t function(size_t);
    // writeln("OMG HI ", (cast(F)memory)(12));
  }

  version(none)
  void term() {
    if (memory) {
      mem.munmap(memory, 4096);
      memory = null;
    }
  }
}

// Foreign Function Interface
// -----------------------------------------------------------------------------

/// Simple wrapper of libffi.
struct FFI {
  @disable this();
static:

  __gshared void*[string] libs;

  void init() {
    cMacroOps[CMacroOp.Add] = CMacroOpInfo(symAdd, 4);
    cMacroOps[CMacroOp.Sub] = CMacroOpInfo(symSub, 4);
    cMacroOps[CMacroOp.Mul] = CMacroOpInfo(symMul, 3);
    cMacroOps[CMacroOp.Div] = CMacroOpInfo(symDiv, 3);
    cMacroOps[CMacroOp.Mod] = CMacroOpInfo(symMod, 3);

    cMacroOps[CMacroOp.Eq]        = CMacroOpInfo(symEq,        7);
    cMacroOps[CMacroOp.Neq]       = CMacroOpInfo(symNeq,       7);
    cMacroOps[CMacroOp.Greater]   = CMacroOpInfo(symGreater,   6);
    cMacroOps[CMacroOp.Less]      = CMacroOpInfo(symLess,      6);
    cMacroOps[CMacroOp.GreaterEq] = CMacroOpInfo(symGreaterEq, 6);
    cMacroOps[CMacroOp.LessEq]    = CMacroOpInfo(symLessEq,    6);

    cMacroOps[CMacroOp.Not] = CMacroOpInfo(symNot, 2);
    cMacroOps[CMacroOp.And] = CMacroOpInfo(symAnd, 11);
    cMacroOps[CMacroOp.Or]  = CMacroOpInfo(symOr,  12);

    cMacroOps[CMacroOp.BitNot] = CMacroOpInfo(symBitNot, 2);
    cMacroOps[CMacroOp.BitAnd] = CMacroOpInfo(symBitAnd, 8);
    cMacroOps[CMacroOp.BitOr]  = CMacroOpInfo(symBitOr, 10);
    cMacroOps[CMacroOp.BitXor] = CMacroOpInfo(symBitXor, 9);
    cMacroOps[CMacroOp.BitShl] = CMacroOpInfo(symBitShl, 5);
    cMacroOps[CMacroOp.BitShr] = CMacroOpInfo(symBitShr, 5);

    auto env = Env(rt.rootEnv);
    env.ns = vc.currentNS;

    // version(none)
    q{(ffi C
       {:flags   ["-D_ANSI_SOURCE" "-D_C99_SOURCE"]}
       {:lib     #?(:linux "libc.so.6"
                    :macos "/usr/lib/system/libsystem_c.dylib"
                    :windows "msvcr120.dll")
        :include ["ctype.h"
                  "errno.h"
                  "float.h"
                  "inttypes.h"
                  "limits.h"
                  "locale.h"
                  "signal.h"
                  "stddef.h"
                  "stdint.h"
                  "stdio.h"
                  "string.h"
                  "time.h"]
        :import  [isalnum isalpha #_isblank iscntrl isdigit isgraph islower
                  isprint ispunct isspace isupper isxdigit tolower toupper
                  #_errno EDOM ERANGE EILSEQ
                  #_FLT_RADIX #_FLT_MANT_DIG #_DBL_MANT_DIG #_LDBL_MANT_DIG #_FLT_DIG
                  #_DBL_DIG #_LDBL_DIG #_FLT_MIN_EXP #_DBL_MIN_EXP #_LDBL_MIN_EXP
                  #_FLT_MIN_10_EXP #_DBL_MIN_10_EXP #_LDBL_MIN_10_EXP #_FLT_MAX_EXP
                  #_DBL_MAX_EXP #_LDBL_MAX_EXP #_FLT_MAX_10_EXP #_DBL_MAX_10_EXP
                  #_LDBL_MAX_10_EXP #_FLT_MAX #_DBL_MAX #_LDBL_MAX #_FTL_EPSILON
                  #_DBL_EPSILON #_LDBL_EPSILON #_FLT_MIN #_DBL_MIN #_LDBL_MIN #_FLT_ROUNDS
                  #_FLT_EVAL_METHOD #_DECIMAL_DIG
                  ;;imaxabs imaxdiv strtoimax strtoumax wcstoimax wcstoumax
                  imaxdiv_t
                  ;;CHAR_BIT SCHAR_MIN SCHAR_MAX UCHAR_MAX CHAR_MIN CHAR_MAX
                  ;;MB_LEN_MAX SHRT_MIN SHRT_MAX USHRT_MAX INT_MIN INT_MAX
                  ;;UINT_MAX LONG_MIN LONG_MAX ULONG_MAX LLONG_MIN LLONG_MAX
                  ;;ULLONG_MAX
                  lconv setlocale localeconv
                  signal raise #_sig_atomic_t SIGABRT SIGFPE SIGILL SIGINT SIGSEGV
                  SIGTERM #_SIG_DFL #_SIG_IGN #_SIG_ERR
                  ptrdiff_t size_t max_align_t #_nullptr_t #_offsetof #_NULL
                  intmax_t uintmax_t int8_t uint8_t int16_t uint16_t int32_t
                  uint32_t int64_t uint64_t int_least8_t uint_least8_t
                  int_least16_t uint_least16_t int_least32_t uint_least32_t
                  int_least64_t uint_least64_t int_fast8_t uint_fast8_t
                  int_fast16_t uint_fast16_t int_fast32_t uint_fast32_t
                  int_fast64_t uint_fast64_t intptr_t uintptr_t #_INTMAX_MIN
                  #_INTPTR_MIN #_INTPTR_MAX #_UINTPTR_MAX #_SIZE_MAX PTRDIFF_MIN
                  PTRDIFF_MAX SIG_ATOMIC_MIN SIG_ATOMIC_MAX #_WCHAR_MIN #_WCHAR_MAX
                  WINT_MIN WINT_MAX
                  clearerr fclose feof ferror fflush fgetc fgetpos fgets fopen
                  fprintf fputc fputs fread freopen fscanf fseek fsetpos ftell
                  fwrite getc getchar #_gets perror printf putc putchar puts
                  remove rewind scanf setbuf setvbuf #_snprintf sprintf
                  sscanf tmpfile tmpnam ungetc vfprintf #_vfscanf vsnprintf
                  vsprintf #_vsscanf #_stderr #_stdin #_stdout FILE fpos_t BUFSIZ
                  EOF FILENAME_MAX FOPEN_MAX L_tmpnam TMP_MAX
                  atof atoi atol #_atoll strtod #_strtof strtol #_strtold #_strtoll
                  strtoul strtoull rand srand abort #_atexit #_at_quick_exit exit
                  getenv #_quick_exit system #__Exit bsearch qsort div labs ldiv
                  llabs lldiv mblen mbtowc wctomb mbstowcs wcstombs EXIT_FAILURE
                  EXIT_SUCCESS #_MB_CUR_MAX RAND_MAX div_t ldiv_t lldiv_t
                  memcpy memmove strcpy strncpy strcat strncat memcmp strcmp
                  strcoll strncmp strxfrm memchr strchr strcspn strpbrk strrchr
                  strspn strstr strtok memset strerror strlen
                  clock difftime mktime time asctime ctime gmtime localtime
                  strftime #_CLOCKS_PER_SRC clock_t time_t tm]}
       {:lib     #?(:linux "libc.so.6"
                    :macos "/usr/lib/system/libsystem_kernel.dylib"
                    :windows "msvcr120.dll")
        :include []
        :import  [rename]}
       {:lib     #?(:linux "libc.so.6"
                    :macos "/usr/lib/system/libsystem_malloc.dylib"
                    :windows "msvcr120.dll")
        :include ["stdlib.h"]
        :import  [calloc free malloc realloc]}
       {:lib     #?(:linux "libm.so.6"
                    :macos "/usr/lib/system/libsystem_m.dylib"
                    :windows "msvcr120.dll")
        :include ["fenv.h"
                  "math.h"]
        :import  [fegetenv fesetenv
                  #_feclearexcept #_feraiseexcept #_fegetexceptflag #_fesetexceptflag
                  #_fegetround #_fesetround #_feholdexcept
                  #_feupdateenv #_fetestexcept fenv_t fexcept_t FE_DIVBYZERO
                  FE_INEXACT FE_INVALID FE_OVERFLOW FE_UNDERFLOW #_FE_ALL_EXCEPT
                  FE_DOWNWARD FE_TONEAREST FE_TOWARDZERO FE_UPWARD #_FE_DFL_ENV
                  cos sin tan acos asin atan atan2 cosh sinh tanh acosh asinh
                  atanh exp frexp ldexp log log10 modf exp2 expm1 ilogb log1p
                  log2 logb scalbn scalbln pow sqrt cbrt hypot erf erfc
                  tgamma lgamma ceil floor fmod trunc round lround llround rint
                  lrint llrint nearbyint remainder remquo copysign nan nextafter
                  nexttoward fdim fmax fmin fabs fma #_fpclassify #_isfinite
                  #_isinf #_isnan #_isnormal #_signbit #_isgreater #_isgreaterequal #_isless
                  #_islessequal #_islessgreater #_isunordered #_math_errhandling
                  #_INFINITY #_NAN #_HUGE_VAL #_HUGE_VALF #_HUGE_VALL MATH_ERRNO
                  MATH_ERREXCEPT FP_INFINITE FP_NAN FP_NORMAL FP_SUBNORMAL
                  FP_ZERO FP_ILOGB0 FP_ILOGBNAN double_t float_t]}
       #_
       {:lib     #?(:linux "libc.so.6"
                    :macos "/usr/lib/system/libsystem_platform.dylib"
                    :windows "msvcr120.dll")
        :include ["setjmp.h"]
        :import  [longjmp setjmp #_jmp_buf]}
       #?(:windows
          {:lib "msvcr120.dll"
           :include ["process.h"]
           :import [_beginthread
                    _beginthreadex
                    _endthread
                    _endthreadex
                    _beginthread_proc_type
                    _beginthreadex_proc_type]}
          :default
          {:lib     #?(:linux "libpthread.so.0"
                       :macos "/usr/lib/system/libsystem_pthread.dylib"
                       :default nil)
           :include ["pthread.h"]
           :import  [pthread_t pthread_attr_t pthread_mutex_t pthread_cond_t pthread_condattr_t
                     pthread_create pthread_equal pthread_exit pthread_join
                     pthread_self pthread_mutex_init pthread_mutex_destroy
                     pthread_mutex_lock pthread_mutex_trylock pthread_mutex_unlock
                     pthread_cond_init pthread_cond_destroy pthread_cond_wait
                     pthread_cond_timedwait pthread_cond_signal pthread_cond_broadcast
                     pthread_once pthread_key_create pthread_key_delete
                     pthread_setspecific pthread_getspecific #_pthread_cleanup_push
                     #_pthread_cleanup_pop pthread_attr_init pthread_attr_destroy
                     pthread_attr_getstacksize pthread_attr_setstacksize
                     pthread_attr_getdetachstate pthread_attr_setdetachstate
                     #_PTHREAD_THREADS_MAX #_PTHREAD_KEYS_MAX #_PTHREAD_STACK_MIN
                     PTHREAD_CREATE_DETACHED PTHREAD_CREATE_JOINABLE
                     pthread_detach]}))
    }.read("#<FFI>").eval(env).print;

    version(none)
    version (Posix)
    q{(ffi C
       {:lib     "/usr/lib/system/libsystem_kernel.dylib"
        :include ["/usr/include/sys/fcntl.h"
                  "/usr/include/sys/ioctl.h"
                  "/usr/include/sys/mman.h"
                  "/usr/include/sys/mount.h"
                  "/usr/include/sys/signal.h"
                  "/usr/include/sys/wait.h"]
        :import [O_RDONLY O_WRONLY O_RDWR O_ACCMODE FREAD FWRITE O_NONBLOCK
                 O_APPEND O_SHLOCK O_EXLOCK O_ASYNC O_FSYNC O_NOFOLLOW O_CREAT
                 O_TRUNC O_EXCL AT_FDCWD AT_EACCESS AT_SYMLINK_NOFOLLOW
                 AT_SYMLINK_FOLLOW AT_REMOVEDIR O_EVTONLY O_NOCTTY O_DIRECTORY
                 O_SYMLINK O_CLOEXEC O_DP_GETRAWENCRYPTED O_DP_GETRAWUNENCRYPTED
                 FAPPEND FASYNC FFSYNC FFDSYNC FNONBLOCK FNDELAY O_NDELAY
                 CPF_OVERWRITE CPF_IGNORE_MODE CPF_MASK F_DUPFD F_GETFD F_SETFD
                 F_GETFL F_SETFL F_GETOWN F_SETOWN F_GETLK F_SETLK F_SETLKW
                 F_SETLKWTIMEOUT F_FLUSH_DATA F_CHKCLEAN F_PREALLOCATE F_SETSIZE
                 F_RDADVISE F_RDAHEAD F_NOCACHE F_LOG2PHYS F_GETPATH F_FULLFSYNC
                 F_PATHPKG_CHECK F_FREEZE_FS F_THAW_FS F_GLOBAL_NOCACHE F_ADDSIGS
                 F_ADDFILESIGS F_NODIRECT F_GETPROTECTIONCLASS
                 F_SETPROTECTIONCLASS F_LOG2PHYS_EXT F_GETLKPID F_SETBACKINGSTORE
                 F_GETPATH_MTMINFO F_GETCODEDIR F_SETNOSIGPIPE F_GETNOSIGPIPE
                 F_SINGLE_WRITER F_GETPROTECTIONLEVEL F_FINDSIGS
                 F_ADDFILESIGS_FOR_DYLD_SIM F_BARRIERFSYNC F_ADDFILESIGS_RETURN
                 F_CHECK_LV F_PUNCHHOLE FCNTL_FS_SPECIFIC_BASE F_DUPFD_CLOEXEC
                 FD_CLOEXEC F_RDLCK F_UNLCK F_WRLCK F_ALLOCATECONTIG
                 F_ALLOCATEALL F_PEOFPOSMODE F_VOLPOSMODE flock flocktimeout
                 radvisory fcodeblobs fsignatures fchecklv LOCK_SH LOCK_EX
                 LOCK_NB LOCK_UN fstore fstore_t fpunchhole fpunchhole_t
                 ftrimactivefile ftrimactivefile_t fbootstraptransfer
                 fbootstraptransfer_t log2phys O_POPUP O_ALERT filesec_property_t
                 open openat creat fcntl openx_np open_dprotected_np flock
                 filesec_init filesec_dup filesec_free filesec_get_property
                 filesec_query_property filesec_set_property
                 filesec_unset_property _FILESEC_UNSET_PROPERTY
                 _FILESEC_REMOVE_ACL
                 ttysize TIOCGSIZE TIOCSSIZE ioctl
                 PROT_NONE PROT_READ PROT_WRITE PROT_EXEC MAP_SHARED MAP_PRIVATE
                 MAP_COPY MAP_FIXED MAP_RENAME MAP_NORESERVE MAP_RESERVED0080
                 MAP_NOEXTEND MAP_HASSEMAPHORE MAP_NOCACHE MAP_JIT MAP_FILE
                 MAP_ANON MAP_ANONYMOUS MAP_RESILIENT_CODESIGN
                 MAP_RESILIENT_MEDIA MCL_CURRENT MCL_FUTURE MAP_FAILED MS_ASYNC
                 MS_INVALIDATE MS_SYNC MS_KILLPAGES MS_DEACTIVATE
                 POSIX_MADV_NORMAL POSIX_MADV_RANDOM POSIX_MADV_SEQUENTIAL
                 POSIX_MADV_WILLNEED POSIX_MADV_DONTNEED MADV_NORMAL
                 MADV_RANDOM MADV_SEQUENTIAL MADV_WILLNEED MADV_DONTNEED
                 MADV_FREE MADV_ZERO_WIRED_PAGES MADV_FREE_REUSABLE
                 MADV_FREE_REUSE MADV_CAN_REUSE MADV_PAGEOUT MINCORE_INCORE
                 MINCORE_REFERENCED MINCORE_MODIFIED MINCORE_REFERENCED_OTHER
                 MINCORE_MODIFIED_OTHER MINCORE_PAGED_OUT MINCORE_COPIED
                 MINCORE_ANONYMOUS mlockall munlockall mlock mmap mprotect
                 msync munlock munmap shm_open shm_unlink posix_madvise madvise
                 mincore minherit
                 MFSNAMELEN MFSTYPENAMELEN MNAMELEN statfs64 statfs vfsstatfs
                 MNT_RDONLY MNT_SYNCHRONOUS MNT_NOEXEC MNT_NOSUID MNT_NODEV
                 MNT_UNION MNT_ASYNC MNT_CPROTECT MNT_EXPORTED MNT_QUARANTINE
                 MNT_LOCAL MNT_QUOTA MNT_ROOTFS MNT_DOVOLFS MNT_DONTBROWSE
                 MNT_IGNORE_OWNERSHIP MNT_AUTOMOUNTED MNT_JOURNALED
                 MNT_NOUSERXATTR MNT_MULTILABEL MNT_NOATIME MNT_SNAPSHOT
                 MNT_UNKNOWNPERMISSIONS MNT_VISFLAGMASK MNT_UPDATE MNT_NOBLOCK
                 MNT_RELOAD MNT_FORCE MNT_CMDFLAGS VFS_GENERIC VFS_NUMMNTOPS
                 VFS_MAXTYPENUM VFS_CONF MNT_WAIT MNT_NOWAIT MNT_DWAIT mount_t
                 vnode_t vfsconf vfsidctl VFS_CTL_VERS1 VFS_CTL_STATFS
                 VFS_CTL_UMOUNT VFS_CTL_QUERY VFS_CTL_NEWADDR VFS_CTL_TIMEO
                 VFS_CTL_NOLOCKS VFS_CTL_SADDR VFS_CTL_DISC VFS_CTL_SERVERINFO
                 VFS_CTL_NSTATUS vfsquery vfs_server netfs_status VQ_NOTRESP
                 VQ_NEEDAUTH VQ_LOWDISK VQ_MOUNT VQ_UNMOUNT VQ_DEAD VQ_ASSIST
                 VQ_NOTRESPLOCK VQ_UPDATE VQ_VERYLOWDISK VQ_SYNCEVENT
                 VQ_SERVEREVENT VQ_QUOTA VQ_NEARLOWDISK VQ_DESIRED_DISK
                 VQ_FLAG8000 NFS_MAX_FH_SIZE NFSV4_MAX_FH_SIZE NFSV3_MAX_FH_SIZE
                 NFSV2_MAX_FH_SIZE fhandle fhandle_t fhopen fstatfs fstatfs64
                 getfh getfsstat getfsstat64 getmntinfo getmntinfo_r_np
                 getmntinfo64 mount fmount statfs statfs64 unmount getvfsbyname
                 SIGHUP SIGINT SIGQUIT SIGILL SIGTRAP SIGABRT #_SIGPOLL SIGIOT
                 SIGEMT SIGFPE SIGKILL SIGBUS SIGSEGV SIGSYS SIGPIPE SIGALRM
                 SIGTERM SIGURG SIGSTOP SIGTSTP SIGCONT SIGCHLD SIGTTIN SIGTTOU
                 SIGIO SIGXCPU SIGXFSZ SIGVTALRM SIGPROF SIGWINCH SIGINFO SIGUSR1
                 SIGUSR2 SIG_DFL SIG_IGN SIG_HOLD SIG_ERR SIG_DFL SIG_IGN
                 SIG_HOLD SIG_ERR sigval SIGEV_NONE SIGEV_SIGNAL SIGEV_THREAD
                 sigevent siginfo_t ILL_NOOP ILL_ILLOPC ILL_ILLTRP ILL_PRVOPC
                 ILL_ILLOPN ILL_ILLADR ILL_PRVREG ILL_COPROC ILL_BADSTK FPE_NOOP
                 FPE_FLTDIV FPE_FLTOVF FPE_FLTUND FPE_FLTRES FPE_FLTINV
                 FPE_FLTSUB FPE_INTDIV FPE_INTOVF SEGV_NOOP SEGV_MAPERR
                 SEGV_ACCERR BUS_NOOP BUS_ADRALN BUS_ADRERR BUS_OBJERR TRAP_BRKPT
                 TRAP_TRACE CLD_NOOP CLD_EXITED CLD_KILLED CLD_DUMPED CLD_TRAPPED
                 CLD_STOPPED CLD_CONTINUED POLL_IN POLL_OUT POLL_MSG POLL_ERR
                 POLL_PRI POLL_HUP sigaction SA_ONSTACK SA_RESTART SA_RESETHAND
                 SA_NOCLDSTOP SA_NODEFER SA_NOCLDWAIT SA_SIGINFO SA_USERTRAMP
                 SA_64REGSET SA_USERSPACE_MASK SIG_BLOCK SIG_UNBLOCK SIG_SETMASK
                 SI_USER SI_QUEUE SI_TIMER SI_ASYNCIO SI_MESGQ sig_t SS_ONSTACK
                 SS_DISABLE MINSIGSTKSZ SIGSTKSZ sigvec SV_ONSTACK SV_INTERRUPT
                 SV_RESETHAND SV_NODEFER SV_NOCLDSTOP SV_SIGINFO
                 sigstack sigmask BADSIG signal

                 idtype_t WNOHANG WUNTRACED WCOREFLAG WEXITED WSTOPPED WCONTINUED
                 WNOWAIT WAIT_ANY WAIT_MYPGRP wait waitpid waitid wait3 wait4]}
       )}.read("posix").eval(env);

    version (none)
    version (OSX)
    q{(ffi C
       {:lib ""
        :include ["/usr/include/mach/mach.h"]
        :import [
         ]}
       )}.read("mach").eval(env);

    version(none)
    q{(ffi C
       {:flags ["-Iinc"]
        :wrap true}
       {:lib     #_"/usr/local/Cellar/zlib/1.2.11/lib/libz.dylib" "./lib/windows/x86_64/zlib.dll"
        :include [#_"/usr/local/Cellar/zlib/1.2.11/include/zlib.h"
                  "zlib.h"]
        :import  [alloc_func free_func z_stream z_streamp gz_header gz_headerp
                  ZLIB_VERSION ZLIB_VERNUM ZLIB_VER_MAJOR ZLIB_VER_MINOR
                  ZLIB_VER_REVISION ZLIB_VER_SUBREVISION
                  Z_NO_FLUSH Z_PARTIAL_FLUSH Z_SYNC_FLUSH Z_FULL_FLUSH
                  Z_FINISH Z_BLOCK Z_TREES Z_OK Z_STREAM_END Z_NEED_DICT
                  Z_ERRNO Z_STREAM_ERROR Z_DATA_ERROR Z_MEM_ERROR Z_BUF_ERROR
                  Z_VERSION_ERROR Z_NO_COMPRESSION Z_BEST_SPEED
                  Z_BEST_COMPRESSION Z_DEFAULT_COMPRESSION Z_FILTERED
                  Z_HUFFMAN_ONLY Z_RLE Z_FIXED Z_DEFAULT_STRATEGY Z_BINARY
                  Z_TEXT Z_ASCII Z_UNKNOWN Z_DEFLATED Z_NULL #_zlib_version
                  zlibVersion deflate deflateEnd inflate inflateEnd
                  deflateSetDictionary deflateGetDictionary deflateCopy
                  deflateReset deflateParams deflateTune deflateBound
                  deflatePending deflatePrime deflateSetHeader
                  inflateSetDictionary inflateGetDictionary inflateSync
                  inflateCopy inflateReset inflatePrime inflateMark
                  inflateGetHeader in_func out_func inflateBack inflateBackEnd
                  zlibCompileFlags compress compress2 compressBound uncompress
                  uncompress2 gzFile gzdopen gzbuffer gzsetparams gzread
                  gzfread gzwrite gzfwrite gzprintf gzputs gzgets gzputc #_gzgetc
                  gzungetc gzflush gzrewind gzeof gzdirect gzclose gzerror
                  gzclearerr adler32 adler32_z crc32 crc32_z deflateInit_
                  inflateInit_ deflateInit2_ inflateInit2_ inflateBackInit_]}
       )}.read("zlib").eval(env);

    version(none)
    q{(ffi C
       {:flags   ["-I/usr/local/Cellar/libxml2/2.9.7/include/libxml2"]}
       {:lib     "/usr/local/Cellar/libxml2/2.9.7/lib/libxml2.dylib"
        :include ["/usr/local/Cellar/libxml2/2.9.7/include/libxml2/libxml/HTMLparser.h"
                  "/usr/local/Cellar/libxml2/2.9.7/include/libxml2/libxml/SAX2.h"]
        :import [LIBXML_DOTTED_VERSION LIBXML_VERSION LIBXML_VERSION_STRING
                 LIBXML_VERSION_EXTRA
                 xmlChar xmlStrdup xmlStrndup xmlCharStrdup xmlStrsub xmlStrchr
                 xmlStrcasestr xmlStrcmp xmlStrncmp xmlStrcasecmp xmlStrncasecmp
                 xmlStrEqual xmlStrQEqual xmlStrlen xmlStrcat xmlStrncatNew
                 xmlStrPrintf xmlStrVPrintf xmlGetUTF8Char xmlCheckUTF8
                 xmlUTF8Strsize xmlUTF8Strndup xmlUTF8Strsub xmlUTF8Strlen
                 xmlUTF8Size xmlUTF8Charcmp
                 xmlParserInputBufferPtr xmlOutputBufferPtr xmlParserInputPtr
                 xmlParserCtxtPtr xmlSAXLocatorPtr xmlSAXHandlerPtr xmlEntityPtr
                 BASE_BUFFER_SIZE xmlBufferAllocationScheme xmlBuffer
                 xmlBufferPtr xmlBufPtr xmlBufContent xmlBufEnd xmlBufUse
                 xmlBufShrink LIBXML2_NEW_BUFFER XML_XML_NAMESPACE
                 XML_XML_ID xmlElementType xmlNotation xmlNotationPtr
                 xmlAttributeType xmlAttributeDefault xmlEnumeration
                 xmlEnumerationPtr xmlAttribute xmlAttributePtr
                 xmlElementContentType xmlElementContentOccur xmlElementContent
                 xmlElementContentPtr xmlElementTypeVal xmlElement xmlElementPtr
                 #_XML_LOCAL_NAMESPACE xmlNsType xmlNs xmlNsPtr xmlDtd xmlDtdPtr
                 xmlAttr xmlAttrPtr xmlID xmlIDPtr xmlRef xmlRefPtr xmlNode
                 xmlDocProperties xmlDoc xmlDocPtr xmlDOMWrapCtxt
                 xmlDOMWrapCtxtPtr xmlDOMWrapAcquireNsFunction
                 xmlValidateNCName xmlValidateQName xmlValidateName
                 xmlValidateNMToken xmlBuildQName xmlSplitQName2 xmlSplitQName3
                 xmlSetBufferAllocationScheme xmlGetBufferAllocationScheme
                 xmlBufferCreate xmlBufferCreateSize xmlBufferCreateStatic
                 xmlBufferResize xmlBufferFree xmlBufferDump xmlBufferAdd
                 xmlBufferAddHead xmlBufferCat xmlBufferCCat xmlBufferShrink
                 xmlBufferGrow xmlBufferEmpty xmlBufferContent xmlBufferDetach
                 xmlBufferSetAllocationScheme xmlBufferLength xmlCreateIntSubset
                 xmlNewDtd xmlGetIntSubset xmlFreeDtd xmlNewNs xmlFreeNs
                 xmlFreeNsList xmlNewDoc xmlFreeDoc xmlNewDocProp
                 xmlNewProp xmlNewNsProp xmlNewNsPropEatName xmlFreePropList
                 xmlFreeProp xmlCopyProp xmlCopyPropList xmlCopyDtd xmlCopyDoc
                 xmlNewDocNode xmlNewDocNodeEatName xmlNewNode xmlNewNodeEatName
                 xmlNewChild xmlNewDocText xmlNewText xmlNewDocPI xmlNewPI
                 xmlNewDocTextLen xmlNewTextLen xmlNewDocComment xmlNewComment
                 xmlNewCDataBlock xmlNewCharRef xmlNewReference xmlCopyNode
                 xmlDocCopyNode xmlDocCopyNodeList xmlCopyNodeList
                 xmlNewTextChild xmlNewDocRawNode xmlNewDocFragment
                 xmlGetLineNo xmlGetNodePath xmlDocGetRootElement xmlGetLastChild
                 xmlNodeIsText xmlIsBlankNode xmlDocSetRootElement xmlNodeSetName
                 xmlAddChild xmlAddChildList xmlReplaceNode xmlAddPrevSibling
                 xmlAddSibling xmlAddNextSibling xmlUnlinkNode xmlTextMerge
                 xmlTextConcat xmlFreeNodeList xmlFreeNode xmlSetTreeDoc
                 xmlSetListDoc xmlSearchNs xmlSearchNsByHref xmlGetNsList
                 xmlSetNs xmlCopyNamespace xmlCopyNamespaceList
                 xmlSetProp xmlSetNsProp xmlGetNoNsProp xmlGetProp xmlHasProp
                 xmlHasNsProp xmlGetNsProp xmlStringGetNodeList
                 xmlStringLenGetNodeList xmlNodeListGetString
                 xmlNodeListGetRawString xmlNodeSetContent xmlNodeSetContentLen
                 xmlNodeAddContent xmlNodeAddContentLen xmlNodeGetContent
                 xmlNodeBufGetContent xmlNodeBufGetContent xmlBufGetNodeContent
                 xmlNodeGetLang xmlNodeGetSpacePreserve xmlNodeSetLang
                 xmlNodeSetSpacePreserve xmlNodeGetBase xmlNodeSetBase
                 xmlRemoveProp xmlUnsetNsProp xmlUnsetProp xmlBufferWriteCHAR
                 xmlBufferWriteChar xmlBufferWriteQuotedString
                 xmlAttrSerializeTxtContent xmlReconciliateNs
                 xmlDocDumpFormatMemory xmlDocDumpMemory xmlDocDumpMemoryEnc
                 xmlDocDumpFormatMemoryEnc xmlDocFormatDump xmlDocDump
                 xmlElemDump xmlSaveFile xmlSaveFormatFile xmlBufNodeDump
                 xmlNodeDump xmlSaveFileTo xmlSaveFormatFileTo xmlNodeDumpOutput
                 xmlSaveFormatFileEnc xmlSaveFileEnc xmlIsXHTML
                 xmlGetDocCompressMode xmlSetDocCompressMode xmlGetCompressMode
                 xmlSetCompressMode xmlDOMWrapNewCtxt xmlDOMWrapFreeCtxt
                 xmlDOMWrapReconcileNamespaces xmlDOMWrapAdoptNode
                 xmlDOMWrapRemoveNode xmlDOMWrapCloneNode xmlChildElementCount
                 xmlNextElementSibling xmlFirstElementChild xmlLastElementChild
                 xmlPreviousElementSibling xmlFreeFunc xmlMallocFunc
                 xmlReallocFunc xmlStrdupFunc xmlMemSetup xmlMemGet xmlGcMemSetup
                 xmlGcMemGet xmlInitMemory xmlCleanupMemory xmlMemUsed
                 xmlMemBlocks xmlMemDisplay xmlMemDisplayLast xmlMemShow
                 xmlMemoryDump xmlMemMalloc xmlMemRealloc xmlMemFree
                 xmlMemoryStrdup xmlMallocLoc xmlReallocLoc xmlMallocAtomicLoc
                 xmlMemStrdupLoc xmlDictPtr xmlInitializeDict xmlDictCreate
                 xmlDictSetLimit xmlDictGetUsage xmlDictCreateSub
                 xmlDictReference xmlDictFree xmlDictLookup xmlDictExists
                 xmlDictQLookup xmlDictOwns xmlDictSize xmlDictCleanup
                 xmlRegexp xmlRegexpPtr xmlRegExecCtxt xmlRegExecCtxtPtr
                 xmlRegexpCompile xmlRegexpExec xmlRegexpPrint
                 xmlRegexpIsDeterminist xmlRegExecCallbacks xmlRegNewExecCtxt
                 xmlRegFreeExecCtxt xmlRegExecPushString xmlRegExecPushString2
                 xmlRegExecNextValues xmlRegExecErrInfo xmlExpCtxtPtr
                 xmlExpFreeCtxt xmlExpNewCtxt xmlExpCtxtNbNodes xmlExpCtxtNbCons
                 xmlExpNodePtr xmlExpNodeType forbiddenExp emptyExp xmlExpFree
                 xmlExpRef xmlExpParse xmlExpNewAtom xmlExpNewOr xmlExpNewSeq
                 xmlExpNewRange xmlExpIsNillable xmlExpMaxToken xmlExpGetLanguage
                 xmlExpGetStart xmlExpStringDerive xmlExpExpDerive xmlExpSubsume
                 xmlExpDump xmlHashTablePtr xmlHashDeallocator xmlHashCopier
                 xmlHashScanner xmlHashScannerFull xmlHashCreate
                 xmlHashCreateDict xmlHashFree xmlHashAddEntry xmlHashUpdateEntry
                 xmlHashAddEntry2 xmlHashUpdateEntry2 xmlHashAddEntry3
                 xmlHashUpdateEntry3 xmlHashRemoveEntry xmlHashRemoveEntry2
                 xmlHashRemoveEntry3 xmlHashLookup xmlHashLookup2 xmlHashLookup3
                 xmlHashQLookup xmlHashQLookup2 xmlHashQLookup3 xmlHashCopy
                 xmlHashSize xmlHashScan xmlHashScan3 xmlHashScanFull
                 xmlHashScanFull3 xmlValidStatePtr xmlValidityErrorFunc
                 xmlValidityWarningFunc xmlValidCtxt xmlValidCtxtPtr
                 xmlNotationTablePtr xmlElementTablePtr xmlAttributeTablePtr
                 xmlIDTablePtr xmlRefTablePtr xmlAddNotationDecl
                 xmlCopyNotationTable xmlFreeNotationTable xmlDumpNotationDecl
                 xmlDumpNotationTable xmlNewElementContent xmlCopyElementContent
                 xmlFreeElementContent xmlNewDocElementContent
                 xmlCopyDocElementContent xmlFreeDocElementContent
                 xmlSnprintfElementContent xmlSprintfElementContent
                 xmlAddElementDecl xmlCopyElementTable xmlFreeElementTable
                 xmlDumpElementTable xmlDumpElementDecl xmlCreateEnumeration
                 xmlFreeEnumeration xmlCopyEnumeration xmlAddAttributeDecl
                 xmlCopyAttributeTable xmlFreeAttributeTable
                 xmlDumpAttributeTable xmlDumpAttributeDecl xmlAddID
                 xmlFreeIDTable xmlIsID xmlRemoveID xmlAddRef xmlFreeRefTable
                 xmlIsRef xmlRemoveRef xmlGetRefs xmlNewValidCtxt
                 xmlFreeValidCtxt xmlValidateRoot xmlValidateElementDecl
                 xmlValidNormalizeAttributeValue
                 xmlValidCtxtNormalizeAttributeValue xmlValidateAttributeDecl
                 xmlValidateAttributeValue xmlValidateNotationDecl xmlValidateDtd
                 xmlValidateDtdFinal xmlValidateDocument xmlValidateElement
                 xmlValidateOneElement xmlValidateOneAttribute
                 xmlValidateOneNamespace xmlValidateDocumentFinal
                 xmlValidateNotationUse xmlIsMixedElement xmlGetDtdAttrDesc
                 xmlGetDtdQAttrDesc xmlGetDtdNotationDesc xmlGetDtdQElementDesc
                 xmlGetDtdElementDesc xmlValidGetPotentialChildren
                 xmlValidGetValidElements xmlValidateNameValue
                 xmlValidateNamesValue xmlValidateNmtokenValue
                 xmlValidateNmtokensValue xmlValidBuildContentModel
                 xmlValidatePushElement xmlValidatePushCData
                 xmlValidatePopElement xmlEntityType xmlEntity
                 xmlEntitiesTablePtr xmlNewEntity xmlAddDocEntity xmlAddDtdEntity
                 xmlGetPredefinedEntity xmlGetDocEntity xmlGetDtdEntity
                 xmlGetParameterEntity xmlEncodeEntitiesReentrant
                 xmlEncodeSpecialChars xmlCreateEntitiesTable
                 xmlCopyEntitiesTable xmlFreeEntitiesTable xmlDumpEntitiesTable
                 xmlDumpEntityDecl xmlErrorLevel xmlErrorDomain xmlError
                 xmlErrorPtr xmlParserErrors xmlGenericErrorFunc
                 xmlStructuredErrorFunc xmlSetGenericErrorFunc
                 initGenericErrorDefaultFunc xmlSetStructuredErrorFunc
                 xmlParserError xmlParserWarning xmlParserValidityError
                 xmlParserValidityWarning xmlParserPrintFileInfo
                 xmlParserPrintFileContext xmlGetLastError xmlResetLastError
                 xmlCtxtGetLastError xmlCtxtResetLastError xmlResetError
                 xmlCopyError XML_DEFAULT_VERSION xmlParserInputDeallocate
                 xmlParserInput xmlParserNodeInfo xmlParserNodeInfoPtr
                 xmlParserNodeInfoSeq xmlParserNodeInfoSeqPtr xmlParserInputState
                 XML_DETECT_IDS XML_COMPLETE_ATTRS XML_SKIP_IDS xmlParserMode
                 xmlParserCtxt xmlSAXLocator resolveEntitySAXFunc
                 internalSubsetSAXFunc externalSubsetSAXFunc getEntitySAXFunc
                 getParameterEntitySAXFunc entityDeclSAXFunc notationDeclSAXFunc
                 attributeDeclSAXFunc elementDeclSAXFunc
                 unparsedEntityDeclSAXFunc setDocumentLocatorSAXFunc
                 startDocumentSAXFunc endDocumentSAXFunc startElementSAXFunc
                 endElementSAXFunc attributeSAXFunc referenceSAXFunc
                 charactersSAXFunc ignorableWhitespaceSAXFunc
                 processingInstructionSAXFunc commentSAXFunc cdataBlockSAXFunc
                 warningSAXFunc errorSAXFunc fatalErrorSAXFunc
                 isStandaloneSAXFunc hasInternalSubsetSAXFunc
                 hasExternalSubsetSAXFunc XML_SAX2_MAGIC
                 startElementNsSAX2Func endElementNsSAX2Func xmlSAXHandler
                 xmlSAXHandlerV1 xmlSAXHandlerV1Ptr xmlExternalEntityLoader
                 xmlCharEncoding xmlCharEncodingInputFunc
                 xmlCharEncodingOutputFunc xmlCharEncodingHandler
                 xmlCharEncodingHandlerPtr xmlInitCharEncodingHandlers
                 xmlCleanupCharEncodingHandlers xmlRegisterCharEncodingHandler
                 xmlGetCharEncodingHandler xmlFindCharEncodingHandler
                 xmlNewCharEncodingHandler xmlAddEncodingAlias
                 xmlDelEncodingAlias xmlGetEncodingAlias
                 xmlCleanupEncodingAliases xmlParseCharEncoding
                 xmlGetCharEncodingName xmlDetectCharEncoding xmlCharEncOutFunc
                 xmlCharEncInFunc xmlCharEncFirstLine xmlCharEncCloseFunc
                 UTF8Toisolat1 isolat1ToUTF8 xmlInputMatchCallback
                 xmlInputOpenCallback xmlInputReadCallback xmlInputCloseCallback
                 xmlOutputMatchCallback xmlOutputOpenCallback
                 xmlOutputWriteCallback xmlOutputCloseCallback
                 xmlParserInputBuffer xmlOutputBuffer xmlCleanupInputCallbacks
                 xmlPopInputCallbacks xmlRegisterDefaultInputCallbacks
                 xmlAllocParserInputBuffer xmlParserInputBufferCreateFilename
                 xmlParserInputBufferCreateFile xmlParserInputBufferCreateFd
                 xmlParserInputBufferCreateMem xmlParserInputBufferCreateStatic
                 xmlParserInputBufferCreateIO xmlParserInputBufferRead
                 xmlParserInputBufferGrow xmlParserInputBufferPush
                 xmlFreeParserInputBuffer xmlParserGetDirectory
                 xmlRegisterInputCallbacks xmlCleanupOutputCallbacks
                 xmlRegisterDefaultOutputCallbacks xmlAllocOutputBuffer
                 xmlOutputBufferCreateFilename xmlOutputBufferCreateFile
                 xmlOutputBufferCreateBuffer xmlOutputBufferCreateFd
                 xmlOutputBufferCreateIO xmlOutputBufferGetContent
                 xmlOutputBufferGetSize xmlOutputBufferWrite
                 xmlOutputBufferWriteString xmlOutputBufferWriteEscape
                 xmlOutputBufferFlush xmlOutputBufferClose
                 xmlRegisterOutputCallbacks xmlRegisterHTTPPostCallbacks
                 xmlCheckHTTPInput xmlNoNetExternalEntityLoader
                 xmlNormalizeWindowsPath xmlCheckFilename xmlFileMatch
                 xmlFileOpen xmlFileRead xmlFileClose xmlIOHTTPMatch
                 xmlIOHTTPOpen xmlIOHTTPOpenW xmlIOHTTPRead xmlIOHTTPClose
                 xmlIOFTPMatch xmlIOFTPOpen xmlIOFTPRead xmlIOFTPClose
                 xmlInitGlobals xmlCleanupGlobals
                 xmlParserInputBufferCreateFilenameFunc
                 xmlOutputBufferCreateFilenameFunc
                 xmlParserInputBufferCreateFilenameDefault
                 xmlOutputBufferCreateFilenameDefault
                 xmlRegisterNodeFunc xmlDeregisterNodeFunc xmlGlobalState
                 xmlGlobalStatePtr xmlInitializeGlobalState
                 xmlThrDefSetGenericErrorFunc xmlThrDefSetStructuredErrorFunc
                 xmlRegisterNodeDefault xmlThrDefRegisterNodeDefault
                 xmlDeregisterNodeDefault xmlThrDefDeregisterNodeDefault
                 xmlThrDefOutputBufferCreateFilenameDefault
                 xmlThrDefParserInputBufferCreateFilenameDefault
                 xmlInitParser xmlParserInputGrow xmlParseDoc xmlParseFile
                 xmlParseMemory xmlSubstituteEntitiesDefault xmlKeepBlanksDefault
                 xmlStopParser xmlPedanticParserDefault xmlLineNumbersDefault
                 xmlRecoverDoc xmlRecoverMemory xmlRecoverFile xmlParseDocument
                 xmlParseExtParsedEnt

                 xmlParserOption xmlCtxtReset xmlCtxtResetPush xmlCtxtUseOptions
                 xmlReadDoc xmlReadFile xmlReadMemory xmlReadFd xmlReadIO
                 xmlCtxtReadDoc xmlCtxtReadFile xmlCtxtReadMemory xmlCtxtReadFd
                 xmlCtxtReadIO xmlFeature xmlHasFeature
                 ]}
       )}.read("xml").eval(env);

    version(none)
    q{(ffi C
       {:flags ["-DCURL_NO_OLDIES" "-Iinc"] :wrap true}
       {:lib "lib/windows/x86_64/libcurl-x64.dll"
        :include ["curl/curl.h"]
        :import [CURL CURLSH curl_socket_t CURL_SOCKET_BAD

                 curl_httppost

                 curl_progress_callback curl_xferinfo_callback
                 CURL_MAX_READ_SIZE CURL_MAX_WRITE_SIZE
                 CURL_MAX_HTTP_HEADER CURL_WRITEFUNC_PAUSE
                 curl_write_callback curl_resolver_start_callback
                 curlfiletype

                 curl_fileinfo CURL_CHUNK_BGN_FUNC_OK
                 CURL_CHUNK_BGN_FUNC_FAIL CURL_CHUNK_BGN_FUNC_SKIP
                 curl_chunk_bgn_callback CURL_CHUNK_END_FUNC_OK
                 CURL_CHUNK_END_FUNC_FAIL curl_chunk_end_callback
                 CURL_FNMATCHFUNC_MATCH CURL_FNMATCHFUNC_NOMATCH
                 CURL_FNMATCHFUNC_FAIL curl_fnmatch_callback
                 CURL_SEEKFUNC_OK CURL_SEEKFUNC_FAIL CURL_SEEKFUNC_CANTSEEK
                 curl_seek_callback CURL_READFUNC_ABORT CURL_READFUNC_PAUSE
                 curl_read_callback curlsocktype CURL_SOCKOPT_OK
                 CURL_SOCKOPT_ERROR CURL_SOCKOPT_ALREADY_CONNECTED
                 curl_sockopt_callback curl_sockaddr curl_opensocket_callback
                 curl_closesocket_callback curlioerr curliocmd
                 curl_ioctl_callback curl_malloc_callback curl_free_callback
                 curl_realloc_callback curl_strdup_callback curl_calloc_callback
                 curl_infotype curl_debug_callback CURLcode
                 curl_conv_callback curl_ssl_ctx_callback curl_proxytype

                 curl_khtype curl_khkey curl_khstat curl_khmatch
                 curl_sshkeycallback curl_usessl CURLSSLOPT_ALLOW_BEAST
                 CURLSSLOPT_NO_REVOKE #_CURLSSLOPT_HET_DEFAULT curl_ftpccc
                 curl_ftpauth curl_ftpcreatedir curl_ftpmethod

                 CURLoption

                 CURL_NETRC_OPTION

                 CURL_TLSAUTH

                 curl_TimeCond CURL_ZERO_TERMINATED curl_strequal curl_strnequal
                 curl_mime curl_mimepart curl_mime_init curl_mime_free
                 curl_mime_addpart curl_mime_name curl_mime_filename
                 curl_mime_type curl_mime_encoder curl_mime_data
                 curl_mime_filedata curl_mime_data_cb curl_mime_subparts
                 curl_mime_headers CURLformoption curl_forms CURLFORMcode
                 curl_formadd curl_formget_callback curl_formget curl_formfree
                 curl_getenv curl_version curl_easy_escape curl_escape
                 curl_easy_unescape curl_unescape curl_free curl_global_init
                 curl_global_init_mem curl_global_cleanup curl_slist
                 curl_ssl_backend CURLsslset curl_global_sslset curl_slist_append
                 curl_slist_free_all curl_getdate curl_certinfo
                 curl_tlssessioninfo

                 CURLINFO #_CURLINFO_HTTP_CODE curl_closepolicy

                 curl_lock_data curl_lock_access curl_lock_function
                 curl_unlock_function CURLSHcode CURLSHoption curl_share_init
                 curl_share_setopt curl_share_cleanup CURLversion
                 #_CURLVERSION_NOW
                 curl_version_info_data

                 curl_version_info curl_easy_strerror curl_share_strerror
                 curl_easy_pause CURLPAUSE_RECV CURLPAUSE_RECV_CONT
                 CURLPAUSE_SEND CURLPAUSE_SEND_CONT CURLPAUSE_ALL CURLPAUSE_CONT]}
       )}.read("curl").eval(env);

    version (none)
    q{(ffi C
       {:wrap true}
       {:lib     "/usr/local/lib/libtermbox.dylib"
        :include ["/usr/local/include/termbox.h"]
        :import  [TB_KEY_F1 TB_KEY_F2 TB_KEY_F3 TB_KEY_F4 TB_KEY_F5 TB_KEY_F6
                  TB_KEY_F7 TB_KEY_F8 TB_KEY_F9 TB_KEY_F10 TB_KEY_F11 TB_KEY_F12
                  TB_KEY_INSERT TB_KEY_DELETE TB_KEY_HOME TB_KEY_END TB_KEY_PGUP
                  TB_KEY_PGDN TB_KEY_ARROW_UP TB_KEY_ARROW_DOWN TB_KEY_ARROW_LEFT
                  TB_KEY_ARROW_RIGHT TB_KEY_MOUSE_LEFT TB_KEY_MOUSE_RIGHT
                  TB_KEY_MOUSE_MIDDLE TB_KEY_MOUSE_RELEASE TB_KEY_MOUSE_WHEEL_UP
                  TB_KEY_MOUSE_WHEEL_DOWN TB_KEY_CTRL_TILDE TB_KEY_CTRL_2
                  TB_KEY_CTRL_A TB_KEY_CTRL_B TB_KEY_CTRL_C TB_KEY_CTRL_D
                  TB_KEY_CTRL_E TB_KEY_CTRL_F TB_KEY_CTRL_G TB_KEY_BACKSPACE
                  TB_KEY_CTRL_H TB_KEY_TAB TB_KEY_CTRL_I TB_KEY_CTRL_J
                  TB_KEY_CTRL_K TB_KEY_CTRL_L TB_KEY_ENTER TB_KEY_CTRL_M
                  TB_KEY_CTRL_N TB_KEY_CTRL_O TB_KEY_CTRL_P TB_KEY_CTRL_Q
                  TB_KEY_CTRL_R TB_KEY_CTRL_S TB_KEY_CTRL_T TB_KEY_CTRL_U
                  TB_KEY_CTRL_V TB_KEY_CTRL_W TB_KEY_CTRL_X TB_KEY_CTRL_Y
                  TB_KEY_CTRL_Z TB_KEY_ESC TB_KEY_CTRL_LSQ_BRACKET
                  TB_KEY_CTRL_3 TB_KEY_CTRL_4 TB_KEY_CTRL_BACKSLASH TB_KEY_CTRL_5
                  TB_KEY_CTRL_RSQ_BRACKET TB_KEY_CTRL_6 TB_KEY_CTRL_7
                  TB_KEY_CTRL_SLASH TB_KEY_CTRL_UNDERSCORE TB_KEY_SPACE
                  TB_KEY_BACKSPACE2 TB_KEY_CTRL_8 TB_MOD_ALT TB_MOD_MOTION
                  TB_DEFAULT TB_BLACK TB_RED TB_GREEN TB_YELLOW TB_BLUE
                  TB_MAGENTA TB_CYAN TB_WHITE TB_BOLD TB_UNDERLINE TB_REVERSE
                  tb_cell TB_EVENT_KEY TB_EVENT_RESIZE TB_EVENT_MOUSE
                  tb_event TB_EUNSUPPORTED_TERMINAL TB_EFAILED_TO_OPEN_TTY
                  TB_EPIPE_TRAP_ERROR tb_init tb_init_file tb_init_fd
                  tb_shutdown tb_width tb_height tb_clear tb_set_clear_attributes
                  tb_present TB_HIDE_CURSOR tb_set_cursor tb_put_cell
                  tb_change_cell tb_blit tb_cell_buffer TB_INPUT_CURRENT
                  TB_INPUT_ESC TB_INPUT_ALT TB_INPUT_MOUSE tb_select_input_mode
                  TB_OUTPUT_CURRENT TB_OUTPUT_NORMAL TB_OUTPUT_256 TB_OUTPUT_216
                  TB_OUTPUT_GRAYSCALE tb_select_output_mode tb_peek_event TB_EOF
                  tb_poll_event tb_utf8_char_length tb_utf8_char_to_unicode
                  tb_utf8_unicode_to_char
                  ]}
       )}.read("termbox").eval(env);

    version(none)
    q{(ffi C
       {:lib     "/usr/local/lib/libfontconfig.dylib"
        :include ["/usr/local/include/fontconfig/fontconfig.h"]
        :import  [FcChar8 FcChar16 FcChar32 FcBool FcFalse FcTrue FcDontCare
                  FC_FAMILY FC_STYLE FC_SLANT FC_WEIGHT FC_SIZE FC_ASPECT
                  FC_PIXEL_SIZE FC_SPACING FC_FOUNDRY FC_ANTIALIAS FC_HINTING
                  FC_HINT_STYLE FC_VERTICAL_LAYOUT FC_AUTOHINT FC_GLOBAL_ADVANCE
                  FC_WIDTH FC_FILE FC_INDEX FC_FT_FACE FC_RASTERIZER FC_OUTLINE
                  FC_SCALABLE FC_COLOR FC_VARIABLE FC_SCALE FC_SYMBOL FC_DPI
                  FC_RGBA FC_MINSPACE FC_SOURCE FC_CHARSET FC_LANG FC_FONTVERSION
                  FC_FULLNAME FC_FAMILYLANG FC_STYLELANG FC_FULLNAMELANG
                  FC_CAPABILITY FC_FONTFORMAT FC_EMBOLDEN FC_EMBEDDED_BITMAP
                  FC_DECORATIVE FC_LCD_FILTER FC_FONT_FEATURES FC_FONT_VARIATIONS
                  FC_NAMELANG FC_PRGNAME FC_HASH FC_POSTSCRIPT_NAME
                  FC_CACHE_SUFFIX FC_DIR_CACHE_FILE FC_USER_CACHE_FILE
                  FC_CHARWIDTH FC_CHAR_WIDTH FC_CHAR_HEIGHT FC_MATRIX
                  FC_WEIGHT_THIN FC_WEIGHT_EXTRALIGHT FC_WEIGHT_ULTRALIGHT
                  FC_WEIGHT_LIGHT FC_WEIGHT_DEMILIGHT FC_WEIGHT_SEMILIGHT
                  FC_WEIGHT_BOOK FC_WEIGHT_REGULAR FC_WEIGHT_NORMAL
                  FC_WEIGHT_MEDIUM FC_WEIGHT_DEMIBOLD FC_WEIGHT_SEMIBOLD
                  FC_WEIGHT_BOLD FC_WEIGHT_EXTRABOLD FC_WEIGHT_ULTRABOLD
                  FC_WEIGHT_BLACK FC_WEIGHT_HEAVY FC_WEIGHT_EXTRABLACK
                  FC_WEIGHT_ULTRABLACK FC_SLANT_ROMAN FC_SLANT_ITALIC
                  FC_SLANT_OBLIQUE FC_WIDTH_ULTRACONDENSED
                  FC_WIDTH_EXTRACONDENSED FC_WIDTH_CONDENSED
                  FC_WIDTH_SEMICONDENSED FC_WIDTH_NORMAL FC_WIDTH_SEMIEXPANDED
                  FC_WIDTH_EXPANDED FC_WIDTH_EXTRAEXPANDED FC_WIDTH_ULTRAEXPANDED
                  FC_PROPORTIONAL FC_DUAL FC_MONO FC_CHARCELL FC_RGBA_UNKNOWN
                  FC_RGBA_RGB FC_RGBA_BGR FC_RGBA_VRGB FC_RGBA_VBGR FC_RGBA_NONE
                  FC_HINT_NONE FC_HINT_SLIGHT FC_HINT_MEDIUM FC_HINT_FULL
                  FC_LCD_NONE FC_LCD_DEFAULT FC_LCD_LIGHT FC_LCD_LEGACY
                  FcType FcMatrix FcMatrixInit FcCharSet FcObjectType FcConstant
                  FcResult FcValueBinding FcPattern FcLangSet FcRange FcValue
                  FcFontSet FcObjectSet FcMatchKind FcLangResult FcSetName
                  FcConfigFileInfoIter FcAtomic FcEndian FcConfig FcFileCache
                  FcBlanks FcStrList FcStrSet FcCache FcBlanksCreate
                  FcBlanksDestroy FcBlanksAdd FcBlanksIsMember FcCacheDir
                  FcCacheCopySet FcCacheSubdir FcCacheNumSubdir FcCacheNumFont
                  FcDirCacheUnlink FcDirCacheValid FcDirCacheClean
                  FcCacheCreateTagFile FcDirCacheCreateUUID FcConfigHome
                  FcConfigCreate FcConfigReference FcConfigDestroy
                  FcConfigSetCurrent FcConfigGetCurrent FcConfigUptoDate
                  FcConfigBuildFonts FcConfigGetFontDirs FcConfigGetConfigDirs
                  FcConfigGetConfigFiles FcConfigGetCache FcConfigGetBlanks
                  FcConfigGetCacheDirs FcConfigGetRescanInterval
                  FcConfigSetRescanInterval FcConfigGetFonts
                  FcConfigAppFontAddFile FcConfigAppFontAddDir
                  FcConfigAppFontClear FcConfigSubstituteWithPat
                  FcConfigSubstitute FcConfigGetSysRoot FcConfigSetSysRoot
                  FcConfigFileInfoIterInit FcConfigFileInfoIterNext
                  FcConfigFileInfoIterGet FcCharSetCreate FcCharSetNew
                  FcCharSetDestroy FcCharSetAddChar FcCharSetDelChar
                  FcCharSetCopy FcCharSetEqual FcCharSetIntersect FcCharSetUnion
                  FcCharSetSubtract FcCharSetMerge FcCharSetHasChar
                  FcCharSetCount FcCharSetIntersectCount FcCharSetSubtractCount
                  FcCharSetIsSubset FC_CHARSET_MAP_SIZE FC_CHARSET_DONE
                  FcCharSetFirstPage FcCharSetNextPage FcCharSetCoverage
                  FcValuePrint FcPatternPrint FcFontSetPrint FcGetDefaultLangs
                  FcDefaultSubstitute FcFileIsDir FcFileScan FcDirScan FcDirSave
                  FcDirCacheLoad FcDirCacheRescan FcDirCacheRead
                  FcDirCacheLoadFile FcDirCacheUnload FcFreeTypeQuery
                  FcFreeTypeQueryAll FcFontSetCreate FcFontSetDestroy
                  FcFontSetAdd FcInitLoadConfig FcInitLoadConfigAndFonts FcInit
                  FcFini FcGetVersion FcInitReinitialize FcInitBringUptoDate
                  FcGetLangs FcLangNormalize FcLangGetCharSet FcLangSetCreate
                  FcLangSetDestroy FcLangSetCopy FcLangSetAdd FcLangSetDel
                  FcLangSetHasLang FcLangSetCompare FcLangSetContains
                  FcLangSetEqual FcLangSetHash FcLangSetGetLangs FcLangSetUnion
                  FcLangSetSubtract FcObjectSetCreate FcObjectSetAdd
                  FcObjectSetVaBuild FcObjectSetBuild FcFontSetList FcFontList
                  FcAtomicCreate FcAtomicLock FcAtomicNewFile FcAtomicOrigFile
                  FcAtomicReplaceOrig FcAtomicDeleteNew FcAtomicUnlock
                  FcAtomicDestroy FcFontSetMatch FcFontMatch FcFontRenderPrepare
                  FcFontSetSort FcFontSort FcFontSetSortDestroy FcMatrixCopy
                  FcMatrixEqual FcMatrixMultiply FcMatrixRotate FcMatrixScale
                  FcMatrixShear FcNameRegisterObjectTypes
                  FcNameUnregisterObjectTypes FcNameGetObjectType
                  FcNameRegisterConstants FcNameUnregisterConstants
                  FcNameGetConstant FcNameConstant FcNameParse FcNameUnparse
                  FcPatternCreate FcPatternDuplicate FcPatternReference
                  FcPatternFilter FcValueDestroy FcValueEqual FcValueSave
                  FcPatternDestroy FcPatternEqual FcPatternEqualSubset
                  FcPatternHash FcPatternAdd FcPatternAddWeak FcPatternGet
                  FcPatternGetWithBinding FcPatternDel FcPatternRemove
                  FcPatternAddInteger FcPatternAddDouble FcPatternAddString
                  FcPatternAddMatrix FcPatternAddCharSet FcPatternAddBool
                  FcPatternAddLangSet FcPatternAddRange FcPatternGetDouble
                  FcPatternGetString FcPatternGetMatrix FcPatternGetCharSet
                  FcPatternGetBool FcPatternGetLangSet FcPatternGetRange
                  FcPatternVaBuild FcPatternBuild FcPatternFormat
                  FcRangeCreateDouble FcRangeCreateInteger FcRangeDestroy
                  FcRangeCopy FcRangeGetDouble FcWeightFromOpenType
                  FcWeightFromOpenTypeDouble FcWeightToOpenType
                  FcWeightToOpenTypeDouble FcStrCopy FcStrCopyFilename
                  FcStrPlus FcStrFree FcIsUpper FcIsLower FcToLower
                  FcStrDowncase FcStrCmpIgnoreCase FcStrCmp FcStrStrIgnoreCase
                  FcStrStr FcUtf8ToUcs4 FcUtf8Len FC_UTF8_MAX_LEN FcUcs4ToUtf8
                  FcUtf16ToUcs4 FcUtf16Len FcStrDirname FcStrBasename
                  FcStrSetCreate FcStrSetMember FcStrSetEqual FcStrSetAdd
                  FcStrSetAddFilename FcStrSetDel FcStrSetDestroy FcStrListCreate
                  FcStrListFirst FcStrListNext FcStrListDone FcConfigParseAndLoad
                  FcConfigParseAndLoadFromMemory]}
       )}.read("fontconfig").eval(env);

    version(none)
    q{(ffi C
       {:lib     "/usr/local/lib/libfreeimage.dylib"
        :include ["/usr/local/include/FreeImage.h"]
        :import  [FIBITMAP FIMULTIBITMAP
                  FIRGB16 FIRGBA16 FIRGBF FIRGBAF FICOMPLEX FI_RGBA_RED
                  FI_RGBA_GREEN FI_RGBA_BLUE FI_RGBA_ALPHA FI_RGBA_RED_MASK
                  FI_RGBA_GREEN_MASK FI_RGBA_BLUE_MASK FI_RGBA_ALPHA_MASK
                  FI_RGBA_RED_SHIFT FI_RGBA_GREEN_SHIFT FI_RGBA_BLUE_SHIFT
                  FI_RGBA_ALPHA_SHIFT FI16_555_RED_MASK FI16_555_GREEN_MASK
                  FI16_555_BLUE_MASK FI16_555_RED_SHIFT FI16_555_GREEN_SHIFT
                  FI16_555_BLUE_SHIFT FI16_565_RED_MASK FI16_565_GREEN_MASK
                  FI16_565_BLUE_MASK FI16_565_RED_SHIFT FI16_565_GREEN_SHIFT
                  FI16_565_BLUE_SHIFT FIICC_DEFAULT FIICC_COLOR_IS_CMYK
                  FIICCPROFILE FREE_IMAGE_FORMAT FREE_IMAGE_TYPE
                  FREE_IMAGE_COLOR_TYPE FREE_IMAGE_QUANTIZE FREE_IMAGE_DITHER
                  FREE_IMAGE_JPEG_OPERATION FREE_IMAGE_TMO FREE_IMAGE_FILTER
                  FREE_IMAGE_COLOR_CHANNEL FREE_IMAGE_MDTYPE FREE_IMAGE_MDMODEL
                  FIMETADATA FITAG FI_ReadProc FI_WriteProc FI_SeekProc
                  FI_TellProc FreeImageIO FIMEMORY FI_FormatProc
                  FI_DescriptionProc FI_ExtensionListProc FI_RegExprProc
                  FI_OpenProc FI_CloseProc FI_PageCountProc FI_PageCapabilityProc
                  FI_LoadProc FI_SaveProc FI_ValidateProc
                  FI_SupportsExportBPPProc FI_SupportsExportTypeProc
                  FI_SupportsICCProfilesProc FI_SupportsNoPixelsProc Plugin
                  FI_InitProc FIF_LOAD_NOPIXELS BMP_DEFAULT BMP_SAVE_RLE
                  CUT_DEFAULT DDS_DEFAULT EXR_DEFAULT EXR_FLOAT EXR_NONE EXR_ZIP
                  EXR_PXR24 EXR_LC FAXG3_DEFAULT GIF_DEFAULT GIF_LOAD256
                  GIF_PLAYBACK HDR_DEFAULT ICO_DEFAULT ICO_MAKEALPHA IFF_DEFAULT
                  J2K_DEFAULT JP2_DEFAULT JPEG_DEFAULT JPEG_FAST JPEG_ACCURATE
                  JPEG_CMYK JPEG_EXIFROTATE JPEG_GREYSCALE JPEG_QUALITYSUPERB
                  JPEG_QUALITYGOOD JPEG_QUALITYNORMAL JPEG_QUALITYAVERAGE
                  JPEG_QUALITYBAD JPEG_PROGRESSIVE JPEG_SUBSAMPLING_411
                  JPEG_SUBSAMPLING_422 JPEG_SUBSAMPLING_444 JPEG_OPTIMIZE
                  JPEG_BASELINE KOALA_DEFAULT LBM_DEFAULT MNG_DEFAULT PCD_DEFAULT
                  PCD_BASE PCD_BASEDIV4 PCD_BASEDIV16 PCX_DEFAULT PFM_DEFAULT
                  PICT_DEFAULT PNG_DEFAULT PNG_IGNOREGAMMA PNG_Z_BEST_SPEED
                  PNG_Z_DEFAULT_COMPRESSION PNG_Z_BEST_COMPRESSION
                  PNG_Z_NO_COMPRESSION PNG_INTERLACED PNM_DEFAULT PNM_SAVE_RAW
                  PNM_SAVE_ASCII PSD_DEFAULT PSD_CMYK PSD_LAB PSD_NONE PSD_RLE
                  PSD_PSB RAS_DEFAULT RAW_DEFAULT RAW_PREVIEW RAW_DISPLAY
                  RAW_HALFSIZE RAW_UNPROCESSED SGI_DEFAULT TARGA_DEFAULT
                  TARGA_LOAD_RGB888 TARGA_SAVE_RLE TIFF_DEFAULT TIFF_CMYK
                  TIFF_PACKBITS TIFF_DEFLATE TIFF_ADOBE_DEFLATE TIFF_NONE
                  TIFF_CCITTFAX3 TIFF_CCITTFAX4 TIFF_LZW TIFF_JPEG TIFF_LOGLUV
                  WBMP_DEFAULT XBM_DEFAULT XPM_DEFAULT WEBP_DEFAULT WEBP_LOSSLESS
                  JXR_DEFAULT JXR_LOSSLESS JXR_PROGRESSIVE FI_COLOR_IS_RGB_COLOR
                  FI_COLOR_IS_RGBA_COLOR FI_COLOR_FIND_EQUAL_COLOR
                  FI_COLOR_ALPHA_IS_INDEX FI_COLOR_PALETTE_SEARCH_MASK
                  FI_RESCALE_DEFAULT FI_RESCALE_TRUE_COLOR
                  FI_RESCALE_OMIT_METADATA FreeImage_Initialise
                  FreeImage_DeInitialise FreeImage_GetVersion
                  FreeImage_GetCopyrightMessage FreeImage_OutputMessageFunction
                  FreeImage_OutputMessageFunctionStdCall
                  FreeImage_SetOutputMessageStdCall
                  FreeImage_SetOutputMessage FreeImage_OutputMessageProc
                  FreeImage_Allocate FreeImage_AllocateT FreeImage_Clone
                  FreeImage_Unload FreeImage_Load FreeImage_LoadU
                  FreeImage_LoadFromHandle FreeImage_Save FreeImage_SaveU
                  FreeImage_SaveToHandle FreeImage_OpenMemory
                  FreeImage_CloseMemory FreeImage_LoadFromMemory
                  FreeImage_SaveToMemory FreeImage_TellMemory
                  FreeImage_SeekMemory FreeImage_AcquireMemory
                  FreeImage_ReadMemory FreeImage_WriteMemory
                  FreeImage_LoadMultiBitmapFromMemory
                  FreeImage_SaveMultiBitmapToMemory

                  FreeImage_GetFileType FreeImage_GetFileTypeU
                  FreeImage_GetFileTypeFromHandle FreeImage_GetFileTypeFromMemory
                  FreeImage_Validate FreeImage_ValidateU
                  FreeImage_ValidateFromHandle FreeImage_ValidateFromMemory
                  FreeImage_GetImageType FreeImage_IsLittleEndian
                  FreeImage_LookupX11Color FreeImage_LookupSVGColor
                  FreeImage_GetBits FreeImage_GetScanLine FreeImage_GetPixelIndex
                  FreeImage_GetPixelColor FreeImage_SetPixelIndex
                  FreeImage_SetPixelColor FreeImage_GetColorsUsed
                  FreeImage_GetBPP FreeImage_GetWidth FreeImage_GetHeight
                  FreeImage_GetLine FreeImage_GetPitch FreeImage_GetDIBSize
                  FreeImage_GetMemorySize FreeImage_GetPalette
                  FreeImage_GetDotsPerMeterX FreeImage_GetDotsPerMeterY
                  FreeImage_SetDotsPerMeterX FreeImage_SetDotsPerMeterY
                  FreeImage_GetInfoHeader FreeImage_GetInfo
                  FreeImage_GetColorType FreeImage_GetRedMask
                  FreeImage_GetGreenMask FreeImage_GetBlueMask
                  FreeImage_GetTransparencyCount FreeImage_GetTransparencyTable
                  FreeImage_SetTransparent FreeImage_SetTransparencyTable
                  FreeImage_IsTransparent FreeImage_SetTransparentIndex
                  FreeImage_GetTransparentIndex FreeImage_HasBackgroundColor
                  FreeImage_GetBackgroundColor FreeImage_SetBackgroundColor
                  FreeImage_GetThumbnail FreeImage_SetThumbnail

                  FreeImage_ConvertTo4Bits FreeImage_ConvertTo8Bits
                  FreeImage_ConvertToGreyscale FreeImage_ConvertTo16Bits555
                  FreeImage_ConvertTo16Bits565 FreeImage_ConvertTo24Bits
                  FreeImage_ConvertTo32Bits FreeImage_ColorQuantize
                  FreeImage_ColorQuantizeEx FreeImage_Threshold FreeImage_Dither
                  FreeImage_ConvertFromRawBits FreeImage_ConvertFromRawBitsEx
                  FreeImage_ConvertToRawBits FreeImage_ConvertToFloat
                  FreeImage_ConvertToRGBF FreeImage_ConvertToRGBAF
                  FreeImage_ConvertToUINT16 FreeImage_ConvertToRGB16
                  FreeImage_ConvertToRGBA16 FreeImage_ConvertToStandardType
                  FreeImage_ConvertToType FreeImage_ToneMapping
                  FreeImage_TmoDrago03 FreeImage_TmoReinhard05
                  FreeImage_TmoReinhard05Ex FreeImage_TmoFattal02
                  FreeImage_ZLibCompress FreeImage_ZLibUncompress
                  FreeImage_ZLibGZip FreeImage_ZLibGUnzip FreeImage_ZLibCRC32

                  FreeImage_Rotate FreeImage_RotateEx FreeImage_FlipHorizontal
                  FreeImage_FlipVertical FreeImage_Rescale
                  FreeImage_MakeThumbnail FreeImage_RescaleRect
                  FreeImage_AdjustCurve FreeImage_AdjustGamma
                  FreeImage_AdjustBrightness FreeImage_AdjustContrast
                  FreeImage_Invert FreeImage_GetHistogram FreeImage_AdjustColors
                  FreeImage_ApplyColorMapping FreeImage_SwapColors
                  FreeImage_ApplyPaletteIndexMapping FreeImage_SwapPaletteIndices
                  FreeImage_GetChannel FreeImage_SetChannel
                  FreeImage_GetComplexChannel FreeImage_SetComplexChannel
                  FreeImage_Copy FreeImage_Paste FreeImage_CreateView
                  FreeImage_Composite FreeImage_PreMultiplyWithAlpha
                  FreeImage_FillBackground FreeImage_EnlargeCanvas
                  FreeImage_AllocateEx FreeImage_AllocateExT
                  FreeImage_MultigridPoissonSolver]}
       )}.read("freeimage").eval(env);

    version(none)
    q{(ffi C
       {:flags   ["-DSQLITE_OMIT_DEPRECATED"] :wrap true}
       {:lib     "E:/Code/vile/lib/windows/x86_64/sqlite3.dll"
        :include ["E:/Code/vile/inc/sqlite3.h"]
        :import  [sqlite3_version sqlite3_libversion sqlite3_sourceid
                  sqlite3_threadsafe sqlite3 sqlite3_int64 sqlite3_uint64
                  sqlite3_close sqlite3_close_v2 sqlite3_callback
                  sqlite3_exec SQLITE_OK SQLITE_ERROR SQLITE_INTERNAL
                  SQLITE_PERM SQLITE_ABORT SQLITE_BUSY SQLITE_LOCKED
                  SQLITE_NOMEM SQLITE_READONLY SQLITE_INTERRUPT SQLITE_IOERR
                  SQLITE_CORRUPT SQLITE_NOTFOUND SQLITE_FULL SQLITE_CANTOPEN
                  SQLITE_PROTOCOL SQLITE_EMPTY SQLITE_SCHEMA SQLITE_TOOBIG
                  SQLITE_CONSTRAINT SQLITE_MISMATCH SQLITE_MISUSE SQLITE_NOLFS
                  SQLITE_AUTH SQLITE_FORMAT SQLITE_RANGE SQLITE_NOTADB
                  SQLITE_NOTICE SQLITE_WARNING SQLITE_ROW SQLITE_DONE
                  SQLITE_ERROR_MISSING_COLLSEQ SQLITE_ERROR_RETRY
                  SQLITE_IOERR_READ SQLITE_IOERR_SHORT_READ SQLITE_IOERR_WRITE
                  SQLITE_IOERR_FSYNC SQLITE_IOERR_DIR_FSYNC SQLITE_IOERR_TRUNCATE
                  SQLITE_IOERR_FSTAT SQLITE_IOERR_UNLOCK SQLITE_IOERR_RDLOCK
                  SQLITE_IOERR_DELETE SQLITE_IOERR_BLOCKED SQLITE_IOERR_NOMEM
                  SQLITE_IOERR_ACCESS SQLITE_IOERR_CHECKRESERVEDLOCK
                  SQLITE_IOERR_LOCK SQLITE_IOERR_CLOSE SQLITE_IOERR_DIR_CLOSE

                  sqlite3_file sqlite3_io_methods

                  sqlite3_mutex sqlite3_api_routines sqlite3_vfs
                  sqlite3_syscall_ptr

                  sqlite3_initialize sqlite3_shutdown sqlite3_os_init
                  sqlite3_os_end sqlite3_config sqlite3_db_config
                  sqlite3_mem_methods sqlite3_extended_result_codes
                  sqlite3_last_insert_rowid sqlite3_set_last_insert_rowid
                  sqlite3_changes sqlite3_total_changes sqlite3_interrupt
                  sqlite3_complete sqlite3_complete16 sqlite3_busy_handler
                  sqlite3_busy_timeout sqlite3_get_table sqlite3_free_table
                  sqlite3_mprintf sqlite3_vmprintf sqlite3_snprintf
                  sqlite3_vsnprintf sqlite3_malloc sqlite3_malloc64
                  sqlite3_realloc sqlite3_realloc64 sqlite3_free sqlite3_msize
                  sqlite3_memory_used sqlite3_memory_highwater sqlite3_randomness
                  sqlite3_set_authorizer

                  sqlite3_trace sqlite3_profile

                  sqlite3_trace_v2 sqlite3_progress_handler sqlite3_open
                  sqlite3_open16 sqlite3_open_v2 sqlite3_uri_parameter
                  sqlite3_uri_boolean sqlite3_uri_int64 sqlite3_errcode
                  sqlite3_extended_errcode sqlite3_errmsg sqlite3_errmsg16
                  sqlite3_errstr sqlite3_stmt sqlite3_limit

                  sqlite3_prepare sqlite3_prepare_v2 sqlite3_prepare_v3
                  sqlite3_prepare16 sqlite3_prepare16_v2 sqlite3_prepare16_v3
                  sqlite3_sql sqlite3_expanded_sql sqlite3_stmt_readonly
                  sqlite3_stmt_busy sqlite3_value sqlite3_context
                  sqlite3_bind_blob sqlite3_bind_blob64 sqlite3_bind_double
                  sqlite3_bind_int sqlite3_bind_int64 sqlite3_bind_null
                  sqlite3_bind_text sqlite3_bind_text16 sqlite3_bind_text64
                  sqlite3_bind_value sqlite3_bind_pointer sqlite3_bind_zeroblob
                  sqlite3_bind_zeroblob64 sqlite3_bind_parameter_count
                  sqlite3_bind_parameter_name sqlite3_bind_parameter_index
                  sqlite3_clear_bindings sqlite3_column_count sqlite3_column_name
                  sqlite3_column_name16 sqlite3_column_database_name
                  sqlite3_column_database_name16 sqlite3_column_table_name
                  sqlite3_column_table_name16 sqlite3_column_origin_name
                  sqlite3_column_origin_name16 sqlite3_column_decltype
                  sqlite3_column_decltype16 sqlite3_step sqlite3_data_count
                  SQLITE_INTEGER SQLITE_FLOAT SQLITE_BLOB SQLITE_NULL SQLITE_TEXT
                  SQLITE3_TEXT sqlite3_column_blob sqlite3_column_double
                  sqlite3_column_int sqlite3_column_int64 sqlite3_column_text
                  sqlite3_column_text16 sqlite3_column_value sqlite3_column_bytes
                  sqlite3_column_bytes16 sqlite3_column_type sqlite3_finalize
                  sqlite3_reset sqlite3_create_function sqlite3_create_function16
                  sqlite3_create_function_v2 SQLITE_UTF8 SQLITE_UTF16LE
                  SQLITE_UTF16BE SQLITE_UTF16 SQLITE_ANY SQLITE_UTF16_ALIGNED
                  SQLITE_DETERMINISTIC sqlite3_value_blob sqlite3_value_double
                  sqlite3_value_int sqlite3_value_int64 sqlite3_value_pointer
                  sqlite3_value_text sqlite3_value_text16 sqlite3_value_text16le
                  sqlite3_value_text16be sqlite3_value_bytes
                  sqlite3_value_bytes16 sqlite3_value_type
                  sqlite3_value_numeric_type sqlite3_value_nochange
                  sqlite3_value_subtype sqlite3_value_dup sqlite3_value_free
                  sqlite3_aggregate_context sqlite3_user_data
                  sqlite3_context_db_handle sqlite3_get_auxdata
                  sqlite3_set_auxdata sqlite3_destructor_type SQLITE_STATIC
                  #_SQLITE_TRANSIENT sqlite3_result_blob sqlite3_result_blob64
                  sqlite3_result_double sqlite3_result_error
                  sqlite3_result_error16 sqlite3_result_error_toobig
                  sqlite3_result_error_nomem sqlite3_result_error_code
                  sqlite3_result_int sqlite3_result_int64 sqlite3_result_null
                  sqlite3_result_text sqlite3_result_text64 sqlite3_result_text16
                  sqlite3_result_text16le sqlite3_result_text16be
                  sqlite3_result_value sqlite3_result_pointer
                  sqlite3_result_zeroblob sqlite3_result_zeroblob64
                  sqlite3_result_subtype sqlite3_create_collation
                  sqlite3_create_collation_v2 sqlite3_create_collation16
                  sqlite3_collation_needed sqlite3_collation_needed16
                  #_sqlite3_key #_sqlite3_key_v2 #_sqlite3_rekey #_sqlite3_rekey_v2
                  #_sqlite3_activate_see #_sqlite3_activate_cerod sqlite3_sleep
                  sqlite3_temp_directory sqlite3_data_directory
                  #_sqlite3_win32_set_directory #_sqlite3_win32_set_directory8
                  #_sqlite3_win32_set_directory16 SQLITE_WIN32_DATA_DIRECTORY_TYPE
                  SQLITE_WIN32_TEMP_DIRECTORY_TYPE sqlite3_get_autocommit
                  sqlite3_db_handle sqlite3_db_filename sqlite3_db_readonly
                  sqlite3_next_stmt sqlite3_commit_hook sqlite3_rollback_hook
                  sqlite3_update_hook sqlite3_enable_shared_cache
                  sqlite3_release_memory sqlite3_db_release_memory
                  sqlite3_soft_heap_limit64 sqlite3_soft_heap_limit
                  sqlite3_table_column_metadata sqlite3_load_extension
                  sqlite3_enable_load_extension sqlite3_auto_extension
                  sqlite3_reset_auto_extension sqlite3_vtab sqlite3_index_info
                  sqlite3_vtab_cursor sqlite3_module sqlite3_index_info

                  sqlite3_create_module sqlite3_create_module_v2
                  sqlite3_declare_vtab sqlite3_overload_function sqlite3_blob
                  sqlite3_blob_open sqlite3_blob_reopen sqlite3_blob_close
                  sqlite3_blob_bytes sqlite3_blob_read sqlite3_blob_write
                  sqlite3_vfs_find sqlite3_vfs_register sqlite3_vfs_unregister
                  sqlite3_mutex_alloc sqlite3_mutex_free sqlite3_mutex_enter
                  sqlite3_mutex_try sqlite3_mutex_leave sqlite3_mutex_methods
                  #_sqlite3_mutex_held #_sqlite3_mutex_notheld

                  sqlite3_db_mutex sqlite3_file_control sqlite3_test_control

                  sqlite3_keyword_count sqlite3_keyword_name
                  sqlite3_keyword_check sqlite3_str sqlite3_str_new
                  sqlite3_str_finish sqlite3_str_appendf sqlite3_str_vappendf
                  sqlite3_str_append sqlite3_str_appendall sqlite3_str_appendchar
                  sqlite3_str_reset sqlite3_str_errcode sqlite3_str_length
                  sqlite3_str_value sqlite3_status sqlite3_status64

                  sqlite3_db_status

                  sqlite3_stmt_status

                  sqlite3_pcache sqlite3_pcache_page sqlite3_pcache_methods2
                  sqlite3_pcache_methods sqlite3_backup sqlite3_backup_init
                  sqlite3_backup_step sqlite3_backup_finish
                  sqlite3_backup_remaining sqlite3_backup_pagecount
                  #_sqlite3_unlock_notify sqlite3_stricmp sqlite3_strnicmp
                  sqlite3_strglob sqlite3_strlike sqlite3_log sqlite3_wal_hook
                  sqlite3_wal_autocheckpoint sqlite3_wal_checkpoint
                  sqlite3_wal_checkpoint_v2

                  sqlite3_vtab_config SQLITE_VTAB_CONSTRAINT_SUPPORT
                  sqlite3_vtab_on_conflict sqlite3_vtab_nochange
                  sqlite3_vtab_collation SQLITE_ROLLBACK SQLITE_FAIL
                  SQLITE_REPLACE

                  #_sqlite3_stmt_scanstatus #_sqlite3_stmt_scanstatus_reset
                  sqlite3_db_cacheflush

                  sqlite3_system_errno sqlite3_snapshot #_sqlite3_snapshot_get
                  #_sqlite3_snapshot_open #_sqlite3_snapshot_free
                  #_sqlite3_snapshot_cmp #_sqlite3_snapshot_recover #_sqlite3_serialize
                  SQLITE_SERIALIZE_NOCOPY #_sqlite3_deserialize

                  sqlite3_rtree_geometry sqlite3_rtree_query_info
                  sqlite3_rtree_dbl sqlite3_rtree_geometry_callback
                  sqlite3_rtree_query_callback NOT_WITHIN PARTLY_WITHIN
                  FULLY_WITHIN
                  ]}
       )}.read("sqlite").eval(env);

    version (none)
    q{(ffi C
       {:lib     "/usr/local/lib/libhiredis.dylib"
        :include ["/usr/local/include/hiredis/async.h"]
        :import  [HIREDIS_MAJOR HIREDIS_MINOR HIREDIS_PATCH HIREDIS_SONAME
                  REDIS_BLOCK REDIS_CONNECTED REDIS_DISCONNECTING REDIS_FREEING
                  REDIS_IN_CALLBACK REDIS_SUBSCRIBED REDIS_MONITORING
                  REDIS_REUSEADDR REDIS_KEEPALIVE_INTERVAL REDIS_CONNECT_RETRIES
                  redisReply redisReaderCreate freeReplyObject
                  redisvFormatCommand redisFormatCommand redisFormatCommandArgv
                  redisFormatSdsCommandArgv redisFreeCommand redisFreeSdsCommand
                  redisConnectionType redisContext redisConnect
                  redisConnectWithTimeout redisConnectNonBlock
                  redisConnectBindNonBlock redisConnectBindNonBlockWithReuse
                  redisConnectUnix redisConnectUnixWithTimeout
                  redisConnectUnixNonBlock redisConnectFd redisReconnect
                  redisSetTimeout redisEnableKeepAlive redisFree redisFreeKeepFd
                  redisBufferRead redisBufferWrite redisGetReply
                  redisGetReplyFromReader redisAppendFormattedCommand
                  redisvAppendCommand redisAppendCommand redisAppendCommandArgv
                  redisvCommand redisCommand redisCommandArgv
                  REDIS_ERR REDIS_OK REDIS_ERR_IO REDIS_ERR_EOF
                  REDIS_ERR_PROTOCOL REDIS_ERR_OOM REDIS_ERR_OTHER
                  REDIS_REPLY_STRING REDIS_REPLY_ARRAY REDIS_REPLY_INTEGER
                  REDIS_REPLY_NIL REDIS_REPLY_STATUS REDIS_REPLY_ERROR
                  REDIS_READER_MAX_BUF redisReadTask redisReplyObjectFunctions
                  redisReader redisReaderCreateWithFunctions redisReaderFree
                  redisReaderFeed redisReaderGetReply
                  sds sdshdr sdsnewlen sdsnew sdsempty #_sdslen #_sdsavail
                  sdsgrowzero sdscatlen sdscatsds sdscpylen sdscpy sdscatvprintf
                  sdscatprintf sdscatfmt sdstrim sdsrange sdsupdatelen sdsclear
                  sdscmp sdssplitlen sdsfreesplitres sdstolower sdstoupper
                  sdsfromlonglong sdscatrepr sdssplitargs sdsmapchars sdsjoin
                  sdsjoinsds sdsMakeRoomFor sdsIncrLen sdsRemoveFreeSpace
                  sdsAllocSize redisAsyncContext redisCallbackFn redisCallback
                  redisCallbackList redisDisconnectCallback redisConnectCallback
                  redisAsyncConnect redisAsyncConnectBind
                  redisAsyncConnectBindWithReuse redisAsyncConnectUnix
                  redisAsyncSetConnectCallback redisAsyncSetDisconnectCallback
                  redisAsyncDisconnect redisAsyncFree redisAsyncHandleRead
                  redisAsyncHandleWrite redisvAsyncCommand redisAsyncCommandArgv
                  redisAsyncFormattedCommand
                  ]}
       )}.read("hiredis").eval(env);

    version(none)
    q{(ffi C
       {:flags ["-ID:/Development/Assimp/include"
                "-Wno-pragma-pack"]}
       {:lib     #_"/usr/local/lib/libassimp.dylib" "d:/Development/Assimp/bin/x64/assimp-vc140-mt.dll"
        :include ["assimp/anim.h"
                  "assimp/camera.h"
                  "assimp/cexport.h"
                  "assimp/cfileio.h"
                  "assimp/cimport.h"
                  "assimp/light.h"
                  "assimp/mesh.h"
                  "assimp/metadata.h"
                  "assimp/postprocess.h"
                  "assimp/scene.h"
                  "assimp/texture.h"
                  "assimp/version.h"]
        :import  [aiScene aiFileIO aiVector2D aiVector3D aiColor4D aiMatrix3x3
                  aiMatrix4x4 aiQuaternion aiPlane aiRay aiColor3D aiString
                  aiReturn aiOrigin aiDefaultLogStream
                  aiMemoryInfo aiImporterFlags aiImporterDesc #_aiGetImporterDesc
                  aiLogStreamCallback aiLogStream aiExportFormatDesc
                  aiGetExportFormatCount aiGetExportFormatDescription
                  aiReleaseExportFormatDescription aiCopyScene aiFreeScene
                  aiExportScene aiExportSceneEx aiExportDataBlob
                  aiExportSceneToBlob aiReleaseExportBlob aiFile
                  aiFileOpenProc aiFileCloseProc aiUserData
                  aiPropertyStore aiBool AI_FALSE AI_TRUE aiImportFile
                  aiImportFileEx aiImportFileExWithProperties
                  aiImportFileFromMemory aiImportFileFromMemoryWithProperties
                  aiApplyPostProcessing aiGetPredefinedLogStream
                  aiAttachLogStream aiEnableVerboseLogging aiDetachLogStream
                  aiDetachAllLogStreams aiReleaseImport aiGetErrorString
                  aiIsExtensionSupported aiGetExtensionList
                  aiGetMemoryRequirements aiCreatePropertyStore
                  aiReleasePropertyStore aiSetImportPropertyInteger
                  aiSetImportPropertyFloat aiSetImportPropertyString
                  aiSetImportPropertyMatrix aiCreateQuaternionFromMatrix
                  aiDecomposeMatrix aiTransposeMatrix4 aiTransposeMatrix3
                  aiTransformVecByMatrix3 aiTransformVecByMatrix4
                  aiMultiplyMatrix4 aiMultiplyMatrix3 aiIdentityMatrix3
                  aiIdentityMatrix4 aiGetImportFormatCount
                  aiGetImportFormatDescription
                  aiVectorKey aiQuatKey aiMeshKey aiMeshMorphKey aiAnimBehaviour
                  aiNodeAnim aiMeshAnim aiMeshMorphAnim aiAnimation aiCamera
                  aiLightSourceType aiLight aiTextureOp aiTextureMapMode
                  aiTextureMapping aiTextureType aiShadingMode aiTextureFlags
                  aiBlendMode aiUVTransform aiPropertyTypeInfo aiMaterialProperty
                  aiMaterial #_aiGetMaterialInteger aiGetMaterialColor
                  aiGetMaterialUVTransform aiGetMaterialString
                  aiGetMaterialTextureCount aiGetMaterialTexture
                  AI_MAX_FACE_INDICES AI_MAX_BONE_WEIGHTS AI_MAX_VERTICES
                  AI_MAX_FACES AI_MAX_NUMBER_OF_COLOR_SETS
                  AI_MAX_NUMBER_OF_TEXTURECOORDS aiFace aiVertexWeight
                  aiBone aiPrimitiveType aiAnimMesh aiMorphingMethod
                  aiMesh aiMetadataType aiMetadataEntry aiMetadata
                  aiPostProcessSteps aiProcess_ConvertToLeftHanded
                  aiProcessPreset_TargetRealtime_Fast
                  aiProcessPreset_TargetRealtime_Quality
                  aiProcessPreset_TargetRealtime_MaxQuality
                  aiNode AI_SCENE_FLAGS_INCOMPLETE AI_SCENE_FLAGS_VALIDATED
                  AI_SCENE_FLAGS_VALIDATION_WARNING
                  AI_SCENE_FLAGS_NON_VERBOSE_FORMAT AI_SCENE_FLAGS_TERRAIN
                  AI_SCENE_FLAGS_ALLOW_SHARED AI_EMBEDDED_TEXNAME_PREFIX
                  aiTexel aiTexture aiGetLegalString aiGetVersionMinor
                  aiGetVersionMajor aiGetVersionRevision
                  ASSIMP_CFLAGS_SHARED ASSIMP_CFLAGS_STLPORT
                  ASSIMP_CFLAGS_DEBUG ASSIMP_CFLAGS_NOBOOST
                  ASSIMP_CFLAGS_SINGLETHREADED aiGetCompileFlags]}
       )}.read("assimp").eval(env);

    version (none)
    q{(ffi C
       #?:windows
       {:flags ["-I../llvm-8.0.0.src/include"
                "-I../llvm-8.0.0.src/msvc/include"]}
       {:lib #?(:linux   "libLLVM.so"
                :macos   "libLLVM.dylib"
                :windows "E:/Code/llvm-8.0.0.src/msvc/RelWithDebInfo/lib/LLVM.dll")
        :include ["llvm-c/Core.h"
                  "llvm-c/ExecutionEngine.h"
                  "llvm-c/OrcBindings.h"
                  "llvm-c/Analysis.h"
                  "llvm-c/BitWriter.h"]
        :import [;;; Types
                 LLVMBool
                 LLVMMemoryBufferRef
                 LLVMContextRef
                 LLVMModuleRef
                 LLVMTypeRef
                 LLVMBasicBlockRef
                 LLVMMetadataRef
                 LLVMNamedMDNodeRef
                 LLVMValueMetadataEntry
                 LLVMBuilderRef
                 LLVMDIBuilderRef
                 LLVMModuleProviderRef
                 LLVMPassManagerRef
                 LLVMPassRegistryRef
                 LLVMUseRef
                 LLVMAttributeRef
                 LLVMDiagnosticInfoRef
                 LLVMComdatRef
                 LLVMModuleFlagEntry
                 LLVMJITEventListenerRef
                 ;;; Error Handling
                 LLVMFatalErrorHandler
                 LLVMInstallFatalErrorHandler
                 LLVMResetFatalErrorHandler
                 LLVMEnablePrettyStackTrace
                 ;;; Error
                 LLVMErrorRef
                 LLVMErrorTypeId
                 LLVMGetErrorTypeId
                 LLVMConsumeError
                 LLVMGetErrorMessage
                 LLVMDisposeErrorMessage
                 LLVMGetStringErrorTypeId
                 ;;; Core
                 LLVMOpcode
                 LLVMTypeKind
                 LLVMLinkage
                 LLVMVisibility
                 LLVMUnnamedAddr
                 LLVMDLLStorageClass
                 LLVMCallConv
                 LLVMValueKind
                 LLVMIntPredicate
                 LLVMRealPredicate
                 LLVMLandingPadClauseTy
                 LLVMThreadLocalMode
                 LLVMAtomicOrdering
                 LLVMAtomicRMWBinOp
                 LLVMDiagnosticSeverity
                 LLVMInlineAsmDialect
                 LLVMModuleFlagBehavior
                 #_LLVMAttributeReturnIndex
                 #_LLVMAttributeFunctionIndex
                 LLVMAttributeIndex
                 LLVMInitializeCore
                 LLVMShutdown
                 LLVMCreateMessage
                 LLVMDisposeMessage
                 LLVMDiagnosticHandler
                 LLVMYieldCallback
                 LLVMContextCreate
                 LLVMGetGlobalContext
                 LLVMContextSetDiagnosticHandler
                 LLVMContextGetDiagnosticContext
                 LLVMContextSetYieldCallback
                 LLVMContextShouldDiscardValueNames
                 LLVMContextSetDiscardValueNames
                 LLVMContextDispose
                 LLVMGetDiagInfoDescription
                 LLVMGetDiagInfoSeverity
                 LLVMGetMDKindIDInContext
                 LLVMGetMDKindID
                 LLVMGetEnumAttributeKindForName
                 LLVMGetLastEnumAttributeKind
                 LLVMCreateEnumAttribute
                 LLVMGetEnumAttributeKind
                 LLVMGetEnumAttributeValue
                 LLVMCreateStringAttribute
                 LLVMGetStringAttributeKind
                 LLVMGetStringAttributeValue
                 LLVMIsEnumAttribute
                 LLVMIsStringAttribute
                 LLVMModuleCreateWithName
                 LLVMModuleCreateWithNameInContext
                 LLVMCloneModule
                 LLVMDisposeModule
                 LLVMGetModuleIdentifier
                 LLVMSetModuleIdentifier
                 LLVMGetSourceFileName
                 LLVMSetSourceFileName
                 LLVMGetDataLayoutStr
                 LLVMGetDataLayout
                 LLVMSetDataLayout
                 LLVMGetTarget
                 LLVMSetTarget
                 LLVMCopyModuleFlagsMetadata
                 LLVMDisposeModuleFlagsMetadata
                 LLVMModuleFlagEntriesGetFlagBehavior
                 LLVMModuleFlagEntriesGetKey
                 LLVMModuleFlagEntriesGetMetadata
                 LLVMGetModuleFlag
                 LLVMAddModuleFlag
                 LLVMDumpModule
                 LLVMPrintModuleToFile
                 LLVMPrintModuleToString
                 LLVMGetModuleInlineAsm
                 LLVMSetModuleInlineAsm2
                 LLVMAppendModuleInlineAsm
                 LLVMGetInlineAsm
                 LLVMGetModuleContext
                 LLVMGetTypeByName
                 LLVMGetFirstNamedMetadata
                 LLVMGetLastNamedMetadata
                 LLVMGetNextNamedMetadata
                 LLVMGetPreviousNamedMetadata
                 LLVMGetNamedMetadata
                 LLVMGetOrInsertNamedMetadata
                 LLVMGetNamedMetadataName
                 LLVMGetNamedMetadataNumOperands
                 LLVMGetNamedMetadataOperands
                 LLVMAddNamedMetadataOperand
                 LLVMGetDebugLocDirectory
                 LLVMGetDebugLocFilename
                 LLVMGetDebugLocLine
                 LLVMGetDebugLocColumn
                 LLVMAddFunction
                 LLVMGetNamedFunction
                 LLVMGetFirstFunction
                 LLVMGetLastFunction
                 LLVMGetNextFunction
                 LLVMGetPreviousFunction
                 LLVMGetTypeKind
                 LLVMTypeIsSized
                 LLVMGetTypeContext
                 LLVMDumpType
                 LLVMPrintTypeToString
                 LLVMInt1TypeInContext
                 LLVMInt8TypeInContext
                 LLVMInt16TypeInContext
                 LLVMInt32TypeInContext
                 LLVMInt64TypeInContext
                 LLVMInt128TypeInContext
                 LLVMIntTypeInContext
                 LLVMInt1Type
                 LLVMInt8Type
                 LLVMInt16Type
                 LLVMInt32Type
                 LLVMInt64Type
                 LLVMInt128Type
                 LLVMIntType
                 LLVMGetIntTypeWidth
                 LLVMHalfTypeInContext
                 LLVMFloatTypeInContext
                 LLVMDoubleTypeInContext
                 LLVMX86FP80TypeInContext
                 LLVMFP128TypeInContext
                 LLVMPPCFP128TypeInContext
                 LLVMHalfType
                 LLVMFloatType
                 LLVMDoubleType
                 LLVMX86FP80Type
                 LLVMFP128Type
                 LLVMPPCFP128Type
                 LLVMFunctionType
                 LLVMIsFunctionVarArg
                 LLVMGetReturnType
                 LLVMCountParamTypes
                 LLVMGetParamTypes
                 LLVMStructTypeInContext
                 LLVMStructType
                 LLVMStructCreateNamed
                 LLVMGetStructName
                 LLVMStructSetBody
                 LLVMCountStructElementTypes
                 LLVMGetStructElementTypes
                 LLVMStructGetTypeAtIndex
                 LLVMIsPackedStruct
                 LLVMIsOpaqueStruct
                 LLVMIsLiteralStruct
                 LLVMGetElementType
                 LLVMGetSubtypes
                 LLVMGetNumContainedTypes
                 LLVMArrayType
                 LLVMGetArrayLength
                 LLVMPointerType
                 LLVMGetPointerAddressSpace
                 LLVMVectorType
                 LLVMGetVectorSize
                 LLVMVoidTypeInContext
                 LLVMLabelTypeInContext
                 LLVMX86MMXTypeInContext
                 LLVMTokenTypeInContext
                 LLVMMetadataTypeInContext
                 LLVMVoidType
                 LLVMLabelType
                 LLVMX86MMXType
                 ;; TODO instructions
                 LLVMTypeOf
                 LLVMGetValueKind
                 LLVMGetValueName2
                 LLVMSetValueName2
                 LLVMDumpValue
                 LLVMPrintValueToString
                 LLVMReplaceAllUsesWith
                 LLVMIsConstant
                 LLVMIsUndef
                 LLVMIsAMDNode
                 LLVMIsAMDString
                 LLVMGetValueName
                 LLVMSetValueName
                 LLVMGetFirstUse
                 LLVMGetNextUse
                 LLVMGetUser
                 LLVMGetUsedValue
                 LLVMGetOperand
                 LLVMGetOperandUse
                 LLVMSetOperand
                 LLVMGetNumOperands
                 LLVMConstNull
                 LLVMConstAllOnes
                 LLVMGetUndef
                 LLVMIsNull
                 LLVMConstPointerNull
                 LLVMConstInt
                 LLVMConstIntOfArbitraryPrecision
                 LLVMConstIntOfString
                 LLVMConstIntOfStringAndSize
                 LLVMConstReal
                 LLVMConstRealOfString
                 LLVMConstRealOfStringAndSize
                 LLVMConstIntGetZExtValue
                 LLVMConstIntGetSExtValue
                 LLVMConstRealGetDouble
                 LLVMConstStringInContext
                 LLVMConstString
                 LLVMIsConstantString
                 LLVMGetAsString
                 LLVMConstStructInContext
                 LLVMConstStruct
                 LLVMConstArray
                 LLVMConstNamedStruct
                 LLVMGetElementAsConstant
                 LLVMConstVector
                 LLVMGetConstOpcode
                 LLVMAlignOf
                 LLVMSizeOf
                 LLVMConstNeg
                 LLVMConstNSWNeg
                 LLVMConstNUWNeg
                 LLVMConstFNeg
                 LLVMConstNot
                 LLVMConstAdd
                 LLVMConstNSWAdd
                 LLVMConstNUWAdd
                 LLVMConstFAdd
                 LLVMConstSub
                 LLVMConstNSWSub
                 LLVMConstNUWSub
                 LLVMConstFSub
                 LLVMConstMul
                 LLVMConstNSWMul
                 LLVMConstNUWMul
                 LLVMConstFMul
                 LLVMConstUDiv
                 LLVMConstExactUDiv
                 LLVMConstSDiv
                 LLVMConstExactSDiv
                 LLVMConstFDiv
                 LLVMConstURem
                 LLVMConstSRem
                 LLVMConstFRem
                 LLVMConstAnd
                 LLVMConstOr
                 LLVMConstXor
                 LLVMConstICmp
                 LLVMConstFCmp
                 LLVMConstShl
                 LLVMConstLShr
                 LLVMConstAShr
                 LLVMConstGEP
                 #_LLVMConstGEP2
                 LLVMConstInBoundsGEP
                 #_LLVMConstInBoundsGEP2
                 LLVMConstTrunc
                 LLVMConstSExt
                 LLVMConstZExt
                 LLVMConstFPTrunc
                 LLVMConstFPExt
                 LLVMConstUIToFP
                 LLVMConstSIToFP
                 LLVMConstFPToUI
                 LLVMConstFPToSI
                 LLVMConstPtrToInt
                 LLVMConstIntToPtr
                 LLVMConstBitCast
                 LLVMConstAddrSpaceCast
                 LLVMConstZExtOrBitCast
                 LLVMConstSExtOrBitCast
                 LLVMConstTruncOrBitCast
                 LLVMConstPointerCast
                 LLVMConstIntCast
                 LLVMConstFPCast
                 LLVMConstSelect
                 LLVMConstExtractElement
                 LLVMConstInsertElement
                 LLVMConstShuffleVector
                 LLVMConstExtractValue
                 LLVMConstInsertValue
                 LLVMBlockAddress
                 LLVMGetGlobalParent
                 LLVMIsDeclaration
                 LLVMGetLinkage
                 LLVMSetLinkage
                 LLVMGetSection
                 LLVMSetSection
                 LLVMGetVisibility
                 LLVMSetVisibility
                 LLVMGetDLLStorageClass
                 LLVMSetDLLStorageClass
                 LLVMGetUnnamedAddress
                 LLVMSetUnnamedAddress
                 LLVMGlobalGetValueType
                 LLVMGetAlignment
                 LLVMSetAlignment
                 LLVMGlobalSetMetadata
                 LLVMGlobalEraseMetadata
                 LLVMGlobalClearMetadata
                 LLVMGlobalCopyAllMetadata
                 LLVMDisposeValueMetadataEntries
                 LLVMValueMetadataEntriesGetKind
                 LLVMValueMetadataEntriesGetMetadata
                 LLVMAddGlobal
                 LLVMAddGlobalInAddressSpace
                 LLVMGetNamedGlobal
                 LLVMGetFirstGlobal
                 LLVMGetLastGlobal
                 LLVMGetNextGlobal
                 LLVMGetPreviousGlobal
                 LLVMDeleteGlobal
                 LLVMGetInitializer
                 LLVMSetInitializer
                 LLVMIsThreadLocal
                 LLVMSetThreadLocal
                 LLVMIsGlobalConstant
                 LLVMSetGlobalConstant
                 LLVMGetThreadLocalMode
                 LLVMSetThreadLocalMode
                 LLVMIsExternallyInitialized
                 LLVMSetExternallyInitialized
                 LLVMAddAlias
                 LLVMGetNamedGlobalAlias
                 LLVMGetFirstGlobalAlias
                 LLVMGetLastGlobalAlias
                 LLVMGetNextGlobalAlias
                 LLVMGetPreviousGlobalAlias
                 LLVMAliasGetAliasee
                 LLVMAliasSetAliasee
                 LLVMDeleteFunction
                 LLVMHasPersonalityFn
                 LLVMGetPersonalityFn
                 LLVMSetPersonalityFn
                 LLVMGetIntrinsicID
                 LLVMGetIntrinsicDeclaration
                 LLVMIntrinsicGetType
                 LLVMIntrinsicGetName
                 LLVMIntrinsicCopyOverloadedName
                 LLVMIntrinsicIsOverloaded
                 LLVMGetFunctionCallConv
                 LLVMSetFunctionCallConv
                 LLVMGetGC
                 LLVMSetGC
                 LLVMAddAttributeAtIndex
                 LLVMGetAttributeCountAtIndex
                 LLVMGetAttributesAtIndex
                 LLVMGetEnumAttributeAtIndex
                 LLVMGetStringAttributeAtIndex
                 LLVMRemoveEnumAttributeAtIndex
                 LLVMRemoveStringAttributeAtIndex
                 LLVMAddTargetDependentFunctionAttr
                 LLVMCountParams
                 LLVMGetParams
                 LLVMGetParam
                 LLVMGetParamParent
                 LLVMGetFirstParam
                 LLVMGetLastParam
                 LLVMGetNextParam
                 LLVMGetPreviousParam
                 LLVMSetParamAlignment
                 LLVMMDStringInContext
                 LLVMMDString
                 LLVMMDNodeInContext
                 LLVMMDNode
                 LLVMMetadataAsValue
                 LLVMValueAsMetadata
                 LLVMGetMDString
                 LLVMGetMDNodeNumOperands
                 LLVMGetMDNodeOperands
                 LLVMBasicBlockAsValue
                 LLVMValueIsBasicBlock
                 LLVMValueAsBasicBlock
                 LLVMGetBasicBlockName
                 LLVMGetBasicBlockParent
                 LLVMGetBasicBlockTerminator
                 LLVMCountBasicBlocks
                 LLVMGetBasicBlocks
                 LLVMGetFirstBasicBlock
                 LLVMGetLastBasicBlock
                 LLVMGetNextBasicBlock
                 LLVMGetPreviousBasicBlock
                 LLVMGetEntryBasicBlock
                 LLVMCreateBasicBlockInContext
                 LLVMAppendBasicBlockInContext
                 LLVMAppendBasicBlock
                 LLVMInsertBasicBlockInContext
                 LLVMInsertBasicBlock
                 LLVMDeleteBasicBlock
                 LLVMRemoveBasicBlockFromParent
                 LLVMMoveBasicBlockBefore
                 LLVMMoveBasicBlockAfter
                 LLVMGetFirstInstruction
                 LLVMGetLastInstruction
                 LLVMHasMetadata
                 LLVMGetMetadata
                 LLVMSetMetadata
                 LLVMInstructionGetAllMetadataOtherThanDebugLoc
                 LLVMGetInstructionParent
                 LLVMGetNextInstruction
                 LLVMGetPreviousInstruction
                 LLVMInstructionRemoveFromParent
                 LLVMInstructionEraseFromParent
                 LLVMGetInstructionOpcode
                 LLVMGetICmpPredicate
                 LLVMGetFCmpPredicate
                 LLVMInstructionClone
                 LLVMIsATerminatorInst
                 LLVMGetNumArgOperands
                 LLVMSetInstructionCallConv
                 LLVMGetInstructionCallConv
                 LLVMSetInstrParamAlignment
                 LLVMAddCallSiteAttribute
                 LLVMGetCallSiteAttributeCount
                 LLVMGetCallSiteAttributes
                 LLVMGetCallSiteEnumAttribute
                 LLVMGetCallSiteStringAttribute
                 LLVMRemoveCallSiteEnumAttribute
                 LLVMRemoveCallSiteStringAttribute
                 LLVMGetCalledFunctionType
                 LLVMGetCalledValue
                 LLVMIsTailCall
                 LLVMSetTailCall
                 LLVMGetNormalDest
                 LLVMGetUnwindDest
                 LLVMSetNormalDest
                 LLVMSetUnwindDest
                 LLVMGetNumSuccessors
                 LLVMGetSuccessor
                 LLVMSetSuccessor
                 LLVMIsConditional
                 LLVMGetCondition
                 LLVMSetCondition
                 LLVMGetSwitchDefaultDest
                 LLVMGetAllocatedType
                 LLVMIsInBounds
                 LLVMSetIsInBounds
                 LLVMAddIncoming
                 LLVMCountIncoming
                 LLVMGetIncomingValue
                 LLVMGetIncomingBlock
                 LLVMGetNumIndices
                 LLVMGetIndices
                 LLVMCreateBuilderInContext
                 LLVMCreateBuilder
                 LLVMPositionBuilder
                 LLVMPositionBuilderBefore
                 LLVMPositionBuilderAtEnd
                 LLVMGetInsertBlock
                 LLVMClearInsertionPosition
                 LLVMInsertIntoBuilder
                 LLVMInsertIntoBuilderWithName
                 LLVMDisposeBuilder
                 LLVMSetCurrentDebugLocation
                 LLVMGetCurrentDebugLocation
                 LLVMSetInstDebugLocation
                 LLVMBuildRetVoid
                 LLVMBuildRet
                 LLVMBuildAggregateRet
                 LLVMBuildBr
                 LLVMBuildCondBr
                 LLVMBuildSwitch
                 LLVMBuildIndirectBr
                 LLVMBuildInvoke
                 LLVMBuildInvoke2
                 LLVMBuildUnreachable
                 LLVMBuildResume
                 LLVMBuildLandingPad
                 LLVMBuildCleanupRet
                 LLVMBuildCatchRet
                 LLVMBuildCatchPad
                 LLVMBuildCleanupPad
                 LLVMBuildCatchSwitch
                 LLVMAddCase
                 LLVMAddDestination
                 LLVMGetNumClauses
                 LLVMGetClause
                 LLVMAddClause
                 LLVMIsCleanup
                 LLVMSetCleanup
                 LLVMAddHandler
                 LLVMGetNumHandlers
                 LLVMGetHandlers
                 LLVMGetArgOperand
                 LLVMSetArgOperand
                 LLVMGetParentCatchSwitch
                 LLVMSetParentCatchSwitch
                 LLVMBuildAdd
                 LLVMBuildNSWAdd
                 LLVMBuildNUWAdd
                 LLVMBuildFAdd
                 LLVMBuildSub
                 LLVMBuildNSWSub
                 LLVMBuildNUWSub
                 LLVMBuildFSub
                 LLVMBuildMul
                 LLVMBuildNSWMul
                 LLVMBuildNUWMul
                 LLVMBuildFMul
                 LLVMBuildUDiv
                 LLVMBuildExactUDiv
                 LLVMBuildSDiv
                 LLVMBuildExactSDiv
                 LLVMBuildFDiv
                 LLVMBuildURem
                 LLVMBuildSRem
                 LLVMBuildFRem
                 LLVMBuildShl
                 LLVMBuildLShr
                 LLVMBuildAShr
                 LLVMBuildAnd
                 LLVMBuildOr
                 LLVMBuildXor
                 LLVMBuildBinOp
                 LLVMBuildNeg
                 LLVMBuildNSWNeg
                 LLVMBuildNUWNeg
                 LLVMBuildFNeg
                 LLVMBuildNot
                 LLVMBuildMalloc
                 LLVMBuildArrayMalloc
                 LLVMBuildMemSet
                 LLVMBuildMemCpy
                 LLVMBuildMemMove
                 LLVMBuildAlloca
                 LLVMBuildArrayAlloca
                 LLVMBuildFree
                 LLVMBuildLoad
                 LLVMBuildLoad2
                 LLVMBuildStore
                 LLVMBuildGEP
                 LLVMBuildInBoundsGEP
                 LLVMBuildStructGEP
                 LLVMBuildGEP2
                 LLVMBuildInBoundsGEP2
                 LLVMBuildStructGEP2
                 LLVMBuildGlobalString
                 LLVMBuildGlobalStringPtr
                 LLVMGetVolatile
                 LLVMSetVolatile
                 LLVMGetOrdering
                 LLVMSetOrdering
                 LLVMBuildTrunc
                 LLVMBuildZExt
                 LLVMBuildSExt
                 LLVMBuildFPToUI
                 LLVMBuildFPToSI
                 LLVMBuildUIToFP
                 LLVMBuildSIToFP
                 LLVMBuildFPTrunc
                 LLVMBuildFPExt
                 LLVMBuildPtrToInt
                 LLVMBuildIntToPtr
                 LLVMBuildBitCast
                 LLVMBuildAddrSpaceCast
                 LLVMBuildZExtOrBitCast
                 LLVMBuildSExtOrBitCast
                 LLVMBuildTruncOrBitCast
                 LLVMBuildCast
                 LLVMBuildPointerCast
                 LLVMBuildIntCast2
                 LLVMBuildFPCast
                 LLVMBuildIntCast
                 LLVMBuildICmp
                 LLVMBuildFCmp
                 LLVMBuildPhi
                 LLVMBuildCall
                 LLVMBuildCall2
                 LLVMBuildSelect
                 LLVMBuildVAArg
                 LLVMBuildExtractElement
                 LLVMBuildInsertElement
                 LLVMBuildShuffleVector
                 LLVMBuildExtractValue
                 LLVMBuildInsertValue
                 LLVMBuildIsNull
                 LLVMBuildIsNotNull
                 LLVMBuildPtrDiff
                 LLVMBuildFence
                 LLVMBuildAtomicRMW
                 LLVMBuildAtomicCmpXchg
                 LLVMIsAtomicSingleThread
                 LLVMSetAtomicSingleThread
                 LLVMGetCmpXchgSuccessOrdering
                 LLVMSetCmpXchgSuccessOrdering
                 LLVMGetCmpXchgFailureOrdering
                 LLVMSetCmpXchgFailureOrdering
                 LLVMCreateModuleProviderForExistingModule
                 LLVMDisposeModuleProvider
                 LLVMCreateMemoryBufferWithContentsOfFile
                 LLVMCreateMemoryBufferWithSTDIN
                 LLVMCreateMemoryBufferWithMemoryRange
                 LLVMCreateMemoryBufferWithMemoryRangeCopy
                 LLVMGetBufferStart
                 LLVMGetBufferSize
                 LLVMDisposeMemoryBuffer
                 LLVMGetGlobalPassRegistry
                 LLVMCreatePassManager
                 LLVMCreateFunctionPassManagerForModule
                 LLVMCreateFunctionPassManager
                 LLVMRunPassManager
                 LLVMInitializeFunctionPassManager
                 LLVMRunFunctionPassManager
                 LLVMFinalizeFunctionPassManager
                 LLVMDisposePassManager
                 LLVMIsMultithreaded
                 ;;; Target
                 LLVMByteOrdering
                 LLVMTargetDataRef
                 LLVMTargetLibraryInfoRef
                 ;; TODO initialize
                 LLVMInitializeX86TargetInfo
                 LLVMInitializeX86Target
                 LLVMInitializeX86TargetMC
                 LLVMInitializeX86AsmParser
                 LLVMInitializeX86AsmPrinter
                 LLVMGetModuleDataLayout
                 LLVMSetModuleDataLayout
                 LLVMCreateTargetData
                 LLVMDisposeTargetData
                 LLVMAddTargetLibraryInfo
                 LLVMCopyStringRepOfTargetData
                 LLVMByteOrder
                 LLVMPointerSize
                 LLVMPointerSizeForAS
                 LLVMIntPtrType
                 LLVMIntPtrTypeForAS
                 LLVMIntPtrTypeInContext
                 LLVMIntPtrTypeForASInContext
                 LLVMSizeOfTypeInBits
                 LLVMStoreSizeOfType
                 LLVMABISizeOfType
                 LLVMABIAlignmentOfType
                 LLVMCallFrameAlignmentOfType
                 LLVMPreferredAlignmentOfType
                 LLVMPreferredAlignmentOfGlobal
                 LLVMElementAtOffset
                 LLVMOffsetOfElement
                 ;;; Target Machine
                 LLVMTargetMachineRef
                 LLVMTargetRef
                 LLVMCodeGenOptLevel
                 LLVMRelocMode
                 LLVMCodeModel
                 LLVMCodeGenFileType
                 LLVMGetFirstTarget
                 LLVMGetNextTarget
                 LLVMGetTargetFromName
                 LLVMGetTargetFromTriple
                 LLVMGetTargetName
                 LLVMGetTargetDescription
                 LLVMTargetHasJIT
                 LLVMTargetHasTargetMachine
                 LLVMTargetHasAsmBackend
                 LLVMCreateTargetMachine
                 LLVMDisposeTargetMachine
                 LLVMGetTargetMachineTarget
                 LLVMGetTargetMachineTriple
                 LLVMGetTargetMachineCPU
                 LLVMGetTargetMachineFeatureString
                 LLVMCreateTargetDataLayout
                 LLVMSetTargetMachineAsmVerbosity
                 LLVMTargetMachineEmitToFile
                 LLVMTargetMachineEmitToMemoryBuffer
                 LLVMGetDefaultTargetTriple
                 LLVMNormalizeTargetTriple
                 LLVMGetHostCPUName
                 LLVMGetHostCPUFeatures
                 LLVMAddAnalysisPasses
                 ;;; Analysis
                 LLVMVerifierFailureAction
                 LLVMVerifyModule
                 LLVMVerifyFunction
                 LLVMViewFunctionCFG
                 LLVMViewFunctionCFGOnly
                 ;;; Execution Engine
                 LLVMLinkInMCJIT
                 LLVMLinkInInterpreter
                 LLVMGenericValueRef
                 LLVMExecutionEngineRef
                 LLVMMCJITMemoryManagerRef
                 LLVMMCJITCompilerOptions
                 LLVMCreateGenericValueOfInt
                 LLVMCreateGenericValueOfPointer
                 LLVMCreateGenericValueOfFloat
                 LLVMGenericValueIntWidth
                 LLVMGenericValueToInt
                 LLVMGenericValueToPointer
                 LLVMGenericValueToFloat
                 LLVMDisposeGenericValue
                 LLVMCreateExecutionEngineForModule
                 LLVMCreateInterpreterForModule
                 LLVMCreateJITCompilerForModule
                 LLVMInitializeMCJITCompilerOptions
                 LLVMCreateMCJITCompilerForModule
                 LLVMDisposeExecutionEngine
                 LLVMRunStaticConstructors
                 LLVMRunStaticDestructors
                 LLVMRunFunctionAsMain
                 LLVMRunFunction
                 LLVMFreeMachineCodeForFunction
                 LLVMAddModule
                 LLVMRemoveModule
                 LLVMFindFunction
                 LLVMRecompileAndRelinkFunction
                 LLVMGetExecutionEngineTargetData
                 LLVMGetExecutionEngineTargetMachine
                 LLVMAddGlobalMapping
                 LLVMGetPointerToGlobal
                 LLVMGetGlobalValueAddress
                 LLVMGetFunctionAddress
                 LLVMMemoryManagerAllocateCodeSectionCallback
                 LLVMMemoryManagerAllocateDataSectionCallback
                 LLVMMemoryManagerFinalizeMemoryCallback
                 LLVMMemoryManagerDestroyCallback
                 LLVMCreateSimpleMCJITMemoryManager
                 LLVMDisposeMCJITMemoryManager
                 LLVMCreateGDBRegistrationListener
                 LLVMCreateIntelJITEventListener
                 LLVMCreateOProfileJITEventListener
                 LLVMCreatePerfJITEventListener
                 ;;; ORC
                 LLVMOrcJITStackRef
                 LLVMOrcModuleHandle
                 LLVMOrcTargetAddress
                 LLVMOrcSymbolResolverFn
                 LLVMOrcLazyCompileCallbackFn
                 LLVMOrcCreateInstance
                 LLVMOrcGetErrorMsg
                 LLVMOrcGetMangledSymbol
                 LLVMOrcDisposeMangledSymbol
                 LLVMOrcCreateLazyCompileCallback
                 LLVMOrcCreateIndirectStub
                 LLVMOrcSetIndirectStubPointer
                 LLVMOrcAddEagerlyCompiledIR
                 LLVMOrcAddLazilyCompiledIR
                 LLVMOrcAddObjectFile
                 LLVMOrcRemoveModule
                 LLVMOrcGetSymbolAddress
                 LLVMOrcGetSymbolAddressIn
                 LLVMOrcDisposeInstance
                 LLVMOrcRegisterJITEventListener
                 LLVMOrcUnregisterJITEventListener
                 ]}
       )}.read("llvm").eval(env).print;

    version(none)
    q{(do
         (LLVMInitializeX86TargetInfo)
         (LLVMInitializeX86Target)
         (LLVMInitializeX86TargetMC)
         (LLVMInitializeX86AsmPrinter)
         (LLVMInitializeX86AsmParser)
         (def mod (LLVMModuleCreateWithName "test"))
         (def param-types (array LLVMTypeRef 2u))
         (aset param-types 0u (LLVMInt32Type))
         (aset param-types 1u (LLVMInt32Type))
         (def ret-type (LLVMFunctionType (LLVMInt32Type) (.ptr param-types) 2u 0))
         (def sum (LLVMAddFunction mod "sum" ret-type))
         (def entry (LLVMAppendBasicBlock sum "entry"))
         (def builder (LLVMCreateBuilder))
         (LLVMPositionBuilderAtEnd builder entry)
         (def tmp (LLVMBuildAdd builder (LLVMGetParam sum 0) (LLVMGetParam sum 1) "tmp"))
         (LLVMBuildRet builder tmp)
         (def error (@UInt8))
         (LLVMVerifyModule mod LLVMAbortProcessAction &error)
         (LLVMDisposeMessage error)
         (def error (@UInt8)) ;; TODO reset var
         #_(def engine (LLVMExecutionEngineRef))
         (def triple (LLVMGetDefaultTargetTriple))
         (def target (LLVMTargetRef))
         (LLVMGetTargetFromTriple triple &target &error)
         (def tm (LLVMCreateTargetMachine target triple (LLVMGetHostCPUName) (LLVMGetHostCPUFeatures) LLVMCodeGenLevelDefault LLVMRelocDefault LLVMCodeModelDefault))
         (LLVMSetTarget mod triple)
         (LLVMSetDataLayout mod (LLVMCopyStringRepOfTargetData (LLVMCreateTargetDataLayout tm)))

         #_(LLVMCreateJITCompilerForModule &engine mod 2u &error)
         #_(LLVMDisposeMessage error)
         #_(def args (array LLVMGenericValueRef 2u))
         #-(aset args 0u (LLVMCreateGenericValueOfInt (LLVMInt32Type) 10u 0))
         #_(aset args 1u (LLVMCreateGenericValueOfInt (LLVMInt32Type) 10u 0))
         #_(def res (LLVMRunFunction engine sum 2u (.ptr args)))
         #_(print (LLVMGenericValueToInt res 0))
       )}.read("llvm test").eval(env);

    version(none)
    q{(ffi C
       {:lib     "/usr/local/lib/libjit.dylib"
        :include ["/usr/local/include/jit/jit.h"]
        :import  [jit_init jit_uses_interpreter jit_supports_threads
                  jit_supports_virtual_memory jit_context_t jit_context_create
                  jit_context_destroy jit_context_build_start
                  jit_context_build_end jit_context_set_on_demand_driver
                  jit_context_set_memory_manager jit_context_set_meta
                  jit_context_set_meta_numeric jit_context_get_meta
                  jit_context_get_meta_numeric jit_context_free_meta
                  jit_function_create jit_function_create_nested
                  jit_function_abandon jit_function_get_context
                  jit_function_get_signature jit_function_set_meta
                  jit_function_set_meta jit_function_free_meta jit_function_next
                  jit_function_previous jit_function_get_entry
                  jit_function_get_current #_jit_function_get_nested_current
                  jit_function_is_compiled jit_function_set_recompilable
                  jit_function_clear_recompilable jit_function_is_recompilable
                  jit_function_to_closure jit_function_from_closure
                  jit_function_from_pc jit_function_to_vtable_pointer
                  jit_function_from_vtable_pointer
                  jit_function_set_on_demand_compiler
                  jit_function_get_on_demand_compiler
                  jit_function_apply jit_function_apply_vararg
                  jit_function_set_optimization_level
                  jit_function_get_optimization_level
                  jit_function_get_max_optimization_level
                  jit_function_reserve_label jit_function_labels_equal
                  #_jit_optimize #_jit_compile_entry jit_function_setup_entry
                  jit_function_compile jit_function_compile_entry
                  jit_type_void jit_type_sbyte jit_type_ubyte jit_type_short
                  jit_type_ushort jit_type_int jit_type_uint jit_type_nint
                  jit_type_nuint jit_type_long jit_type_float32 jit_type_float64
                  jit_type_nfloat jit_type_void_ptr jit_type_copy jit_type_free
                  jit_type_sys_bool jit_type_sys_char jit_type_sys_schar
                  jit_type_sys_short jit_type_sys_ushort jit_type_sys_int
                  jit_type_sys_uint jit_type_sys_long jit_type_sys_ulong
                  jit_type_sys_longlong jit_type_sys_ulonglong
                  jit_type_sys_float jit_type_sys_double
                  jit_type_sys_long_double jit_type_create_struct
                  jit_type_create_union jit_type_create_signature
                  jit_type_create_pointer jit_type_create_tagged jit_type_set_names
                  jit_type_set_size_and_alignment jit_type_set_offset
                  jit_type_get_kind jit_type_get_size jit_type_get_alignment
                  jit_type_num_fields jit_type_get_field jit_type_get_offset
                  jit_type_get_name jit_type_find_name jit_type_num_params
                  jit_type_get_return jit_type_get_param jit_type_get_abi
                  jit_type_get_ref jit_type_get_tagged_type jit_type_set_tagged_type
                  jit_type_get_tagged_kind jit_type_get_tagged_data
                  jit_type_set_tagged_data jit_type_is_primitive jit_type_is_struct
                  jit_type_is_union jit_type_is_signature jit_type_is_pointer
                  jit_type_is_tagged jit_type_best_alignment jit_type_normalize
                  jit_type_remove_tags jit_type_promote_int
                  jit_type_return_via_pointer jit_type_has_tag
                  jit_value_create jit_value_create_nint_constant
                  jit_value_create_long_constant jit_value_create_float32_constant
                  jit_value_create_float64_constant jit_value_create_nfloat_constant
                  jit_value_create_constant jit_value_get_param
                  jit_value_get_struct_pointer jit_value_is_temporary
                  jit_value_is_local jit_value_is_constant jit_value_is_parameter
                  jit_value_ref jit_value_set_volatile jit_value_is_volatile
                  jit_value_set_addressable jit_value_is_addressable
                  jit_value_get_type jit_value_get_function jit_value_get_block
                  jit_value_get_context jit_value_get_nint_constant
                  jit_value_get_long_constant jit_value_get_float32_constant
                  jit_value_get_float64_constant jit_value_get_nfloat_constant
                  jit_value_is_true #_jit_constant_revert
                  #_jit_insn_opcode jit_insn_get_dest jit_insn_get_value1
                  jit_insn_get_value2 jit_insn_get_label jit_insn_get_function
                  jit_insn_get_native jit_insn_get_name jit_insn_get_signature
                  jit_insn_dest_is_value jit_insn_label jit_insn_label_tight
                  jit_insn_new_block jit_insn_nop jit_insn_load jit_insn_dup
                  #_jit_insn_load_small jit_insn_store jit_insn_load_relative
                  jit_insn_store_relative jit_insn_add_relative jit_insn_load_elem
                  jit_insn_load_elem_address jit_insn_store_elem
                  jit_insn_check_null jit_insn_add jit_insn_add_ovf jit_insn_sub
                  jit_insn_sub_ovf jit_insn_mul jit_insn_mul_ovf jit_insn_div
                  jit_insn_div jit_insn_rem jit_insn_rem_ieee jit_insn_neg
                  jit_insn_and jit_insn_or jit_insn_xor jit_insn_not
                  jit_insn_shl jit_insn_shr jit_insn_ushr jit_insn_sshr
                  jit_insn_eq jit_insn_ne jit_insn_lt jit_insn_le jit_insn_gt
                  jit_insn_ge jit_insn_cmpl jit_insn_cmpg jit_insn_to_bool
                  jit_insn_to_not_bool jit_insn_acos jit_insn_asin jit_insn_atan
                  jit_insn_atan2 jit_insn_cos jit_insn_cosh jit_insn_exp
                  jit_insn_log jit_insn_log10 jit_insn_pow jit_insn_sin
                  jit_insn_sinh jit_insn_sqrt jit_insn_tan jit_insn_tanh
                  jit_insn_ceil jit_insn_floor jit_insn_rint jit_insn_round
                  jit_insn_trunc jit_insn_is_nan jit_insn_is_finite jit_insn_is_inf
                  jit_insn_abs jit_insn_min jit_insn_max jit_insn_sign
                  jit_insn_branch jit_insn_branch_if jit_insn_branch_if_not
                  jit_insn_jump_table jit_insn_address_of jit_insn_address_of_label
                  jit_insn_convert jit_insn_call jit_insn_call_indirect
                  jit_insn_call_indirect_vtable jit_insn_call_native
                  jit_insn_call_intrinsic jit_insn_incoming_reg
                  jit_insn_incoming_frame_posn jit_insn_outgoing_reg
                  jit_insn_outgoing_frame_posn jit_insn_return_reg
                  jit_insn_setup_for_nested jit_insn_flush_struct
                  jit_insn_import jit_insn_push jit_insn_push_ptr
                  jit_insn_set_param jit_insn_set_param_ptr
                  jit_insn_push_return_area_ptr jit_insn_pop_stack
                  jit_insn_defer_pop_stack jit_insn_flush_defer_pop
                  jit_insn_return jit_insn_return_ptr jit_insn_default_return
                  jit_insn_get_call_stack jit_insn_thrown_exception
                  jit_insn_uses_catcher jit_insn_start_catcher
                  jit_insn_branch_if_pc_not_in_range
                  jit_insn_rethrow_unhandled jit_insn_start_finally
                  jit_insn_return_from_finally jit_insn_call_finally
                  jit_insn_start_filter jit_insn_return_from_filter
                  jit_insn_call_filter jit_insn_memcpy jit_insn_memmove
                  jit_insn_memset jit_insn_alloca jit_insn_move_blocks_to_end
                  jit_insn_move_blocks_to_start jit_insn_mark_offset
                  jit_insn_iter_init jit_insn_iter_init_last
                  jit_insn_iter_next jit_insn_iter_previous
                  jit_block_get_function jit_block_get_context jit_block_get_label
                  jit_block_get_next_label jit_block_next jit_block_previous
                  jit_block_from_label jit_block_set_meta jit_block_get_meta
                  jit_block_free_meta jit_block_is_reachable
                  jit_block_ends_in_dead jit_block_current_is_dead
                  jit_function_t jit_insn_t
                  JIT_CALL_NOTHROW JIT_CALL_NORETURN JIT_CALL_TAIL
                  JIT_TYPETAG_NAME JIT_TYPETAG_STRUCT_NAME JIT_TYPETAG_UNION_NAME
                  JIT_TYPETAG_ENUM_NAME JIT_TYPETAG_CONST JIT_TYPETAG_VOLATILE
                  JIT_TYPETAG_REFERENCE JIT_TYPETAG_OUTPUT JIT_TYPETAG_RESTRICT
                  JIT_TYPETAG_SYS_BOOL JIT_TYPETAG_SYS_CHAR
                  JIT_RESULT_OK
                  JIT_RESULT_COMPILE_ERROR
                  JIT_RESULT_OUT_OF_MEMORY
                  JIT_OPTION_CACHE_LIMIT
                  JIT_OPTION_CACHE_PAGE_SIZE
                  JIT_OPTION_PRE_COMPILE
                  JIT_OPTION_DONT_FOLD
                  JIT_OPTION_POSITION_INDEPENDENT]}
       )}.read("jit").eval(env);

    version(none)
    version (OSX)
    q{(ffi ObjC
       {:lib "/usr/lib/libobjc.dylib"
        :include ["objc/runtime.h"]
        :import [objc_class Class objc_object id objc_selector
                 SEL IMP BOOL sel_getName sel_registerName
                 object_getClassName object_getIndexedIvars
                 sel_isMapped sel_getUid objc_method Method
                 objc_ivar Ivar objc_category Category objc_property
                 objc_property_t Protocol object_copy object_dispose
                 object_getClass object_setClass object_isClass
                 object_getIvar object_setIvarWithStrongDefault
                 object_setInstanceVariable
                 object_setInstanceVariableWithStrongDefault
                 object_getInstanceVariable objc_getMetaClass objc_lookUpClass
                 objc_getRequiredClass objc_getClassList objc_copyClassList
                 class_getName class_isMetaClass class_getSuperclass
                 class_setSuperclass class_getVersion class_setVersion
                 class_getInstanceSize class_getClassVariable
                 class_copyIvarList class_getInstanceMethod
                 class_getClassMethod class_getMethodImplementation
                 class_getMethodImplementation_stret class_respondsToSelector
                 class_copyMethodList class_conformsToProtocol
                 class_copyProtocolList class_getProperty class_copyPropertyList
                 class_getIvarLayout class_getWeakIvarLayout class_addMethod
                 class_replaceMethod class_addIvar class_addProtocol
                 class_addProperty class_replaceProperty class_setIvarLayout
                 objc_property_attribute_t
                 class_setWeakIvarLayout objc_getFutureClass
                 class_createInstance objc_constructInstance
                 objc_destructInstance objc_allocateClassPair
                 objc_registerClassPair objc_duplicateClass objc_disposeClassPair
                 method_getName method_getImplementation method_getTypeEncoding
                 method_getNumberOfArguments method_copyReturnType
                 method_copyArgumentType method_getDescription
                 method_setImplementation method_exchangeImplementations
                 ivar_getName ivar_getTypeEncoding ivar_getOffset
                 property_getName property_getAttributes
                 property_copyAttributeList property_copyAttributeValue
                 objc_getProtocol objc_copyProtocolList
                 protocol_conformsToProtocol protocol_isEqual protocol_getName
                 sel_getName sel_registerName sel_isEqual]}
       {:lib     "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation"
        :include ["CoreFoundation/CoreFoundation.h"]
         :import []}
       {:lib     "/System/Library/Frameworks/AVFoundation.framework/AVFoundation"
        :include ["AVFoundation/AVFoundation.h"]
        :import []}
       {:lib "/System/Library/Frameworks/Foundation.framework/Foundation"
        :include ["Foundation/Foundation.h"]
        :import [NSInteger NSUInteger NSDecimal NSDecimalNumber NSNumber
                 NSNumberFormatter NSData NSMutableData NSURL NSURLComponents
                 NSURLQueryItem NSUUID CGFloat NSPoint NSSize NSRect
                 NSAffineTransform NSEdgeInsets NSRange NSString NSMutableString
                 NSStringExtensionMethods NSStringEncodingDetection
                 NSItemProvider NSMutableStringExtensionMethods
                 NSExtendedStringPropertyListParsing NSSimpleCString
                 NSConstantString NSRecursiveLock NSLocking NSCopying
                 NSSecureCoding
                 NSAttributedString NSMutableAttributedString NSCharacterSet
                 NSMutableCharacterSet unichar NSLinguisticTagger NSScanner
                 NSRegularExpression NSDataDetector NSTextCheckingResult
                 #_NSNotFound NSSpellServer #_NSSpellServerDelegate NSLocale
                 NSOrthography #_NSLocalizedString #_NSLocalizedStringFromTable
                 #_NSLocalizedStringFromTableInBundle
                 #_NSLocalizedStringWithDefaultValue NSObject NSCoderMethods
                 NSDiscardableContentProxy
                 NSArray NSMutableArray NSDictionary NSMutableDictionary
                 NSSet NSMutableSet NSIndexPath NSIndexSet NSMutableIndexSet
                 NSCountedSet NSOrderedSet NSMutableOrderedSet NSCache
                 NSPurgeableData NSPointerArray NSMapTable NSEnumerator
                 NSExtendedEnumerator NSValue NSValueCreation
                 NSValueExtensionMethods NSNumberCreation NSValueRangeExtensions
                 NSExtendedArray NSArrayCreation NSExtendedMutableArray
                 NSMutableArrayCreation
                 NSFastEnumeration NSEnumerationOptions #_NSSortOptions
                 NSNull NSDate NSDateInterval NSTimeInterval NSDateComponents
                 NSCalendar NSTimeZone NSDateFormatter NSDateComponentsFormatter
                 NSDateIntervalFormatter NSISO8601DateFormatter NSLocale
                 NSMeasurement NSUnit NSDimension NSUnitConverter
                 NSUnitConverterLinear NSUnitArea NSUnitLength NSUnitVolume
                 NSUnitAngle NSUnitMass NSUnitPressure NSUnitAcceleration
                 NSUnitDuration NSUnitFrequency NSUnitSpeed NSUnitEnergy
                 NSPredicate
                 NSProcessInfo]}
       {:lib "/System/Library/Frameworks/CloudKit.framework/CloudKit"
        :include []
        :import []}
       {:lib "/System/Library/Frameworks/AppKit.framework/AppKit"
        :include ["AppKit/AppKit.h"]
        :import [NSApplication NSRunningApplication NSApplicationDelegate
                 NSApplicationMain NSApplicationLoad NSShowsServicesMenuItem
                 NSSetShowsServicesMenuItem NSUpdateDynamicServices
                 NSPerformService NSServiceProviderName
                 NSApplicationActivationPolicy
                 NSAppKitVersionNumber NSDocumentController
                 NSPersistentDocument NSDocument NSPasteboard NSPasteboardItem
                 NSPasteboardReading NSPasteboardWriting #_NSPasteboardDataProvider
                 NSPasteboardContentsOptions #_NSPasteboardTypeOwner
                 NSUserDefaultsController NSSharingService NSSharingServicePicker
                 NSServicesMenuRequestor NSRegisterServicesProvider
                 NSUnregisterServicesProvider NSUpdateDynamicServices
                 NSServiceProviderName NSPerformService NSHelpManager
                 NSUserInterfaceItemSearching NSObjectController NSController
                 NSTreeController NSTreeNode NSArrayController
                 NSDictionaryController NSDictionaryControllerKeyValuePair
                 NSPlaceholders
                 NSFilePromiseProvider NSFilePromiseProviderDelegate
                 NSFilePromiseReceiver NSCloudSharingValidation
                 NSCloudSharingServiceDelegate NSStoryboard NSStoryboardSegue
                 NSSeguePerforming NSDataAsset NSNib #_NSNibConnector
                 #_NSNibControlConnector #_NSNibOutletConnector NSView NSControl
                 NSCell NSActionCell NSGridView NSGridCell NSGridColumn
                 NSGridRow NSSplitView NSStackView NSTabView NSScrollView
                 NSScroller NSClipView NSRulerView NSRulerMarker NSBrowser
                 NSBrowserDelegate NSBrowserCell NSCollectionView
                 NSCollectionViewSectionHeaderView NSCollectionViewDataSource
                 NSCollectionViewDelegate NSCollectionViewItem
                 NSCollectionViewElement NSCollectionViewFlowLayout
                 NSCollectionViewDelegateFlowLayout NSCollectionViewGridLayout
                 NSCollectionViewTransitionLayout NSCollectionViewLayoutAttributes
                 NSCollectionViewLayout NSCollectionViewUpdateItem
                 NSCollectionViewLayoutInvalidationContext
                 NSCollectionViewFlowLayoutInvalidationContext NSOutlineView
                 NSOutlineViewDataSource NSOutlineViewDelegate #_NSOpenGLView
                 NSTableView NSTableCellView NSTableViewDataSource
                 NSTableViewDelegate NSTableHeaderView NSTableHeaderCell
                 NSTableRowView NSTableColumn NSTableViewRowAction
                 NSTableColumnResizingOptions NSTextView NSButton NSColorWell
                 NSDatePicker NSDatePickerCell NSDatePickerCellDelegate
                 NSImageView NSLevelIndicator NSPathControl
                 NSPathControlDelegate NSPathCell NSPathCellDelegate
                 NSPathComponentCell NSPathControlItem NSPopUpButton
                 NSProgressIndicator NSRuleEditor NSPredicateEditor
                 NSSearchField NSSearchFieldCell NSTextFinder
                 NSTextFinderBarContainer NSTextFinderClient
                 NSSegmentedControl NSSlider NSSliderCell NSSliderAccessory
                 #_NSSliderAccessoryBehaviour NSSliderAccessoryWidth
                 NSStepper NSTextField NSSecureTextField NSTextFieldDelegate
                 NSControlTextEditingDelegate NSTextFieldCell
                 NSSecureTextFieldCell NSTokenField NSTokenFieldDelegate
                 NSTokenFieldCell NSTokenFieldCellDelegate NSToolbar
                 NSToolbarItemValidation NSToolbarItem NSToolbarItemGroup
                 NSComboBox NSComboBoxDataSource NSComboBoxDelegate
                 NSComboBoxCell NSComboBoxCellDataSource NSMatrix
                 NSLayoutConstraint NSLayoutGuide NSLayoutDimension
                 NSLayoutAnchor NSLayoutXAxisAnchor NSLayoutYAxisAnchor
                 #_NSDictionaryOfVariableBindings NSUserInterfaceCompression
                 NSVisualEffectView NSBox NSAppearance
                 NSAppearanceCustomization NSUserInterfaceValidations
                 NSValidatedUserInterfaceItem #_NSViewToolTipOwner
                 NSViewController NSWindowController NSWindowStyleMask
                 NSBackingStoreType
                 NSTitlebarAccessoryViewController NSPageController
                 NSSplitViewController NSSplitView NSSplitViewItem
                 NSStackView NSTabViewController NSTabView NSTabViewItem
                 NSEditor NSEditorRegistration NSMediaLibraryBrowserController
                 NSMenu NSMenuItem NSMenuDelegate #_NSMenuItemValidation
                 NSStatusBar NSStatusItem NSStatusBarButton NSCursor
                 NSTrackingArea NSDockTile NSDockTilePlugIn NSWindow NSPanel
                 NSWindowDelegate NSWindowTab NSWindowTabGroup
                 NSWindowRestoration NSUserInterfaceItemIdentification
                 NSScreen NSPopover NSPopoverDelegate NSAlert NSAlertDelegate
                 NSOpenPanel NSSavePanel NSOpenSavePanelDelegate
                 NSPDFPanel NSPrintPanelAccessorizing NSColorPanel
                 NSColorPickingCustom NSColorPickingDefault NSColorPicker
                 NSFontPanel NSFontPanelModeMask #_NSFontPanelValidation
                 #_NSFontChanging #_NSTouchBar NSTouchBarDelegate
                 NSTouchBarProvider NSTouchBarItem NSCandidateListTouchBarItem
                 NSColorPickerTouchBarItem NSCustomTouchBarItem
                 NSGroupTouchBarItem NSPopoverTouchBarItem
                 NSSharingServicePickerTouchBarItem NSSliderTouchBarItem
                 NSUserInterfaceCompressionOptions NSScrubber
                 NSScrubberDataSource NSScrubberDelegate
                 NSScrubberArrangedView NSScrubberImageItemView
                 NSScrubberSelectionStyle NSScrubberSelectionView
                 NSScrubberTextItemView NSScrubberFlowLayout
                 NSScrubberFlowLayoutDelegate NSScrubberProportionalLayout
                 NSScrubberLayoutAttributes NSScrubberLayout NSViewAnimation
                 NSAnimatablePropertyContainer NSAnimationContext
                 NSAnimationProgress NSAnimationEffect
                 NSViewControllerPresentationAnimator NSAnimation
                 NSAnimationDelegate NSShowAnimationEffect NSSound NSBeep
                 NSSpeechRecognizer NSSpeechSynthesizer
                 NSHapticFeedbackManager NSHapticFeedbackPerformer
                 NSAlignmentFeedbackToken NSAlignmentFeedbackFilter
                 NSResponder NSEvent NSTouch NSPressureConfiguration
                 NSEventMask NSEventButtonMask NSEventModifierFlags
                 NSEventPhase NSEventSwipeTrackingOptions
                 NSClickGestureRecognizer NSPressGestureRecognizer
                 NSPanGestureRecognizer NSRotationGestureRecognizer
                 NSMagnificationGestureRecognizer NSGestureRecognizer
                 NSGestureRecognizerDelegate NSDraggingSource NSDraggingItem
                 NSDraggingSession NSDraggingImageComponent
                 NSDraggingDestination NSDraggingInfo
                 NSSpringLoadingDestination #_NSAccessibility
                 NSAccessibilityContainsTransientUI
                 NSAccessibilityElementLoading NSAccessibilityElement
                 NSAccessibilityButton NSAccessibilityCheckBox
                 NSAccessibilityImage NSAccessibilityList
                 NSAccessibilityNavigableStaticText NSAccessibilityOutline
                 NSAccessibilityProgressIndicator
                 NSAccessibilityRadioButton NSAccessibilityRow
                 NSAccessibilitySlider NSAccessibilityStaticText
                 NSAccessibilityStepper NSAccessibilitySwitch
                 NSAccessibilityTable NSAccessibilityAnnotationAttributeKey
                 NSAccessibilityAnnotationPosition NSAccessibilityGroup
                 NSAccessibilityCustomAction NSAccessibilityCustomRotor
                 NSAccessibilityCustomRotorItemResult
                 NSAccessibilityCustomRotorSearchParameters
                 NSAccessibilityCustomRotorType
                 NSAccessibilityCustomRotorSearchDirection
                 NSAccessibilityCustomRotorItemSearchDelegate
                 NSAccessibilityLayoutArea NSAccessibilityLayoutItem
                 NSAccessibilityAttributeName NSAccessibilityFontAttributeKey
                 NSAccessibilityLoadingToken NSAccessibilityOrientationValue
                 NSAccessibilityParameterizedAttributeName NSAccessibilityRole
                 NSAccessibilitySubrole NSAccessibilityRulerMarkerTypeValue
                 NSAccessibilityRulerUnitValue
                 NSAccessibilitySortDirectionValue
                 NSAccessibilityActionDescription
                 NSAccessibilityPostNotification
                 NSAccessibilityPostNotificationWithUserInfo
                 NSAccessibilityRoleDescription
                 NSAccessibilityRoleDescriptionForUIElement
                 NSAccessibilitySetMayContainProtectedContent
                 NSAccessibilityUnignoredChildren
                 NSAccessibilityUnignoredChildrenForOnlyChild
                 NSAccessibilityUnignoredDescendant
                 NSAccessibilityUnignoredAncestor
                 NSAccessibilityFrameInView NSAccessibilityPointInView
                 NSImage NSImageDelegate NSImageRep NSBitmapImageRep
                 NSCIImageRep NSPICTImageRep NSPDFImageRep NSPDFInfo
                 NSEPSImageRep NSCustomImageRep
                 ]}
       {:lib "/System/Library/Frameworks/Cocoa.framework/Cocoa"
           :include ["Cocoa/Cocoa.h"]
           :import []}
       {:lib "/System/Library/Frameworks/CoreAudio.framework/CoreAudio"
        :include ["CoreAudio/CoreAudio.h"]
        :import [COREAUDIOTYPES_VERSION CA_PREFER_FIXED_POINT
                 #_kAudio_UnimplementedError
                 #_kAudio_FilePermissionError
                 #_kAudio_TooManyFilesOpenError
                 #_kAudio_BadFilePathError
                 #_kAudio_ParamError
                 #_kAudio_MemFullError
                 AudioValueRange #_AudioValueTransition AudioBufferList AudioBuffer
                 AudioSampleType AudioUnitSampleType #_kAudioUnitSampleFractionBits
                 AudioFormatID AudioFormatFlags AudioStreamBasicDescription
                 #_kAudioStreamAnyRate AudioFormatID #_TestAudioFormatNativeEndian
                 AudioStreamPacketDescription SMPTETimeType SMPTETimeFlags
                 SMPTETime AudioTimeStampFlags AudioTimeStamp
                 AudioClassDescription AudioChannelLabel AudioChannelLayoutTag
                 AudioChannelBitmap AudioChannelFlags AudioChannelCoordinateIndex
                 AudioChannelDescription AudioChannelLayout
                 #_AudioChannelLayoutTag_GetNumberOfChannels
                 MPEG4ObjectID AudioObjectID AudioClassID
                 AudioObjectPropertySelector AudioObjectPropertyScope
                 AudioObjectPropertyElement AudioObjectPropertyAddress
                 kAudioEndPointDeviceUIDKey
                 kAudioEndPointDeviceNameKey
                 kAudioEndPointDeviceEndPointListKey
                 kAudioEndPointDeviceMasterEndPointKey
                 kAudioEndPointDeviceIsPrivateKey
                 kAudioEndPointUIDKey
                 kAudioEndPointNameKey
                 kAudioEndPointInputChannelsKey
                 kAudioEndPointOutputChannelsKey
                 AudioStreamRangedDescription AudioObjectPropertyListenerProc
                 AudioObjectPropertyListenerBlock AudioObjectShow
                 AudioObjectHasProperty AudioObjectIsPropertySettable
                 AudioObjectGetPropertyDataSize AudioObjectGetPropertyData
                 AudioObjectSetPropertyData AudioObjectAddPropertyListener
                 AudioObjectRemovePropertyListener
                 AudioObjectAddPropertyListenerBlock
                 AudioObjectRemovePropertyListenerBlock AudioHardwarePowerHint
                 AudioHardwareUnload AudioHardwareCreateAggregateDevice
                 AudioHardwareDestroyAggregateDevice AudioDeviceIOProc
                 AudioDeviceIOBlock AudioDeviceIOProcID
                 AudioHardwareIOProcStreamUsage]}
       {:lib "/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics"
        :include ["CoreGraphics/CGDirectDisplay.h"]
        :import [CGMainDisplayID]}
       {:lib "/System/Library/Frameworks/QuartzCore.framework/QuartzCore"
        :include ["QuartzCore/CAMetalLayer.h"
                  "CoreVideo/CVDisplayLink.h"]
        :import [CALayer CALayerDelegate CAMetalLayer CAMetalDrawable
                 CVTimeStamp CVReturn CVDisplayLinkRef CVDisplayLinkIsRunning
                 CVDisplayLinkCreateWithActiveCGDisplays
                 CVDisplayLinkSetOutputCallback CVDisplayLinkOutputCallback
                 CVDisplayLinkSetCurrentCGDisplay
                 CVDisplayLinkStart CVDisplayLinkStop CVDisplayLinkRelease]}
       {:lib "/System/Library/Frameworks/Metal.framework/Metal"
        :include ["Metal/Metal.h"]
        :import [MTLCreateSystemDefaultDevice
                 MTLDevice MTLCommandQueue MTLCommandBuffer MTLCommandEncoder
                 MTLRenderCommandEncoder MTLParallelRenderCommandEncoder
                 MTLRenderPassDescriptor MTLRenderPassAttachmentDescriptor
                 MTLRenderPassColorAttachmentDescriptor
                 MTLRenderPassColorAttachmentDescriptorArray
                 MTLRenderPassDepthAttachmentDescriptor
                 MTLRenderPassStencilAttachmentDescriptor
                 MTLRenderPipelineState MTLRenderPipelineDescriptor
                 MTLRenderPipelineColorAttachmentDescriptor
                 MTLRenderPipelineColorAttachmentDescriptorArray
                 MTLDepthStencilState
                 MTLDepthStencilDescriptor
                 MTLStencilDescriptor
                 MTLVertexDescriptor MTLVertexAttribute
                 MTLVertexAttributeDescriptor MTLVertexAttributeDescriptorArray
                 MTLVertexBufferLayoutDescriptor
                 MTLVertexBufferLayoutDescriptorArray MTLDrawable
                 MTLComputePipelineDescriptor MTLComputePipelineState
                 MTLComputeCommandEncoder MTLFunction MTLFunctionConstant
                 MTLFunctionConstantValues MTLLibrary MTLCompileOptions
                 MTLStageInputOutputDescriptor MTLAttribute
                 MTLAttributeDescriptor MTLAttributeDescriptorArray
                 MTLBufferLayoutDescriptor MTLBufferLayoutDescriptorArray
                 MTLPipelineOption MTLComputePipelineReflection
                 MTLRenderPipelineReflection MTLArgument MTLType MTLStructType
                 MTLStructMember MTLArrayType MTLPointerType
                 MTLTextureReferenceType MTLBlitCommandEncoder MTLResource
                 MTLBuffer MTLPipelineBufferDescriptor
                 MTLPipelineBufferDescriptorArray MTLTexture MTLTextureDescriptor
                 MTLPixelFormat MTLSamplerState MTLSamplerDescriptor MTLHeap
                 MTLHeapDescriptor MTLFence MTLSizeAndAlign MTLArgumentEncoder
                 MTLArgumentDescriptor MTLOrigin MTLSize MTLRegion]
        })}.read("#<Test>").eval(env);

    try {
    version (none)
    version (OSX)
    q{(do
         (print "Hello objc")
         (data AppDelegate :extends NSObject
          ;; (NSWindow* window)
          NSApplicationDelegate
          (applicationWillFinishLaunching: [self notification]
           (puts "Will Finish")
           ;(let [dg (new WindowDelegate)]
           ; )
           )
          (applicationDidFinishLaunching: [self notification]
           (puts "Did Finish"))
          (applicationWillBecomeActive: [self notification]
           (puts "Will Active"))
          (applicationDidBecomeActive: [self notification]
           (puts "Did Active"))
          (applicationWillResignActive: [self notification]
           (puts "Will Unactive"))
          (applicationDidResignActive: [self notification]
           (puts "Did Unactive"))
          (applicationShouldTerminate: [self sender]
           (puts "Terminate")
           ;;(. NSApplicationTerminateReply NSTerminateNow)
           0)
          (applicationShouldTerminateAfterLastWindowClosed: [self sender]
           (puts "Should Terminate")
           true)
          (applicationWillTerminate: [self notification]
           (puts "Will Term"))
          (applicationWillHide: [self notification]
           (puts "Will Hide"))
          (applicationDidHide: [self notification]
           (puts "Did Hide"))
          (applicationWillUnhide: [self notification]
           (puts "Will Unhide"))
          (applicationDidUnhide: [self notification]
           (puts "Did Unhide"))
          (applicationWillUpdate: [self notification]
           (puts "Will Update"))
          (applicationDidUpdate: [self notification]
           (puts "Did Update"))
          (applicationShouldHandleReopen:hasVisibleWindows: [self sender flag]
           (puts "Should Handle Reopen")
           false)
          (application:willPresentError: [self app error]
           (puts "Will Present Error")))

         (data WindowDelegate :extends NSObject
          NSWindowDelegate
          #_(window:willPositionSheet:usingRect: [self window sheet rect]
           (NSRect. 0 0 0 0))
          (windowWillBeginSheet: [self notification]
           (puts "Begin sheet"))
          (windowDidEndSheet: [self notification]
           (puts "End sheet"))
          #_(windowWillResize:toSize: [self sender frame-size]
           frame-size)
          (windowDidResize: [self notification]
           )
          (windowWillStartLiveResize: [self notification]
           )
          (windowDidEndLiveResize: [self notification]
           )
          (windowWillMiniaturize: [self notification]
           )
          (windowDidMiniaturize: [self notification]
           )
          (windowDidDeminiaturize: [self notification]
           )
          (windowWillUseStandardFrame:defaultFrame: [self window new-frame]
           )
          (windowShouldZoom:toFrame: [self window new-frame]
           ))

          (def ui-thread (pthread_t))
          (def ui-entry
            (fn ^:cdecl ^@Void entry [^@Void _]
             (def app (. NSApplication sharedApplication))
             (def app-dg (AppDelegate))
             (. app setDelegate: app-dg)
             (. app setActivationPolicy: NSApplicationActivationPolicyRegular)

             (def app-name (. (. NSProcessInfo processInfo) processName))
             (def app-menu (NSMenu))
             (def app-item (NSMenuItem))
             (. app-item setSubmenu: app-menu)

             (def menubar (NSMenu))
             (. app setMainMenu: menubar)
             (. menubar addItem: app-item)

             (def wnd (NSWindow))
             (def wnd-dg (WindowDelegate))
             (. wnd initWithContentRect:styleMask:backing:defer:
              (NSRect (NSPoint 0.0 0.0) (NSSize 200.0 200.0))
              1u #_NSWindowStyleMaskTitled NSBackingStoreBuffered false)
             (. wnd setDelegate: wnd-dg)
             (. wnd setTitle: app-name)
             (. wnd makeKeyAndOrderFront: nil)
             (. app activateIgnoringOtherApps: true)
             (. app run)))
          (pthread_create &ui-thread nil ui-entry nil)
       )}.read("#do").eval(env);
    } catch (Exception e ) writeln(e.next);

    // version(none)
    version (Windows)
    q{(ffi C++
       {:flags ["-DWIN32_MEAN_AND_LEAN"]}
       {:lib "kernel32.dll"
        :include ["windows.h" "psapi.h"]
        :import [;; Debugging
                 ; Events
                 CREATE_PROCESS_DEBUG_EVENT
                 CREATE_THREAD_DEBUG_EVENT
                 EXCEPTION_DEBUG_EVENT
                 EXIT_PROCESS_DEBUG_EVENT
                 EXIT_THREAD_DEBUG_EVENT
                 LOAD_DLL_DEBUG_EVENT
                 OUTPUT_DEBUG_STRING_EVENT
                 UNLOAD_DLL_DEBUG_EVENT
                 RIP_EVENT
                 ; Functions
                 CheckRemoteDebuggerPresent
                 ContinueDebugEvent
                 DebugActiveProcess
                 DebugActiveProcessStop
                 DebugBreak
                 DebugBreakProcess
                 DebugSetProcessKillOnExit
                 FatalExit
                 FlushInstructionCache
                 GetThreadContext
                 GetThreadSelectorEntry
                 IsDebuggerPresent
                 OutputDebugStringA OutputDebugStringW
                 ReadProcessMemory
                 SetThreadContext
                 WaitForDebugEvent WaitForDebugEventEx
                 Wow64GetThreadContext
                 Wow64GetThreadSelectorEntry
                 Wow64SetThreadContext
                 WriteProcessMemory
                 ; Structures
                 CONTEXT
                 CREATE_PROCESS_DEBUG_INFO
                 CREATE_THREAD_DEBUG_INFO
                 DEBUG_EVENT
                 EXCEPTION_DEBUG_INFO
                 EXIT_PROCESS_DEBUG_INFO
                 EXIT_THREAD_DEBUG_INFO
                 LDT_ENTRY
                 LOAD_DLL_DEBUG_INFO
                 OUTPUT_DEBUG_STRING_INFO
                 RIP_INFO
                 UNLOAD_DLL_DEBUG_INFO
                 WOW64_CONTEXT
                 WOW64_FLOATING_SAVE_AREA
                 WOW64_LDT_ENTRY
                 ;; Dynamic-Link Library
                 AddDllDirectory
                 DisableThreadLibraryCalls
                 FreeLibrary
                 FreeLibraryAndExitThread
                 GetDllDirectoryA GetDllDirectoryW
                 GetModuleFileNameA GetModuleFileNameW
                 GetModuleFileNameExA GetModuleFileNameExW
                 GetModuleHandleA GetModuleHandleW
                 GetModuleHandleExA GetModuleHandleExW
                 GetProcAddress
                 LoadLibraryA LoadLibraryW
                 LoadLibraryExA
                 LoadLibraryExW
                 LoadPackagedLibrary
                 RemoveDllDirectory
                 SetDefaultDllDirectories
                 SetDllDirectoryA SetDllDirectoryW
                 ;; Error Handling
                 AddVectoredContinueHandler
                 AddVectoredExceptionHandler
                 FatalAppExitA FatalAppExitW
                 GetErrorMode
                 GetLastError
                 GetThreadErrorMode
                 RaiseException
                 RaiseFailFastException
                 RemoveVectoredContinueHandler
                 RemoveVectoredExceptionHandler
                 SetErrorMode
                 SetLastError
                 SetThreadErrorMode
                 SetUnhandledExceptionFilter
                 UnhandledExceptionFilter
                 ;;; Memory Management
                 ;; Global and Local
                 GlobalAlloc LocalAlloc
                 GlobalLock LocalLock
                 GlobalUnlock LocalUnlock
                 GlobalSize LocalSize
                 GlobalReAlloc LocalReAlloc
                 GlobalFree LocalFree
                 GlobalDiscard LocalDiscard
                 GlobalFlags LocalFlags
                 GlobalHandle LocalHandle
                 ;;; Data Exchange
                 ;; Atom
                 ; Functions
                 AddAtomA AddAtomW
                 DeleteAtom
                 FindAtomA FindAtomW
                 GetAtomNameA GetAtomNameW
                 GlobalAddAtomA GlobalAddAtomW
                 GlobalAddAtomExA GlobalAddAtomExW
                 GlobalDeleteAtom
                 GlobalFindAtomA GlobalFindAtomW
                 GlobalGetAtomNameA GlobalGetAtomNameW
                 InitAtomTable
                 ; Macros
                 MAKEINTATOM
                 ;;; Processes and Threads
                 ;; Enumerations
                 CPU_SET_INFORMATION_TYPE
                 #_DISPATCHERQUEUE_THREAD_APARTMENTTYPE
                 #_DISPATCHERQUEUE_THREAD_TYPE
                 LOGICAL_PROCESSOR_RELATIONSHIP
                 JOB_OBJECT_NET_RATE_CONTROL_FLAGS
                 PROCESS_INFORMATION_CLASS
                 PROCESS_MEMORY_EXHAUSTION_TYPE
                 PROCESS_MITIGATION_POLICY
                 PROCESSOR_CACHE_TYPE
                 UMS_THREAD_INFO_CLASS
                 ;; Functions
                 ; Dispatch Queues
                 ;CreateDispatcherQueueController TODO:coremessaging.dll
                 ; Processes
                 CreateProcessA CreateProcessW
                 CreateProcessAsUserA CreateProcessAsUserW
                 #_CreateProcessWithLogonW
                 #_CreateProcessWithTokenW
                 ExitProcess
                 FlushProcessWriteBuffers
                 FreeEnvironmentStringsA FreeEnvironmentStringsW
                 GetCommandLineA GetCommandLineW
                 GetCurrentProcess
                 GetCurrentProcessId
                 GetCurrentProcessorNumber
                 GetEnvironmentStringsA GetEnvironmentStringsW
                 GetEnvironmentVariableA GetEnvironmentVariableW
                 GetExitCodeProcess
                 #_GetGuiResources
                 GetLogicalProcessorInformation
                 GetPriorityClass
                 GetProcessAffinityMask
                 GetProcessGroupAffinity
                 GetProcessHandleCount
                 GetProcessId
                 GetProcessIdOfThread
                 GetProcessIoCounters
                 GetProcessMitigationPolicy
                 GetProcessShutdownParameters
                 GetProcessTimes
                 GetProcessVersion
                 GetProcessWorkingSetSize
                 GetProcessWorkingSetSizeEx
                 GetProcessorSystemCycleTime
                 GetStartupInfoA GetStartupInfoW
                 #_IsImmersiveProcess
                 NeedCurrentDirectoryForExePathA NeedCurrentDirectoryForExePathW
                 OpenProcess
                 QueryFullProcessImageNameA QueryFullProcessImageNameW
                 QueryProcessAffinityUpdateMode
                 QueryProcessCycleTime
                 SetEnvironmentVariableA SetEnvironmentVariableW
                 SetPriorityClass
                 SetProcessAffinityMask
                 SetProcessAffinityUpdateMode
                 SetProcessInformation
                 SetProcessMitigationPolicy
                 SetProcessPriorityBoost
                 #_SetProcessRestrictionExemption
                 SetProcessShutdownParameters
                 SetProcessWorkingSetSize
                 SetProcessWorkingSetSizeEx
                 TerminateProcess
                 PROCESS_ALL_ACCESS
                 ; Process Enumeration
                 EnumProcesses
                 #_Process32First
                 #_Process32Next
                 #_WTSEnumerateProcesses
                 ; Policy
                 QueryProtectedPolicy
                 SetProtectedPolicy
                 ; Thread
                 #_AttachThreadInput
                 CreateRemoteThread
                 CreateRemoteThreadEx
                 CreateThread
                 ExitThread
                 GetCurrentThread
                 GetCurrentThreadId
                 GetExitCodeThread
                 GetThreadDescription
                 GetThreadGroupAffinity
                 GetThreadId
                 GetThreadIdealProcessorEx
                 GetThreadInformation
                 GetThreadIOPendingFlag
                 GetThreadPriority
                 GetThreadPriorityBoost
                 GetThreadTimes
                 OpenThread
                 QueryIdleProcessorCycleTime
                 QueryThreadCycleTime
                 ResumeThread
                 SetThreadAffinityMask
                 SetThreadDescription
                 SetThreadGroupAffinity
                 SetThreadIdealProcessor
                 SetThreadIdealProcessorEx
                 SetThreadInformation
                 SetThreadPriority
                 SetThreadPriorityBoost
                 SetThreadStackGuarantee
                 Sleep
                 SleepEx
                 SuspendThread
                 SwitchToThread
                 TerminateThread
                 TlsAlloc
                 TlsFree
                 TlsGetValue
                 TlsSetValue
                 #_WaitForInputIdle
                 LPTHREAD_START_ROUTINE
                 ; Process and Thread Extended Attributes
                 DeleteProcThreadAttributeList
                 InitializeProcThreadAttributeList
                 UpdateProcThreadAttribute
                 ; WOW64
                 #_IsWow64Message
                 IsWow64Process
                 IsWow64Process2
                 Wow64SuspendThread
                 ; Job Object
                 AssignProcessToJobObject
                 CreateJobObjectA CreateJobObjectW
                 IsProcessInJob
                 OpenJobObjectA OpenJobObjectW
                 QueryInformationJobObject
                 SetInformationJobObject
                 TerminateJobObject
                 #_UserHandleGrantAccess
                 ; Thread Pool
                 CallbackMayRunLong
                 CancelThreadpoolIo
                 CloseThreadpool
                 CloseThreadpoolCleanupGroup
                 CloseThreadpoolCleanupGroupMembers
                 CloseThreadpoolIo
                 CloseThreadpoolTimer
                 CloseThreadpoolWait
                 CloseThreadpoolWork
                 CreateThreadpool
                 CreateThreadpoolCleanupGroup
                 CreateThreadpoolIo
                 CreateThreadpoolTimer
                 CreateThreadpoolWait
                 CreateThreadpoolWork
                 #_DestroyThreadpoolEnvironment
                 DisassociateCurrentThreadFromCallback
                 FreeLibraryWhenCallbackReturns
                 #_InitializeThreadpoolEnvironment
                 IsThreadpoolTimerSet
                 LeaveCriticalSectionWhenCallbackReturns
                 QueryThreadpoolStackInformation
                 ReleaseMutexWhenCallbackReturns
                 ReleaseSemaphoreWhenCallbackReturns
                 SetEventWhenCallbackReturns
                 #_SetThreadpoolCallbackCleanupGroup
                 #_SetThreadpoolCallbackLibrary
                 #_SetThreadpoolCallbackPool
                 #_SetThreadpoolCallbackPriority
                 #_SetThreadpoolCallbackRunsLong
                 SetThreadpoolStackInformation
                 SetThreadpoolThreadMaximum
                 SetThreadpoolThreadMinimum
                 SetThreadpoolTimerEx
                 SetThreadpoolTimer
                 SetThreadpoolWait
                 SetThreadpoolWaitEx
                 StartThreadpoolIo
                 SubmitThreadpoolWork
                 #_TpInitializeCallbackEnviron
                 #_TpDestroyCallbackEnviron
                 #_TpSetCallbackActivationContext
                 #_TpSetCallbackCleanupGroup
                 #_TpSetCallbackFinalizationCallback
                 #_TpSetCallbackLongFunction
                 #_TpSetCallbackNoActivationContext
                 #_TpSetCallbackPersistent
                 #_TpSetCallbackPriority
                 #_TpSetCallbackRaceWithDll
                 TrySubmitThreadpoolCallback
                 WaitForThreadpoolIoCallbacks
                 WaitForThreadpoolTimerCallbacks
                 WaitForThreadpoolWaitCallbacks
                 WaitForThreadpoolWorkCallbacks
                 ; Thread Pooling
                 BindIoCompletionCallback
                 QueueUserWorkItem
                 RegisterWaitForSingleObject
                 UnregisterWaitEx
                 ; Thread Ordering Service
                 #_AvQuerySystemResponsiveness
                 #_AvRtCreateThreadOrderingGroup
                 #_AvRtCreateThreadOrderingGroupEx
                 #_AvRtDeleteThreadOrderingGroup
                 #_AvRtJoinThreadOrderingGroup
                 #_AvRtLeaveThreadOrderingGroup
                 #_AvRtWaitOnThreadOrderingGroup
                 ; Multimedia Class Scheduler Service
                 #_AvRevertMmThreadCharacteristics
                 #_AvSetMmMaxThreadCharacteristics
                 #_AvSetMmThreadCharacteristics
                 #_AvSetMmThreadPriority
                 ; Fiber
                 ConvertFiberToThread
                 ConvertThreadToFiber
                 ConvertThreadToFiberEx
                 CreateFiber
                 CreateFiberEx
                 DeleteFiber
                 FlsAlloc
                 FlsFree
                 FlsGetValue
                 FlsSetValue
                 IsThreadAFiber
                 SwitchToFiber
                 LPFIBER_START_ROUTINE
                 ; NUMA
                 ; TODO
                 ; Processor
                 GetActiveProcessorCount
                 GetActiveProcessorGroupCount
                 GetCurrentProcessorNumber
                 GetCurrentProcessorNumberEx
                 GetLogicalProcessorInformation
                 GetLogicalProcessorInformationEx
                 GetMaximumProcessorCount
                 GetMaximumProcessorGroupCount
                 QueryIdleProcessorCycleTime
                 QueryIdleProcessorCycleTimeEx
                 ; User-Mode Scheduling
                 #_CreateUmsCompletionList
                 #_CreateUmsThreadContext
                 #_DeleteUmsCompletionList
                 #_DeleteUmsThreadContext
                 #_DequeueUmsCompletionListItems
                 #_EnterUmsSchedulingMode
                 #_ExecuteUmsThread
                 #_GetCurrentUmsThread
                 #_GetNextUmsListItem
                 #_GetUmsCompletionListEvent
                 #_GetUmsSystemThreadInformation
                 #_QueryUmsThreadInformation
                 #_SetUmsThreadInformation
                 #_UmsThreadYield
                 #_PUMS_SCHEDULER_ENTRY_POINT
                 ;; Structures
                 ; Process and Thread
                 APP_MEMORY_INFORMATION
                 AR_STATE
                 CACHE_DESCRIPTOR
                 IO_COUNTERS
                 ORIENTATION_PREFERENCE
                 #_PEB
                 #_PEB_LDR_DATA
                 PROCESS_INFORMATION
                 PROCESS_MEMORY_EXHAUSTION_INFO
                 PROCESS_MITIGATION_ASLR_POLICY
                 PROCESS_MITIGATION_BINARY_SIGNATURE_POLICY
                 PROCESS_MITIGATION_CONTROL_FLOW_GUARD_POLICY
                 PROCESS_MITIGATION_DEP_POLICY
                 PROCESS_MITIGATION_DYNAMIC_CODE_POLICY
                 PROCESS_MITIGATION_EXTENSION_POINT_DISABLE_POLICY
                 PROCESS_MITIGATION_FONT_DISABLE_POLICY
                 PROCESS_MITIGATION_IMAGE_LOAD_POLICY
                 PROCESS_MITIGATION_STRICT_HANDLE_CHECK_POLICY
                 PROCESS_MITIGATION_SYSTEM_CALL_DISABLE_POLICY
                 #_RTL_USER_PROCESS_PARAMETERS
                 STARTUPINFO
                 STARTUPINFOEX
                 #_TEB
                 ; Processor
                 CACHE_RELATIONSHIP
                 GROUP_AFFINITY
                 GROUP_RELATIONSHIP
                 NUMA_NODE_RELATIONSHIP
                 PROCESSOR_GROUP_INFO
                 PROCESSOR_NUMBER
                 PROCESSOR_RELATIONSHIP
                 SYSTEM_CPU_SET_INFORMATION
                 SYSTEM_LOGICAL_PROCESSOR_INFORMATION
                 SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX
                 ; Job Object
                 JOBOBJECT_ASSOCIATE_COMPLETION_PORT
                 JOBOBJECT_BASIC_ACCOUNTING_INFORMATION
                 JOBOBJECT_BASIC_AND_IO_ACCOUNTING_INFORMATION
                 JOBOBJECT_BASIC_LIMIT_INFORMATION
                 JOBOBJECT_BASIC_PROCESS_ID_LIST
                 JOBOBJECT_BASIC_UI_RESTRICTIONS
                 JOBOBJECT_END_OF_JOB_TIME_INFORMATION
                 JOBOBJECT_EXTENDED_LIMIT_INFORMATION
                 JOBOBJECT_SECURITY_LIMIT_INFORMATION
                 ; User-Mode Scheduling
                 #_UMS_CREATE_THREAD_ATTRIBUTES
                 #_UMS_SCHEDULER_STARTUP_INFO
                 #_UMS_SYSTEM_THREAD_INFORMATION
                 ;; Sync
                 AcquireSRWLockExclusive AcquireSRWLockShared
                 CancelWaitableTimer
                 CreateEventA CreateEventW
                 CreateEventExA CreateEventExW
                 CreateMutexA CreateMutexW
                 CreateMutexExA CreateMutexExW
                 CreateSemaphoreA CreateSemaphoreW
                 CreateSemaphoreExA CreateSemaphoreExW
                 CreateWaitableTimerA CreateWaitableTimerW
                 CreateWaitableTimerExA CreateWaitableTimerW
                 DeleteCriticalSection
                 DeleteSynchronizationBarrier
                 EnterCriticalSection
                 EnterSynchronizationBarrier
                 InitializeConditionVariable
                 InitializeCriticalSection
                 InitializeCriticalSectionAndSpinCount
                 InitializeCriticalSectionEx
                 InitializeSRWLock
                 InitializeSynchronizationBarrier
                 InitOnceBeginInitialize
                 InitOnceComplete
                 InitOnceExecuteOnce
                 InitOnceInitialize
                 LeaveCriticalSection
                 OpenEventA OpenEventW
                 OpenMutexA OpenMutexW
                 OpenSemaphoreA OpenSemaphoreW
                 OpenWaitableTimerA OpenWaitableTimerW
                 PINIT_ONCE_FN
                 PTIMERAPCROUTINE
                 ReleaseMutex
                 ReleaseSemaphore
                 ReleaseSRWLockExclusive
                 ReleaseSRWLockShared
                 ResetEvent
                 SetCriticalSectionSpinCount
                 SetEvent
                 SetWaitableTimer SetWaitableTimerEx
                 SignalObjectAndWait
                 Sleep
                 SleepConditionVariableCS
                 SleepConditionVariableSRW
                 SleepEx
                 TryAcquireSRWLockExclusive
                 TryAcquireSRWLockShared
                 TryEnterCriticalSection
                 WaitForMultipleObjects WaitForMultipleObjectsEx
                 WaitForSingleObject WaitForSingleObjectEx
                 #_WaitOnAddress
                 WakeAllConditionVariable
                 #_WakeByAddressAll
                 #_WakeByAddressSingle
                 WakeConditionVariable
                 MUTEX_ALL_ACCESS
                 SYNCHRONIZE
                 EVENT_MODIFY_STATE
                 OpenFileMappingA OpenFileMappingW
                 CreateFileMappingA CreateFileMappingW
                 MapViewOfFile MapViewOfFileEx UnmapViewOfFile
                 FILE_MAP_ALL_ACCESS FILE_MAP_EXECUTE FILE_MAP_READ FILE_MAP_WRITE
                 FILE_MAP_COPY FILE_MAP_LARGE_PAGES FILE_MAP_TARGETS_INVALID
                 PAGE_EXECUTE_READ PAGE_EXECUTE_READWRITE PAGE_EXECUTE_WRITECOPY PAGE_READONLY
                 PAGE_READWRITE PAGE_WRITECOPY
                 SEC_COMMIT SEC_IMAGE SEC_IMAGE_NO_EXECUTE SEC_LARGE_PAGES SEC_NOCACHE SEC_RESERVE
                 SEC_WRITECOMBINE
                 #_INVALID_HANDLE_VALUE
                 CloseHandle CompareObjectHandles DuplicateHandle GetHandleInformation SetHandleInformation
                 ;; ?
                 FormatMessageA FormatMessageW
                 FORMAT_MESSAGE_ALLOCATE_BUFFER
                 FORMAT_MESSAGE_ARGUMENT_ARRAY
                 FORMAT_MESSAGE_FROM_HMODULE
                 FORMAT_MESSAGE_FROM_STRING
                 FORMAT_MESSAGE_FROM_SYSTEM
                 FORMAT_MESSAGE_IGNORE_INSERTS
                 FORMAT_MESSAGE_MAX_WIDTH_MASK
                 ;; ?
                 MAKELANGID
                 LANG_NEUTRAL
                 SUBLANG_DEFAULT
                 ]}
       {:lib "user32.dll"
        :include []
        :import [;; Windows Data Types
                 ATOM BOOL BOOLEAN BYTE CCHAR CHAR COLORREF DWORD DWORDLONG DWORD_PTR
                 DWORD32 DWORD64 FLOAT HACCEL HALF_PTR HANDLE HBITMAP HBRUSH HCOLORSPACE
                 HCONV HDC HDDEDATA HDESK HDROP HDWP HENHMETAFILE HFILE HGDIOBJ HGLOBAL
                 HHOOK HICON HINSTANCE HKEY HKL HLOCAL HMENU HMETAFILE HMODULE HMONITOR
                 HPALETTE HPEN HRESULT HRGN HRSRC HSZ HWINSTA HWND INT INT_PTR INT8 INT16
                 INT32 INT64 LANGID LCID LCTYPE LGRPID LONG LONGLONG LONG_PTR LONG32 LONG64
                 LPARAM LPBOOL LPBYTE LPCOLORREF LPCSTR LPCVOID LPCWSTR LPDWORD LPHANDLE
                 LPINT LPLONG LPSTR LPVOID LPWORD LPWSTR LRESULT PBOOL PBOOLEAN PBYTE
                 PCHAR PCSTR PCWSTR PDWORD PDWORDLONG PDWORD_PTR PDWORD32 PDWORD64 PHANDLE
                 PHKEY PINT PINT_PTR PINT8 PINT16 PINT32 PINT64 PLCID PLONGLONG PLONG_PTR
                 PLONG32 PLONG64 #_POINTER_32 #_POINTER_64 #_POINTER_SIGNED #_POINTER_UNSIGNED
                 PSHORT PSIZE_T PSSIZE_T PSTR PUCHAR PUHALF_PTR PUINT PUINT_PTR PUINT8
                 PUINT16 PUINT32 PUINT64 PULONG PULONGLONG PULONG_PTR PULONG32 PULONG64
                 PUSHORT PVOID PWCHAR PWORD PWSTR #_QWORD SC_HANDLE SC_LOCK SERVICE_STATUS_HANDLE
                 SHORT SIZE_T UCHAR UHALF_PTR UINT UINT_PTR UINT8 UINT16 UINT32 UINT64 ULONG
                 ULONGLONG ULONG_PTR ULONG32 ULONG64 #_UNICODE_STRING USHORT USN #_VOID WCHAR
                 WORD WPARAM
                 ;;; Windowing and Messaging
                 ;; Windows
                 ; Functions
                 AdjustWindowRect AdjustWindowRectEx AllowSetForegroundWindow AnimateWindow
                 AnyPopup ArrangeIconicWindows BeginDeferWindowPos BringWindowToTop
                 CalculatePopupWindowPosition CascadeWindows
                 ChangeWindowMessageFilter ChangeWindowMessageFilterEx ChildWindowFromPoint
                 ChildWindowFromPointEx CloseWindow
                 CreateWindowA CreateWindowExA CreateWindowExW CreateWindowW
                 DeferWindowPos DeregisterShellHookWindow DestroyWindow
                 EndDeferWindowPos #_EndTask EnumChildWindows
                 EnumThreadWindows EnumWindows FindWindowA FindWindowExA FindWindowExW FindWindowW
                 GetAltTabInfoA GetAltTabInfoW GetAncestor GetClientRect
                 GetDesktopWindow GetForegroundWindow GetGUIThreadInfo
                 GetLastActivePopup GetLayeredWindowAttributes GetNextWindow GetParent
                 GetProcessDefaultLayout GetShellWindow
                 GetSysColor GetTitleBarInfo GetTopWindow GetWindow
                 GetWindowDisplayAffinity GetWindowInfo GetWindowModuleFileNameA
                 GetWindowModuleFileNameW GetWindowPlacement GetWindowRect GetWindowTextA
                 GetWindowTextW GetWindowTextLengthA GetWindowTextLengthW GetWindowTextW
                 GetWindowThreadProcessId InternalGetWindowText IsChild IsGUIThread IsHungAppWindow IsIconic
                 IsProcessDPIAware IsWindow IsWindowUnicode IsWindowVisible IsZoomed
                 LockSetForegroundWindow LogicalToPhysicalPoint
                 MoveWindow OpenIcon PhysicalToLogicalPoint RealChildWindowFromPoint RealGetWindowClassW
                 RegisterShellHookWindow ReplyMessage SENDASYNCPROC
                 SetForegroundWindow SetLayeredWindowAttributes SetParent
                 SetProcessDefaultLayout SetProcessDPIAware SetSysColors
                 SetWindowDisplayAffinity SetWindowPlacement SetWindowPos
                 SetWindowTextA SetWindowTextW
                 ShowOwnedPopups ShowWindow ShowWindowAsync SoundSentry SwitchToThisWindow
                 TileWindows UpdateLayeredWindow
                 WindowFromPhysicalPoint WindowFromPoint
                 ; Macros
                 #_GET_X_LPARAM #_GET_Y_LPARAM HIBYTE HIWORD LOBYTE LOWORD MAKELONG
                 MAKELPARAM MAKELRESULT MAKEWORD MAKEWPARAM
                 ; Window Messages
                 MN_GETHMENU WM_GETFONT WM_GETTEXT WM_GETTEXTLENGTH WM_SETFONT WM_SETICON
                 WM_SETTEXT
                 ; Window Notifications
                 WM_ACTIVATEAPP WM_CANCELMODE WM_CHILDACTIVATE WM_CLOSE WM_COMPACTING
                 WM_CREATE WM_DESTROY WM_ENABLE WM_ENTERSIZEMOVE WM_ERASEBKGND
                 WM_EXITSIZEMOVE WM_GETICON WM_GETMINMAXINFO WM_INPUTLANGCHANGE
                 WM_INPUTLANGCHANGEREQUEST WM_MOVE WM_MOVING WM_NCACTIVATE
                 WM_NCCALCSIZE WM_NCCREATE WM_NCDESTROY WM_NULL WM_PARENTNOTIFY
                 WM_QUERYDRAGICON WM_QUERYOPEN WM_QUIT WM_SHOWWINDOW WM_SIZE
                 WM_SIZING WM_STYLECHANGED WM_STYLECHANGING WM_THEMECHANGED WM_USERCHANGED
                 WM_WINDOWPOSCHANGED WM_WINDOWPOSCHANGING
                 ; Structures
                 ALTTABINFO ANIMATIONINFO AUDIODESCRIPTION CHANGEFILTERSTRUCT CLIENTCREATESTRUCT
                 CREATESTRUCTA CREATESTRUCTW GUITHREADINFO
                 MINIMIZEDMETRICS MINMAXINFO NCCALCSIZE_PARAMS NONCLIENTMETRICSA NONCLIENTMETRICSW
                 STYLESTRUCT TITLEBARINFO TITLEBARINFOEX UPDATELAYEREDWINDOWINFO
                 WINDOWINFO WINDOWPLACEMENT WINDOWPOS
                 ; Constants
                 CW_USEDEFAULT
                 ; Extended Window Styles
                 WS_EX_ACCEPTFILES
                 WS_EX_APPWINDOW
                 WS_EX_CLIENTEDGE
                 WS_EX_COMPOSITED
                 WS_EX_CONTEXTHELP
                 WS_EX_CONTROLPARENT
                 WS_EX_DLGMODALFRAME
                 WS_EX_LAYERED
                 WS_EX_LAYOUTRTL
                 WS_EX_LEFT
                 WS_EX_LEFTSCROLLBAR
                 WS_EX_LTRREADING
                 WS_EX_MDICHILD
                 WS_EX_NOACTIVATE
                 WS_EX_NOINHERITLAYOUT
                 WS_EX_NOPARENTNOTIFY
                 WS_EX_NOREDIRECTIONBITMAP
                 #_WS_EX_OVERLAPPED_WINDOW
                 WS_EX_PALETTEWINDOW
                 WS_EX_RIGHT
                 WS_EX_RIGHTSCROLLBAR
                 WS_EX_RTLREADING
                 WS_EX_STATICEDGE
                 WS_EX_TOOLWINDOW
                 WS_EX_TOPMOST
                 WS_EX_TRANSPARENT
                 WS_EX_WINDOWEDGE
                 ; Window Styles
                 WS_BORDER
                 WS_CAPTION
                 WS_CHILD
                 WS_CHILDWINDOW
                 WS_CLIPCHILDREN
                 WS_CLIPSIBLINGS
                 WS_DISABLED
                 WS_DLGFRAME
                 WS_GROUP
                 WS_HSCROLL
                 WS_ICONIC
                 WS_MAXIMIZE
                 WS_MAXIMIZEBOX
                 WS_MINIMIZE
                 WS_MINIMIZEBOX
                 WS_OVERLAPPED
                 WS_OVERLAPPEDWINDOW
                 WS_POPUP
                 WS_POPUPWINDOW
                 WS_SIZEBOX
                 WS_SYSMENU
                 WS_TABSTOP
                 WS_THICKFRAME
                 WS_TILED
                 WS_VSCROLL
                 ;; Window Class
                 ; Functions
                 GetClassInfoA GetClassInfoW
                 GetClassInfoExA GetClassInfoExW
                 GetClassLongA GetClassLongW
                 GetClassLongPtrA GetClassLongPtrW
                 GetClassNameA GetClassNameW
                 GetClassWord
                 GetWindowLongA GetWindowLongW
                 GetWindowLongPtrA GetWindowLongPtrW
                 RegisterClassA RegisterClassW
                 RegisterClassExA RegisterClassExW
                 SetClassLongA SetClassLongW
                 SetClassLongPtrA SetClassLongPtrW
                 SetClassWord
                 SetWindowLongA SetWindowLongW
                 SetWindowLongPtrA SetWindowLongPtrW
                 UnregisterClassA UnregisterClassW
                 ; Structures
                 WNDCLASSA WNDCLASSW
                 WNDCLASSEXA WNDCLASSEXW
                 ; Styles
                 CS_BYTEALIGNCLIENT
                 CS_BYTEALIGNWINDOW
                 CS_CLASSDC
                 CS_DBLCLKS
                 CS_DROPSHADOW
                 CS_GLOBALCLASS
                 CS_HREDRAW
                 CS_NOCLOSE
                 CS_OWNDC
                 CS_PARENTDC
                 CS_SAVEBITS
                 CS_VREDRAW
                 ;; Window Procedures
                 ; Functions
                 CallWindowProcA CallWindowProcW
                 DefWindowProcA DefWindowProcW
                 WNDPROC
                 ;; Messages and Message Queues
                 ; Functions
                 BroadcastSystemMessageA BroadcastSystemMessageW
                 BroadcastSystemMessageExA BroadcastSystemMessageExW
                 DispatchMessageA DispatchMessageW
                 GetInputState
                 GetMessageA GetMessageW
                 GetMessageExtraInfo
                 GetMessagePos
                 GetMessageTime
                 GetQueueStatus
                 InSendMessage
                 InSendMessageEx
                 PeekMessageA PeekMessageW
                 PostMessageA PostMessageW
                 PostQuitMessage
                 PostThreadMessageA PostThreadMessageW
                 RegisterWindowMessageA RegisterWindowMessageW
                 SendMessageA SendMessageW
                 SendMessageCallbackA SendMessageCallbackW
                 SendMessageTimeoutA SendMessageTimeoutW
                 SendNotifyMessageA SendNotifyMessageW
                 SetMessageExtraInfo
                 TranslateMessage
                 WaitMessage
                 ; Structures
                 BSMINFO PBSMINFO
                 MSG
                 ; Constants
                 #_OCM_BASE
                 WM_APP
                 WM_USER
                 ;; Timers
                 ; Functions
                 KillTimer
                 SetCoalescableTimer
                 SetTimer
                 TIMERPROC
                 ; Notifications
                 WM_TIMER
                 ;; Window Properties
                 ; Functions
                 EnumPropsA EnumPropsW
                 EnumPropsExA EnumPropsExW
                 GetPropA GetPropW
                 PROPENUMPROCA PROPENUMPROCW
                 PROPENUMPROCEXA PROPENUMPROCEXW
                 RemovePropA RemovePropW
                 SetPropA SetPropW
                 ;; Configuration
                 GetSystemMetrics
                 SystemParametersInfoA SystemParametersInfoW
                 ; Contact Visualization
                 CONTACTVISUALIZATION_OFF
                 CONTACTVISUALIZATION_ON
                 CONTACTVISUALIZATION_PRESENTATIONMODE
                 ; Gesture Visualization
                 GESTUREVISUALIZATION_OFF
                 GESTUREVISUALIZATION_ON
                 GESTUREVISUALIZATION_TAP
                 GESTUREVISUALIZATION_DOUBLETAP
                 GESTUREVISUALIZATION_PRESSANDTAP
                 GESTUREVISUALIZATION_PRESSANDHOLD
                 GESTUREVISUALIZATION_RIGHTTAP
                 ; Pen Visualization
                 PENVISUALIZATION_OFF
                 PENVISUALIZATION_ON
                 PENVISUALIZATION_TAP
                 PENVISUALIZATION_DOUBLETAP
                 PENVISUALIZATION_CURSOR
                 ;; Hooks
                 ; Functions
                 CallMsgFilterA CallMsgFilterW
                 CallMsgFilterW CallNextHookEx
                 HOOKPROC
                 SetWindowsHookExA SetWindowsHookExW
                 UnhookWindowsHookEx
                 ; Notifications
                 WM_CANCELJOURNAL
                 WM_QUEUESYNC
                 ; Structures
                 CBT_CREATEWNDA CBT_CREATEWNDW
                 CBTACTIVATESTRUCT
                 CWPRETSTRUCT
                 CWPSTRUCT
                 DEBUGHOOKINFO
                 EVENTMSG
                 KBDLLHOOKSTRUCT
                 MOUSEHOOKSTRUCT
                 MOUSEHOOKSTRUCTEX
                 MSLLHOOKSTRUCT
                 ;; Multiple Documents Interface
                 ; Functions
                 CreateMDIWindowA CreateMDIWindowW
                 DefFrameProcA DefFrameProcW
                 DefMDIChildProcA DefMDIChildProcW
                 TranslateMDISysAccel
                 ; Messages
                 WM_MDIACTIVATE
                 WM_MDICASCADE
                 WM_MDICREATE
                 WM_MDIDESTROY
                 WM_MDIGETACTIVE
                 WM_MDIICONARRANGE
                 WM_MDIMAXIMIZE
                 WM_MDINEXT
                 WM_MDIREFRESHMENU
                 WM_MDIRESTORE
                 WM_MDISETMENU
                 WM_MDITILE
                 ; Structures
                 MDICREATESTRUCTA MDICREATESTRUCTW
                 ]}
       {:lib "ole32.dll"
        :include ["objbase.h"]
        :import [; Enumerations
                 COINIT
                 ; Functions
                 CoInitialize
                 CoInitializeEx
                 CoUninitialize
                 ; Types
                 GUID IID LPIID
                 IUnknown
                 ]}
       {:lib "gdi32.dll"
        :include ["wingdi.h"]
        :import [;; Bitmaps
                 BitBlt CreateBitmap CreateBitmapIndirect CreateCompatibleBitmap CreateDIBitmap
                 CreateDIBSection ExtFloodFill GetBitmapDimensionEx GetDIBColorTable GetDIBits
                 GetPixel GetStretchBltMode MaskBlt PlgBlt SetBitmapDimensionEx SetDIBColorTable
                 SetDIBits SetDIBitsToDevice SetPixel SetPixelV SetStretchBltMode StretchBlt
                 StretchDIBits
                 BITMAP BITMAPCOREHEADER BITMAPCOREINFO BITMAPFILEHEADER BITMAPINFO
                 BITMAPINFOHEADER BITMAPV4HEADER BITMAPV5HEADER BLENDFUNCTION COLORADJUSTMENT
                 DIBSECTION GRADIENT_RECT GRADIENT_TRIANGLE RGBQUAD RGBTRIPLE SIZE TRIVERTEX
                 MAKEROP4
                 ;; Brushes
                 CreateBrushIndirect CreateDIBPatternBrushPt CreateHatchBrush CreatePatternBrush
                 CreateSolidBrush GetBrushOrgEx PatBlt SetBrushOrgEx SetDCBrushColor
                 LOGBRUSH LOGBRUSH32
                 ;; Clipping
                 ExcludeClipRect ExtSelectClipRgn GetClipBox GetClipRgn GetMetaRgn GetRandomRgn
                 IntersectClipRect OffsetClipRgn PtVisible RectVisible SelectClipPath SelectClipRgn
                 SetMetaRgn
                 ;; Colors
                 AnimatePalette CreateHalftonePalette CreatePalette GetColorAdjustment
                 GetNearestColor GetNearestPaletteIndex GetPaletteEntries GetSystemPaletteEntries
                 GetSystemPaletteUse RealizePalette ResizePalette SelectPalette SetColorAdjustment
                 SetPaletteEntries SetSystemPaletteUse UnrealizeObject UpdateColors
                 COLORREF LOGPALETTE PALETTEENTRY
                 DIBINDEX GetBValue GetGValue GetRValue PALETTEINDEX PALETTERGB RGB
                 WM_PALETTECHANGED WM_PALETTEISCHANGING WM_QUERYNEWPALETTE WM_SYSCOLORCHANGE
                 ;; Coordinate Space and Transformation
                 CombineTransform DPtoLP GetCurrentPositionEx GetGraphicsMode GetMapMode
                 GetViewportExtEx GetViewportOrgEx GetWindowExtEx GetWindowOrgEx GetWorldTransform
                 LPtoDP ModifyWorldTransform OffsetViewportOrgEx OffsetWindowOrgEx
                 ScaleViewportExtEx ScaleWindowExtEx SetGraphicsMode SetMapMode SetViewportExtEx
                 SetViewportOrgEx SetWindowExtEx SetWindowOrgEx SetWorldTransform
                 XFORM
                 ;; Device Contexts
                 CancelDC CreateCompatibleDC CreateDCA CreateDCW CreateICA CreateICW DeleteDC DeleteObject
                 DrawEscape EnumObjects GOBJENUMPROC GetCurrentObject GetDCBrushColor GetDCOrgEx
                 GetDCPenColor GetDeviceCaps GetLayout GetObjectA GetObjectW GetObjectType GetStockObject
                 ResetDCA ResetDCW RestoreDC SaveDC SelectObject SetDCBrushColor SetDCPenColor SetLayout
                 DISPLAY_DEVICE VIDEOPARAMETERS
                 WM_DEVMODECHANGE
                 WHITE_BRUSH
                 IDC_ARROW
                 ;; Filled Shapes
                 Chord Ellipse Pie Polygon PolyPolygon
                 Rectangle RoundRect
                 ;; Fonts and Text
                 AddFontResourceA AddFontResourceW AddFontResourceExA AddFontResourceExW
                 AddFontMemResourceEx
                 CreateFontA CreateFontW CreateFontIndirectA CreateFontIndirectW
                 CreateFontIndirectExA CreateFontIndirectExW FONTENUMPROC
                 EnumFontFamiliesExA EnumFontFamiliesExW ExtTextOutA ExtTextOutW GetAspectRatioFilterEx
                 GetCharABCWidthsA GetCharABCWidthsW GetCharABCWidthsFloatA GetCharABCWidthsFloatW
                 GetCharABCWidthsI GetCharacterPlacementA GetCharacterPlacementW GetCharWidth32A GetCharWidth32W
                 GetCharWidthFloatA GetCharWidthFloatW GetCharWidthI GetFontData GetFontLanguageInfo
                 GetFontUnicodeRanges GetGlyphIndicesA GetGlyphIndicesW GetGlyphOutlineA GetGlyphOutlineW
                 GetKerningPairsA GetKerningPairsW GetOutlineTextMetricsA GetOutlineTextMetricsW
                 GetRasterizerCaps GetTextAlign
                 GetTextCharacterExtra GetTextColor GetTextExtentExPointA GetTextExtentExPointW
                 GetTextExtentExPointI GetTextExtentPoint32A GetTextExtentPoint32W GetTextExtentPointI
                 GetTextFaceA GetTextFaceW GetTextMetricsA GetTextMetricsW
                 PolyTextOutA PolyTextOutW RemoveFontMemResourceEx RemoveFontResourceA RemoveFontResourceW
                 RemoveFontResourceExA RemoveFontResourceExW
                 SetMapperFlags SetTextAlign SetTextCharacterExtra SetTextColor
                 SetTextJustification TextOutA TextOutW
                 ABC ABCFLOAT AXESLIST DESIGNVECTOR DRAWTEXTPARAMS ENUMLOGFONT ENUMLOGFONTEX
                 ENUMLOGFONTEXDV ENUMTEXTMETRIC EXTLOGFONT FIXED GCP_RESULTS GLYPHMETRICS
                 GLYPHSET KERNINGPAIR LOGFONTA LOGFONTW MAT2 NEWTEXTMETRIC NEWTEXTMETRICEX
                 OUTLINETEXTMETRIC PANOSE POINTFX POLYTEXT RASTERIZER_STATUS SIZE TEXTMETRIC
                 TTPOLYCURVE TTPOLYGONHEADER WCRANGE
                 #_DeleteFont #_SelectFont
                 WM_FONTCHANGE
                 ;; TODO errors
                 ;; Lines and Curves
                 AngleArc Arc ArcTo GetArcDirection LineDDA LINEDDAPROC LineTo MoveToEx
                 PolyBezier PolyBezierTo PolyDraw Polyline PolylineTo PolyPolyline SetArcDirection
                 ;; Metafiles
                 CloseEnhMetaFile CopyEnhMetaFileA CopyEnhMetaFileW CreateEnhMetaFileA CreateEnhMetaFileW
                 DeleteEnhMetaFile
                 ENHMFENUMPROC EnumEnhMetaFile GdiComment GetEnhMetaFileA GetEnhMetaFileW GetEnhMetaFileBits
                 GetEnhMetaFileDescriptionA GetEnhMetaFileDescriptionW GetEnhMetaFileHeader
                 GetEnhMetaFilePaletteEntries
                 GetMetaFileA GetMetaFileW GetWinMetaFileBits PlayEnhMetaFile PlayEnhMetaFileRecord
                 SetEnhMetaFileBits SetWinMetaFileBits
                 EMRTEXT EMRTRANSPARENTBLT EMRWIDENPATH ENHMETAHEADER ENHMETARECORD HANDLETABLE
                 POINTL RECTL EMR ;; TODO emr structures
                 ;; Painting and Drawing
                 GdiFlush GdiGetBatchLimit GdiSetBatchLimit GetBkColor GetBkMode GetBoundsRect
                 GetROP2 SetBkColor SetBkMode SetBoundsRect SetROP2
                 PAINTSTRUCT
                 WM_DISPLAYCHANGE WM_ERASEBKGND WM_NCPAINT WM_PAINT WM_PRINT WM_PRINTCLIENT
                 WM_SETREDRAW WM_SYNCPAINT
                 ;; Paths
                 AbortPath BeginPath CloseFigure EndPath FillPath FlattenPath GetMiterLimit
                 GetPath PathToRegion SetMiterLimit StrokeAndFillPath StrokePath WidenPath
                 ;; Pens
                 CreatePen CreatePenIndirect ExtCreatePen SetDCPenColor
                 EXTLOGPEN LOGPEN
                 ;; Regions
                 CombineRgn CreateEllipticRgn CreateEllipticRgnIndirect CreatePolygonRgn
                 CreatePolyPolygonRgn CreateRectRgn CreateRectRgnIndirect CreateRoundRectRgn
                 EqualRgn ExtCreateRegion FillRgn FrameRgn GetPolyFillMode GetRegionData
                 GetRgnBox InvertRgn OffsetRgn PaintRgn PtInRegion RectInRegion
                 SetPolyFillMode SetRectRgn
                 RGNDATA RGNDATAHEADER]}
       {:lib "msimg32.dll"
        :include []
        :import [;; Bitmaps
                 AlphaBlend GradientFill TransparentBlt]}
       {:lib "user32.dll"
        :include []
        :import [;; Bitmaps
                 LoadBitmapA LoadBitmapW
                 ;; Brushes
                 GetSysColorBrush
                 ;; Coordinate Space and Transformation
                 ClientToScreen GetDisplayAutoRotationPreferences MapWindowPoints
                 ScreenToClient SetDisplayAutoRotationPreferences
                 ;; Device Contexts
                 ChangeDisplaySettingsA ChangeDisplaySettingsW ChangeDisplaySettingsExA
                 ChangeDisplaySettingsExW #_DeviceCapabilitiesA #_DeviceCapabilitiesW
                 EnumDisplayDevicesA EnumDisplayDevicesW EnumDisplaySettingsA
                 EnumDisplaySettingsW EnumDisplaySettingsExA EnumDisplaySettingsExW
                 GetDC GetDCEx ReleaseDC WindowFromDC
                 ;; Filled Shapes
                 FillRect FrameRect InvertRect
                 ;; Fonts and Text
                 DrawTextA DrawTextW DrawTextExA DrawTextExW GetTabbedTextExtentA
                 GetTabbedTextExtentW TabbedTextOutA TabbedTextOutW
                 ;; Multiple Display Monitors
                 EnumDisplayMonitors GetMonitorInfoA GetMonitorInfoW MONITORENUMPROC MonitorFromPoint
                 MonitorFromRect MonitorFromWindow
                 MONITORINFO MONITORINFOEX
                 ;; Painting and Drawing
                 BeginPaint DrawAnimatedRects DrawCaption DrawEdge DrawFocusRect DrawFrameControl
                 DrawStateA DrawStateW DRAWSTATEPROC EndPaint ExcludeUpdateRgn GetUpdateRect
                 GetUpdateRgn GetWindowDC GetWindowRgn GetWindowRgnBox GrayStringA GrayStringW
                 InvalidateRect InvalidateRgn LockWindowUpdate GRAYSTRINGPROC PaintDesktop
                 RedrawWindow SetWindowRgn UpdateWindow ValidateRect ValidateRgn
                 ;; Rectangles
                 CopyRect EqualRect InflateRect IntersectRect IsRectEmpty OffsetRect
                 PtInRect SetRect SetRectEmpty SubtractRect UnionRect
                 POINT POINTS RECT
                 #_MAKEPOINTS #_POINTSTOPOINT #_POINTTOPOINTS]}
       {:lib "XInput1_4.dll"
        :include ["xinput.h"]
        :import [XInputEnable XInputGetAudioDeviceIds XInputGetBatteryInformation
                 XInputGetCapabilities #_XInputGetDSoundAudioDeviceGuids
                 XInputGetKeystroke XInputGetState XInputSetState
                 XINPUT_BATTERY_INFORMATION XINPUT_CAPABILITIES XINPUT_GAMEPAD
                 XINPUT_KEYSTROKE XINPUT_STATE XINPUT_VIBRATION]}
       {:lib "dxgi.dll"
        :include ["dxgi1_6.h"]
        :import [IDXGIAdapter IDXGIAdapter1 IDXGIAdapter2 IDXGIAdapter3 IDXGIAdapter4
                 IDXGIDecodeSwapChain IDXGIDevice IDXGIDevice2 IDXGIDevice3 IDXGIDevice4
                 IDXGIDeviceSubObject IDXGIDisplayControl
                 IDXGIFactory IDXGIFactory1 IDXGIFactory2 IDXGIFactory3 IDXGIFactory4
                 IDXGIFactory5 IDXGIFactory6 IDXGIFactory7 IDXGIFactoryMedia IDXGIKeyedMutex
                 IDXGIObject IDXGIOutput IDXGIOutput1 IDXGIOutput2 IDXGIOutput3 IDXGIOutput4
                 IDXGIOutput5 IDXGIOutput6 IDXGIOutputDuplication
                 IDXGIResource IDXGIResource1 IDXGISurface IDXGISurface1 IDXGISurface2
                 IDXGISwapChain IDXGISwapChain1 IDXGISwapChain2 IDXGISwapChain3 IDXGISwapChain4
                 IDXGISwapChainMedia
                 CreateDXGIFactory CreateDXGIFactory1 CreateDXGIFactory2 DXGIGetDebugInterface1
                 DXGIDeclareAdapterRemovalSupport
                 D3DCOLORVALUE
                 DXGI_ADAPTER_DESC DXGI_ADAPTER_DESC1 DXGI_ADAPTER_DESC2 DXGI_ADAPTER_DESC3
                 DXGI_DECODE_SWAP_CHAIN_DESC DXGI_DISPLAY_COLOR_SPACE DXGI_FRAME_STATISTICS
                 DXGI_FRAME_STATISTICS_MEDIA DXGI_GAMMA_CONTROL DXGI_GAMMA_CONTROL_CAPABILITIES
                 DXGI_HDR_METADATA_HDR10 DXGI_JPEG_AC_HUFFMAN_TABLE DXGI_JPEG_DC_HUFFMAN_TABLE
                 DXGI_JPEG_QUANTIZATION_TABLE DXGI_MATRIX_3X2_F DXGI_MAPPED_RECT DXGI_MODE_DESC
                 DXGI_MODE_DESC1 DXGI_OUTPUT_DESC DXGI_OUTPUT_DESC1 DXGI_OUTDUPL_DESC
                 DXGI_OUTDUPL_FRAME_INFO DXGI_OUTDUPL_MOVE_RECT DXGI_OUTDUPL_POINTER_POSITION
                 DXGI_OUTDUPL_POINTER_SHAPE_INFO DXGI_PRESENT_PARAMETERS
                 DXGI_QUERY_VIDEO_MEMORY_INFO DXGI_RATIONAL DXGI_RGB DXGI_RGBA DXGI_SAMPLE_DESC
                 DXGI_SHARED_RESOURCE DXGI_SURFACE_DESC DXGI_SWAP_CHAIN_DESC DXGI_SWAP_CHAIN_DESC1
                 DXGI_SWAP_CHAIN_FULLSCREEN_DESC _LUID
                 DXGI_ADAPTER_FLAG DXGI_ADAPTER_FLAG3 DXGI_ALPHA_MODE DXGI_COLOR_SPACE_TYPE
                 DXGI_COMPUTE_PREEMPTION_GRANULARITY DXGI_FEATURE DXGI_FORMAT
                 DXGI_FRAME_PRESENTATION_MODE DXGI_GPU_PREFERENCE
                 DXGI_GRAPHICS_PREEMPTION_GRANULARITY DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAGS
                 DXGI_HDR_METADATA_TYPE DXGI_MEMORY_SEGMENT_GROUP DXGI_MODE_ROTATION
                 DXGI_MODE_SCALING DXGI_MODE_SCANLINE_ORDER DXGI_MULTIPLANE_OVERLAY_YCbCr_FLAGS
                 DXGI_OFFER_RESOURCE_FLAGS DXGI_OFFER_RESOURCE_PRIORITY
                 DXGI_OUTDUPL_POINTER_SHAPE_TYPE DXGI_OVERLAY_COLOR_SPACE_SUPPORT_FLAG
                 DXGI_OVERLAY_SUPPORT_FLAG DXGI_RECLAIM_RESOURCE_RESULTS DXGI_RESIDENCY
                 DXGI_SCALING DXGI_SWAP_CHAIN_COLOR_SPACE_SUPPORT_FLAG DXGI_SWAP_CHAIN_FLAG
                 DXGI_SWAP_EFFECT DXGI_ENUM_MODES_INTERLACED DXGI_ENUM_MODES_SCALING
                 DXGI_ENUM_MODES_STEREO DXGI_ENUM_MODES_DISABLED_STEREO
                 DXGI_ERROR_ACCESS_DENIED DXGI_ERROR_ACCESS_LOST DXGI_ERROR_ALREADY_EXISTS
                 DXGI_ERROR_CANNOT_PROTECT_CONTENT DXGI_ERROR_DEVICE_HUNG DXGI_ERROR_DEVICE_REMOVED
                 DXGI_ERROR_DEVICE_RESET DXGI_ERROR_DRIVER_INTERNAL_ERROR
                 DXGI_ERROR_FRAME_STATISTICS_DISJOINT DXGI_ERROR_GRAPHICS_VIDPN_SOURCE_IN_USE
                 DXGI_ERROR_INVALID_CALL DXGI_ERROR_MORE_DATA DXGI_ERROR_NAME_ALREADY_EXISTS
                 DXGI_ERROR_NONEXCLUSIVE DXGI_ERROR_NOT_CURRENTLY_AVAILABLE DXGI_ERROR_NOT_FOUND
                 DXGI_ERROR_REMOTE_CLIENT_DISCONNECTED DXGI_ERROR_REMOTE_OUTOFMEMORY
                 DXGI_ERROR_RESTRICT_TO_OUTPUT_STALE DXGI_ERROR_SDK_COMPONENT_MISSING
                 DXGI_ERROR_SESSION_DISCONNECTED DXGI_ERROR_UNSUPPORTED DXGI_ERROR_WAIT_TIMEOUT
                 DXGI_ERROR_WAS_STILL_DRAWING #_S_OK
                 DXGI_PRESENT_DO_NOT_SEQUENCE DXGI_PRESENT_TEST DXGI_PRESENT_RESTART
                 DXGI_PRESENT_DO_NOT_WAIT DXGI_PRESENT_RESTRICT_TO_OUTPUT
                 DXGI_PRESENT_STEREO_PREFER_RIGHT DXGI_PRESENT_STEREO_TEMPORARY_MONO
                 DXGI_PRESENT_USE_DURATION DXGI_PRESENT_ALLOW_TEARING
                 DXGI_SHARED_RESOURCE_READ DXGI_SHARED_RESOURCE_WRITE
                 DXGI_STATUS_OCCLUDED DXGI_STATUS_MODE_CHANGED DXGI_STATUS_MODE_CHANGE_IN_PROGRESS
                 DXGI_USAGE_BACK_BUFFER DXGI_USAGE_DISCARD_ON_PRESENT DXGI_USAGE_READ_ONLY
                 DXGI_USAGE_RENDER_TARGET_OUTPUT DXGI_USAGE_SHADER_INPUT DXGI_USAGE_SHARED
                 DXGI_USAGE_UNORDERED_ACCESS
                 DXGI_CREATE_FACTORY_DEBUG
                 DXGI_MWA_NO_WINDOW_CHANGES
                 DXGI_MWA_NO_ALT_ENTER
                 DXGI_MWA_NO_PRINT_SCREEN]}
       {:lib "DXGIDebug.dll"
        :include ["dxgidebug.h"]
        :import [IDXGIDebug IDXGIDebug1 IDXGIInfoQueue DXGIGetDebugInterface
                 DXGI_INFO_QUEUE_FILTER DXGI_INFO_QUEUE_FILTER_DESC DXGI_INFO_QUEUE_MESSAGE
                 DXGI_DEBUG_RLO_FLAGS DXGI_INFO_QUEUE_MESSAGE_CATEGORY DXGI_INFO_QUEUE_MESSAGE_SEVERITY
                 DXGI_DEBUG_ID ;;DXGI_DEBUG_ALL DXGI_DEBUG_DX DXGI_DEBUG_DXGI DXGI_DEBUG_APP
                 #_DXGI_DEBUG_D3D11]}
       {:lib "D3DCompiler_47.dll"
        :include ["d3dcommon.h" "d3dcompiler.h"]
        :import [;; Functions
                 D3DCompile D3DCompile2 D3DCompileFromFile D3DCompressShaders
                 D3DCreateBlob D3DCreateFunctionLinkingGraph D3DCreateLinker
                 D3DDecompressShaders D3DDisassemble D3DDisassemble10Effect
                 D3DDisassembleRegion D3DGetBlobPart D3DGetDebugInfo D3DGetInputAndOutputSignatureBlob
                 D3DGetInputSignatureBlob D3DGetOutputSignatureBlob D3DGetTraceInstructionOffsets
                 D3DLoadModule D3DPreprocess D3DReadFileToBlob D3DReflect D3DReflectLibrary
                 D3DSetBlobPart D3DStripShader D3DWriteBlobToFile
                 ;; Structures
                 D3D_SHADER_DATA
                 ;; Enumerations
                 D3D_BLOB_PART D3DCOMPILER_STRIP_FLAGS
                 ;; Constants
                 D3DCOMPILE_DEBUG D3DCOMPILE_SKIP_VALIDATION D3DCOMPILE_SKIP_OPTIMIZATION
                 D3DCOMPILE_PACK_MATRIX_ROW_MAJOR D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR
                 D3DCOMPILE_PARTIAL_PRECISION D3DCOMPILE_FORCE_VS_SOFTWARE_NO_OPT
                 D3DCOMPILE_FORCE_PS_SOFTWARE_NO_OPT D3DCOMPILE_NO_PRESHADER
                 D3DCOMPILE_AVOID_FLOW_CONTROL D3DCOMPILE_PREFER_FLOW_CONTROL
                 D3DCOMPILE_ENABLE_STRICTNESS D3DCOMPILE_ENABLE_BACKWARDS_COMPATIBILITY
                 D3DCOMPILE_IEEE_STRICTNESS D3DCOMPILE_OPTIMIZATION_LEVEL0
                 D3DCOMPILE_OPTIMIZATION_LEVEL1 D3DCOMPILE_OPTIMIZATION_LEVEL2
                 D3DCOMPILE_OPTIMIZATION_LEVEL3 D3DCOMPILE_WARNINGS_ARE_ERRORS
                 D3DCOMPILE_RESOURCES_MAY_ALIAS
                 ;; Interfaces
                 ID3DBlob ID3DInclude]}
       {:lib "D3D12.dll"
        :include ["d3d12.h" "d3d12shader.h"]
        :import [D3D_FEATURE_LEVEL D3D_PRIMITIVE_TOPOLOGY D3D_PRIMITIVE D3D_TESSELLATOR_OUTPUT_PRIMITIVE
                 D3D_TESSELLATOR_PARTITIONING D3D_TESSELLATOR_DOMAIN D3D_CBUFFER_TYPE
                 D3D_SHADER_VARIABLE_TYPE D3D_SHADER_VARIABLE_CLASS D3D_INTERPOLATION_MODE
                 D3D_SHADER_INPUT_TYPE D3D_RESOURCE_RETURN_TYPE D3D_SRV_DIMENSION
                 D3D_REGISTER_COMPONENT_TYPE D3D_MIN_PRECISION
                 ID3D12CommandAllocator ID3D12CommandList ID3D12CommandQueue
                 ID3D12CommandSignature ID3D12DescriptorHeap
                 ID3D12Device ID3D12Device1 ID3D12Device2 ID3D12Device3 ID3D12Device4 ID3D12Device5
                 ID3D12DeviceChild #_ID3D12DeviceRemovedExtendedData #_ID3D12DeviceRemovedExtendedDataSettings
                 ID3D12Fence ID3D12Fence1 ID3D12GraphicsCommandList ID3D12GraphicsCommandList1
                 ID3D12GraphicsCommandList2 ID3D12GraphicsCommandList3 ID3D12GraphicsCommandList4
                 ID3D12Heap ID3D12MetaCommand ID3D12Object ID3D12Pageable ID3D12PipelineLibrary
                 ID3D12PipelineLibrary1 ID3D12PipelineState ID3D12QueryHeap ID3D12Resource
                 ID3D12RootSignature ID3D12RootSignatureDeserializer ID3D12StateObject
                 ID3D12StateObjectProperties ID3D12Tools ID3D12VersionedRootSignatureDeserializer
                 D3D12CreateDevice D3D12CreateRootSignatureDeserializer
                 D3D12CreateVersionedRootSignatureDeserializer D3D12EnableExperimentalFeatures
                 D3D12GetDebugInterface D3D12SerializeRootSignature D3D12SerializeVersionedRootSignature
                 D3D12_AUTO_BREADCRUMB_NODE D3D12_BLEND_DESC D3D12_BOX D3D12_BUFFER_RTV
                 D3D12_BUFFER_SRV D3D12_BUFFER_UAV
                 D3D12_BUILD_RAYTRACING_ACCELERATION_STRUCTURE_DESC
                 D3D12_BUILD_RAYTRACING_ACCELERATION_STRUCTURE_INPUTS
                 D3D12_BUILD_RAYTRACING_ACCELERATION_STRUCTURE_TOOLS_VISUALIZATION_HEADER
                 D3D12_CACHED_PIPELINE_STATE D3D12_CLEAR_VALUE
                 D3D12_COMMAND_QUEUE_DESC D3D12_COMMAND_SIGNATURE_DESC D3D12_COMPUTE_PIPELINE_STATE_DESC
                 D3D12_CONSTANT_BUFFER_VIEW_DESC D3D12_CPU_DESCRIPTOR_HANDLE D3D12_DEPTH_STENCIL_DESC
                 D3D12_DEPTH_STENCIL_DESC1 D3D12_DEPTH_STENCIL_VALUE D3D12_DEPTH_STENCIL_VIEW_DESC
                 D3D12_DEPTH_STENCILOP_DESC D3D12_DESCRIPTOR_HEAP_DESC D3D12_DESCRIPTOR_RANGE
                 D3D12_DESCRIPTOR_RANGE1 #_D3D12_DEVICE_REMOVED_EXTENDED_DATA
                 #_D3D12_DEVICE_REMOVED_EXTENDED_DATA1 D3D12_DISCARD_REGION D3D12_DISPATCH_ARGUMENTS
                 D3D12_DRAW_ARGUMENTS D3D12_DRAW_INDEXED_ARGUMENTS #_D3D12_DRED_ALLOCATION_NODE
                 #_D3D12_DRED_AUTO_BREADCRUMBS_OUTPUT #_D3D12_DRED_PAGE_FAULT_OUTPUT
                 D3D12_FEATURE_DATA_ARCHITECTURE D3D12_FEATURE_DATA_ARCHITECTURE1
                 D3D12_FEATURE_DATA_COMMAND_QUEUE_PRIORITY D3D12_FEATURE_DATA_CROSS_NODE
                 D3D12_FEATURE_DATA_D3D12_OPTIONS D3D12_FEATURE_DATA_D3D12_OPTIONS1
                 D3D12_FEATURE_DATA_D3D12_OPTIONS2 D3D12_FEATURE_DATA_D3D12_OPTIONS3
                 D3D12_FEATURE_DATA_D3D12_OPTIONS4 D3D12_FEATURE_DATA_EXISTING_HEAPS
                 D3D12_FEATURE_DATA_FEATURE_LEVELS D3D12_FEATURE_DATA_FORMAT_INFO
                 D3D12_FEATURE_DATA_FORMAT_SUPPORT D3D12_FEATURE_DATA_GPU_VIRTUAL_ADDRESS_SUPPORT
                 D3D12_FEATURE_DATA_MULTISAMPLE_QUALITY_LEVELS
                 D3D12_FEATURE_DATA_PROTECTED_RESOURCE_SESSION_SUPPORT
                 D3D12_FEATURE_DATA_ROOT_SIGNATURE D3D12_FEATURE_DATA_SERIALIZATION
                 D3D12_FEATURE_DATA_SHADER_CACHE D3D12_FEATURE_DATA_SHADER_MODEL
                 D3D12_GPU_DESCRIPTOR_HANDLE D3D12_GRAPHICS_PIPELINE_STATE_DESC D3D12_HEAP_DESC
                 D3D12_HEAP_PROPERTIES D3D12_INDEX_BUFFER_VIEW D3D12_INDIRECT_ARGUMENT_DESC
                 D3D12_INPUT_ELEMENT_DESC D3D12_INPUT_LAYOUT_DESC D3D12_MEMCPY_DEST
                 D3D12_META_COMMAND_DESC D3D12_META_COMMAND_PARAMETER_DESC
                 D3D12_PACKED_MIP_INFO D3D12_PIPELINE_STATE_STREAM_DESC
                 D3D12_PLACED_SUBRESOURCE_FOOTPRINT D3D12_QUERY_DATA_PIPELINE_STATISTICS
                 D3D12_QUERY_DATA_SO_STATISTICS D3D12_QUERY_HEAP_DESC D3D12_RANGE
                 D3D12_RANGE_UINT64 D3D12_RASTERIZER_DESC D3D12_RAYTRACING_AABB
                 D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_COMPACTED_SIZE_DESC
                 D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_CURRENT_SIZE_DESC
                 D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_DESC
                 D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_SERIALIZATION_DESC
                 D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_TOOLS_VISUALIZATION_DESC
                 D3D12_RAYTRACING_ACCELERATION_STRUCTURE_PREBUILD_INFO
                 D3D12_RAYTRACING_ACCELERATION_STRUCTURE_SRV
                 D3D12_RAYTRACING_GEOMETRY_AABBS_DESC D3D12_RAYTRACING_GEOMETRY_DESC
                 D3D12_RAYTRACING_GEOMETRY_TRIANGLES_DESC D3D12_RAYTRACING_INSTANCE_DESC
                 D3D12_RAYTRACING_PIPELINE_CONFIG D3D12_RAYTRACING_SHADER_CONFIG
                 D3D12_RECT D3D12_RENDER_PASS_BEGINNING_ACCESS
                 D3D12_RENDER_PASS_BEGINNING_ACCESS_CLEAR_PARAMETERS
                 D3D12_RENDER_PASS_DEPTH_STENCIL_DESC D3D12_RENDER_PASS_ENDING_ACCESS
                 D3D12_RENDER_PASS_ENDING_ACCESS_RESOLVE_PARAMETERS
                 D3D12_RENDER_PASS_ENDING_ACCESS_RESOLVE_SUBRESOURCE_PARAMETERS
                 D3D12_RENDER_PASS_RENDER_TARGET_DESC D3D12_RENDER_TARGET_BLEND_DESC
                 D3D12_RENDER_TARGET_VIEW_DESC D3D12_RESOURCE_ALIASING_BARRIER
                 D3D12_RESOURCE_ALLOCATION_INFO D3D12_RESOURCE_BARRIER D3D12_RESOURCE_DESC
                 D3D12_RESOURCE_TRANSITION_BARRIER D3D12_RESOURCE_UAV_BARRIER
                 D3D12_ROOT_CONSTANTS D3D12_ROOT_DESCRIPTOR D3D12_ROOT_DESCRIPTOR1
                 D3D12_ROOT_DESCRIPTOR_TABLE D3D12_ROOT_DESCRIPTOR_TABLE1
                 D3D12_ROOT_PARAMETER D3D12_ROOT_PARAMETER1 D3D12_ROOT_SIGNATURE_DESC
                 D3D12_ROOT_SIGNATURE_DESC1 D3D12_RT_FORMAT_ARRAY
                 D3D12_SAMPLE_POSITION D3D12_SAMPLER_DESC D3D12_SHADER_BYTECODE
                 D3D12_SHADER_RESOURCE_VIEW_DESC D3D12_SO_DECLARATION_ENTRY
                 D3D12_STATIC_SAMPLER_DESC D3D12_STREAM_OUTPUT_BUFFER_VIEW
                 D3D12_STREAM_OUTPUT_DESC D3D12_SUBRESOURCE_DATA
                 D3D12_SUBRESOURCE_FOOTPRINT D3D12_SUBRESOURCE_INFO
                 D3D12_SUBRESOURCE_RANGE_UINT64 D3D12_SUBRESOURCE_TILING
                 D3D12_TEX1D_ARRAY_DSV D3D12_TEX1D_ARRAY_RTV D3D12_TEX1D_ARRAY_SRV D3D12_TEX1D_ARRAY_UAV
                 D3D12_TEX1D_DSV D3D12_TEX1D_RTV D3D12_TEX1D_SRV D3D12_TEX1D_UAV
                 D3D12_TEX2D_ARRAY_DSV D3D12_TEX2D_ARRAY_RTV D3D12_TEX2D_ARRAY_SRV D3D12_TEX2D_ARRAY_UAV
                 D3D12_TEX2D_DSV D3D12_TEX2D_RTV D3D12_TEX2D_SRV D3D12_TEX2D_UAV
                 D3D12_TEX2DMS_ARRAY_DSV D3D12_TEX2DMS_ARRAY_RTV D3D12_TEX2DMS_ARRAY_SRV
                 D3D12_TEX2DMS_DSV D3D12_TEX2DMS_RTV D3D12_TEX2DMS_SRV
                 D3D12_TEX3D_RTV D3D12_TEX3D_SRV D3D12_TEX3D_UAV
                 D3D12_TEXCUBE_ARRAY_SRV D3D12_TEXCUBE_SRV D3D12_TEXTURE_COPY_LOCATION
                 D3D12_TILE_REGION_SIZE D3D12_TILE_SHAPE D3D12_TILED_RESOURCE_COORDINATE
                 D3D12_UNORDERED_ACCESS_VIEW_DESC D3D12_VERTEX_BUFFER_VIEW
                 D3D12_VERSIONED_DEVICE_REMOVED_EXTENDED_DATA D3D12_VERSIONED_ROOT_SIGNATURE_DESC
                 D3D12_VIEW_INSTANCE_LOCATION D3D12_VIEW_INSTANCING_DESC D3D12_VIEWPORT
                 D3D12_WRITEBUFFERIMMEDIATE_PARAMETER
                 D3D_ROOT_SIGNATURE_VERSION D3D_SHADER_MODEL
                 D3D12_AUTO_BREADCRUMB_OP D3D12_BLEND D3D12_BLEND_OP D3D12_BUFFER_SRV_FLAGS
                 D3D12_BUFFER_UAV_FLAGS D3D12_CLEAR_FLAGS D3D12_COLOR_WRITE_ENABLE
                 D3D12_COMMAND_LIST_TYPE
                 D3D12_COMMAND_LIST_SUPPORT_FLAGS D3D12_COMMAND_LIST_FLAGS D3D12_COMMAND_QUEUE_FLAGS
                 D3D12_COMMAND_QUEUE_PRIORITY D3D12_COMPARISON_FUNC D3D12_CONSERVATIVE_RASTERIZATION_MODE
                 D3D12_CONSERVATIVE_RASTERIZATION_TIER D3D12_CPU_PAGE_PROPERTY
                 D3D12_CROSS_NODE_SHARING_TIER D3D12_CULL_MODE D3D12_DEBUG_DEVICE_PARAMETER_TYPE
                 D3D12_DEPTH_WRITE_MASK D3D12_DESCRIPTOR_HEAP_FLAGS D3D12_DESCRIPTOR_HEAP_TYPE
                 D3D12_DESCRIPTOR_RANGE_FLAGS D3D12_DESCRIPTOR_RANGE_TYPE #_D3D12_DRED_ALLOCATION_TYPE
                 #_D3D12_DRED_ENABLEMENT #_D3D12_DRED_FLAGS #_D3D12_DRED_VERSION
                 D3D12_DSV_DIMENSION D3D12_DSV_FLAGS D3D12_FEATURE D3D12_FENCE_FLAGS D3D12_FILL_MODE
                 D3D12_FILTER D3D12_FILTER_REDUCTION_TYPE D3D12_FILTER_TYPE D3D12_FORMAT_SUPPORT1
                 D3D12_FORMAT_SUPPORT2 D3D12_GRAPHICS_STATES D3D12_HEAP_FLAGS
                 D3D12_HEAP_SERIALIZATION_TIER D3D12_HEAP_TYPE D3D12_INDEX_BUFFER_STRIP_CUT_VALUE
                 D3D12_INDIRECT_ARGUMENT_TYPE D3D12_INPUT_CLASSIFICATION D3D12_LOGIC_OP
                 D3D12_MEMORY_POOL D3D12_META_COMMAND_PARAMETER_FLAGS D3D12_META_COMMAND_PARAMETER_STAGE
                 D3D12_META_COMMAND_PARAMETER_TYPE D3D12_MULTIPLE_FENCE_WAIT_FLAGS
                 #_D3D12_MULTISAMPLE_QUALITY_LEVELS_FLAG D3D12_PIPELINE_STATE_FLAGS
                 D3D12_PIPELINE_STATE_SUBOBJECT_TYPE D3D12_PREDICATION_OP D3D12_PRIMITIVE_TOPOLOGY_TYPE
                 D3D12_PROGRAMMABLE_SAMPLE_POSITIONS_TIER D3D12_PROTECTED_RESOURCE_SESSION_SUPPORT_FLAGS
                 D3D12_QUERY_HEAP_TYPE D3D12_QUERY_TYPE D3D12_RAY_FLAGS
                 D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAGS
                 D3D12_RAYTRACING_ACCELERATION_STRUCTURE_COPY_MODE
                 D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_TYPE
                 D3D12_RAYTRACING_ACCELERATION_STRUCTURE_TYPE
                 D3D12_RAYTRACING_GEOMETRY_FLAGS D3D12_RAYTRACING_GEOMETRY_TYPE
                 D3D12_RAYTRACING_INSTANCE_FLAGS D3D12_RAYTRACING_TIER
                 D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE D3D12_RENDER_PASS_ENDING_ACCESS_TYPE
                 D3D12_RENDER_PASS_FLAGS D3D12_RESIDENCY_FLAGS D3D12_RESIDENCY_PRIORITY
                 D3D12_RESOLVE_MODE D3D12_RESOURCE_BARRIER_FLAGS D3D12_RESOURCE_BARRIER_TYPE
                 D3D12_RESOURCE_BINDING_TIER D3D12_RESOURCE_DIMENSION D3D12_RESOURCE_FLAGS
                 D3D12_RESOURCE_HEAP_TIER D3D12_RESOURCE_STATES D3D12_ROOT_DESCRIPTOR_FLAGS
                 D3D12_ROOT_PARAMETER_TYPE D3D12_ROOT_SIGNATURE_FLAGS D3D12_RTV_DIMENSION
                 D3D12_SHADER_CACHE_SUPPORT_FLAGS D3D12_SHADER_COMPONENT_MAPPING
                 D3D12_SHADER_MIN_PRECISION_SUPPORT D3D12_SHADER_VISIBILITY
                 D3D12_SHARED_RESOURCE_COMPATIBILITY_TIER D3D12_SRV_DIMENSION D3D12_STATIC_BORDER_COLOR
                 D3D12_STENCIL_OP D3D12_TEXTURE_ADDRESS_MODE D3D12_TEXTURE_COPY_TYPE
                 D3D12_TEXTURE_LAYOUT D3D12_TILE_COPY_FLAGS D3D12_TILE_MAPPING_FLAGS
                 D3D12_TILE_RANGE_FLAGS D3D12_TILED_RESOURCES_TIER D3D12_UAV_DIMENSION
                 D3D12_VIEW_INSTANCING_FLAGS D3D12_VIEW_INSTANCING_TIER
                 D3D12_WRITEBUFFERIMMEDIATE_MODE
                 D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES
                 ;; TODO constants
                 ;; D3D12 Shaders
                 ID3D12FunctionParameterReflection ID3D12FunctionReflection ID3D12LibraryReflection
                 ID3D12ShaderReflection ID3D12ShaderReflectionConstantBuffer ID3D12ShaderReflectionType
                 ID3D12ShaderReflectionVariable
                 D3D12_FUNCTION_DESC D3D12_LIBRARY_DESC D3D12_PARAMETER_DESC D3D12_SHADER_BUFFER_DESC
                 D3D12_SHADER_DESC D3D12_SHADER_INPUT_BIND_DESC D3D12_SHADER_TYPE_DESC
                 D3D12_SHADER_VARIABLE_DESC D3D12_SIGNATURE_PARAMETER_DESC D3D12_SHADER_VERSION_TYPE
                 ]}
       {:lib "C:\\Windows\\System32\\d3d12SDKLayers.dll"
        :include ["d3d12sdklayers.h"]
        :import [ID3D12Debug ID3D12Debug1 ID3D12Debug2 ID3D12DebugCommandList ID3D12DebugCommandList1
                 ID3D12DebugCommandQueue ID3D12DebugDevice ID3D12DebugDevice1 ID3D12InfoQueue
                 ID3D12SharingContract
                 D3D12_DEBUG_COMMAND_LIST_GPU_BASED_VALIDATION_SETTINGS
                 D3D12_DEBUG_DEVICE_GPU_BASED_VALIDATION_SETTINGS
                 D3D12_DEBUG_DEVICE_GPU_SLOWDOWN_PERFORMANCE_FACTOR
                 D3D12_INFO_QUEUE_FILTER D3D12_INFO_QUEUE_FILTER_DESC D3D12_MESSAGE
                 D3D12_DEBUG_COMMAND_LIST_PARAMETER_TYPE
                 D3D12_DEBUG_DEVICE_PARAMETER_TYPE D3D12_DEBUG_FEATURE D3D12_GPU_BASED_VALIDATION_FLAGS
                 D3D12_GPU_BASED_VALIDATION_PIPELINE_STATE_CREATE_FLAGS
                 D3D12_GPU_BASED_VALIDATION_SHADER_PATCH_MODE D3D12_MESSAGE_CATEGORY D3D12_MESSAGE_ID
                 D3D12_MESSAGE_SEVERITY D3D12_RLDO_FLAGS]}
       )}.read("dx").eval(env).print();

    // TODO anonymous type lookup
    version(none) q{
      {:lib "ws2_32.dll"
          :include ["winsock2.h" "ws2tcpip.h"]
          :import [accept bind closesocket connect freeaddrinfo gai_strerror getaddrinfo
                   gethostbyaddr gethostbyname getnameinfo getpeername getprotobyname
                   getprotobynumber getservbyname getservbyport getsockname getsockopt
                   getsourcefilter htond htonf htonl htonll htons inet_addr inet_ntoa
                   ioctlsocket listen ntohd ntohf ntohl ntohll ntohs recv recvfrom
                   select send sendto setsockopt setsourcefilter shutdown socket
                   addrinfo AFPROTOCOLS BLOB fd_set hostent in_addr]}
    };

    // TODO namespaced type lookup
    version(none) q{
       {:lib "XAudio2_9.dll"
        :include ["xaudio2.h" "xaudio2fx.h" "x3daudio.h" "xapo.h" "xapofx.h" "hrtfapoapi.h"]
        :import [IXAPO IXAPOHrtfParameters IXAPOParameters IXAudio2 IXAudio2EngineCallback
                 IXAudio2MasteringVoice IXAudio2SourceVoice IXAudio2SubmixVoice
                 IXAudio2Voice IXAudio2VoiceCallback
                 FXECHO_INITDATA FXECHO_PARAMETERS FXEQ_PARAMETERS FXMASTERINGLIMITER_PARAMETERS
                 FXREVERB_PARAMETERS HrtfApoInit HrtfDirectivity HrtfDirectivityCardioid
                 HrtfDirectivityCone HrtfDistanceDecay HrtfOrientation HrtfPosition X3DAUDIO_CONE
                 X3DAUDIO_DISTANCE_CURVE X3DAUDIO_DISTANCE_CURVE_POINT X3DAUDIO_DSP_SETTINGS
                 X3DAUDIO_EMITTER X3DAUDIO_LISTENER XAPO_LOCKFORPROCESS_PARAMETERS
                 XAPO_PROCESS_BUFFER_PARAMETERS XAPO_REGISTRATION_PROPERTIES XAUDIO2_BUFFER
                 XAUDIO2_BUFFER_WMA XAUDIO2_DEBUG_CONFIGURATION XAUDIO2_EFFECT_CHAIN
                 XAUDIO2_EFFECT_DESCRIPTOR XAUDIO2_FILTER_PARAMETERS XAUDIO2_PERFORMANCE_DATA
                 XAUDIO2_SEND_DESCRIPTOR XAUDIO2_VOICE_DETAILS XAUDIO2_VOICE_SENDS
                 XAUDIO2_VOICE_STATE XAUDIO2FX_REVERB_I3DL2_PARAMETERS
                 XAUDIO2FX_REVERB_PARAMETERS XAUDIO2FX_VOLUMEMETER_LEVELS]}};

    // version(none)
    version(Windows)
      q{(do
          ;; TODO HRESULT error handling

          (print "Debug Strings")

          (def running true)
          (def debugger
           (fn ^_beginthreadex_proc_type debugger [_]
            (let [process-id (GetCurrentProcessId)
                  dbg-buf-ready (CreateEventA nil 0 1 "DBWIN_BUFFER_READY")
                  dbg-data-ready (CreateEventA nil 0 0 "DBWIN_DATA_READY")
                  dbg-file-map (CreateFileMappingA (cast @Void -1) nil PAGE_READWRITE 0u 4096u "DBWIN_BUFFER")
                  dbg-buf (MapViewOfFile dbg-file-map FILE_MAP_READ 0u 0u 0u)]
             (while running
              (if (= 0 (WaitForSingleObject dbg-data-ready 100u))
                (do (if (= process-id @(cast @UInt32 dbg-buf))
                       (printf (cast @UInt8 (+ (cast UInt64 dbg-buf) 4))))
                    (SetEvent dbg-buf-ready))))
             (UnmapViewOfFile dbg-buf)
             (CloseHandle dbg-file-map)
             (CloseHandle dbg-data-ready)
             (CloseHandle dbg-buf-ready)
             0u)))
          (def dbg-thread (if true #_(not (IsDebuggerPresent))
                           (cast HANDLE (_beginthreadex nil 0u debugger nil 0u nil))))

          (print "Window")
          (def ps (PAINTSTRUCT))
          (def wndproc (fn ^WNDPROC wndproc [hwnd msg l w]
                        (case msg
                         WM_DESTROY (do (PostQuitMessage 0) 0)
                         WM_ERASEBKGND 1
                         WM_PAINT (do (BeginPaint wnd &ps)
                                      (EndPaint wnd &ps)
                                      0)
                         (DefWindowProcA hwnd msg l w))))

          (def inst (GetModuleHandleA nil))
          (def wndclass (RegisterClassExA &(WNDCLASSEXA (sizeof WNDCLASSEXA)
                                            3u wndproc 0 0 inst
                                            nil nil nil nil "Hello" nil)))

          (def style-ex 0u)
          (def style 13565952u)
          (def rc (RECT 50 50 640 480))
          (AdjustWindowRectEx &rc style 0 style-ex)
          (def wnd (CreateWindowExA style-ex "Hello" "Hello Windows!" style
                    (.left rc) (.top rc) (.right rc) (.bottom rc) nil nil inst nil))

          (print "Debug")
          (def debug (@ID3D12Debug))
          (D3D12GetDebugInterface &IID_ID3D12Debug (cast @@Void &debug))
          (. debug EnableDebugLayer)
          (. debug Release)

          (def dxgi-info-queue (@IDXGIInfoQueue))
          (DXGIGetDebugInterface &IID_IDXGIInfoQueue (cast @@Void &dxgi-info-queue))
         ;; TODO enable debug output

          (print "Factory")
          (def dxgi-factory (@IDXGIFactory6))
          (CreateDXGIFactory2 DXGI_CREATE_FACTORY_DEBUG &IID_IDXGIFactory6 (cast @@Void &dxgi-factory))

          (print "Adapter")
          (def dxgi-adapter1 (@IDXGIAdapter1))
          (def dxgi-adapter (@IDXGIAdapter4))
          (. dxgi-factory EnumAdapters1 0u &dxgi-adapter1)
          (. dxgi-adapter1 QueryInterface &IID_IDXGIAdapter4 (cast @@Void &dxgi-adapter))
          (. dxgi-adapter1 Release)

          (print "Device")
          (def device (@ID3D12Device5))
          (def dbg-device (@ID3D12DebugDevice))
          (def dbg-device1 (@ID3D12DebugDevice1))
          (D3D12CreateDevice dxgi-adapter D3D_FEATURE_LEVEL_12_1 &IID_ID3D12Device5 (cast @@Void &device))
          (. device QueryInterface &IID_ID3D12DebugDevice (cast @@Void &dbg-device))
          (. device QueryInterface &IID_ID3D12DebugDevice1 (cast @@Void &dbg-device1))
          (. device SetName "Primary Device")
          (. dxgi-adapter Release)

          (print "Queue")
          (def queue-desc (D3D12_COMMAND_QUEUE_DESC D3D12_COMMAND_LIST_TYPE_DIRECT 0
                                                    D3D12_COMMAND_QUEUE_FLAG_NONE 0u))
          (def queue (@ID3D12CommandQueue))
          (. device CreateCommandQueue &queue-desc &IID_ID3D12CommandQueue (cast @@Void &queue))
          (. queue SetName "Graphics Queue")

          (print "Swap Chain")
          (def swap-chain-desc (DXGI_SWAP_CHAIN_DESC1 640u 480u DXGI_FORMAT_R8G8B8A8_UNORM 0
                                                      (DXGI_SAMPLE_DESC 1u 0u)
                                                      DXGI_USAGE_RENDER_TARGET_OUTPUT 2u
                                                      DXGI_SCALING_NONE DXGI_SWAP_EFFECT_FLIP_DISCARD))
          (def swap-chain1 (@IDXGISwapChain1))
          (def swap-chain (@IDXGISwapChain4))
          (. dxgi-factory CreateSwapChainForHwnd queue wnd &swap-chain-desc nil nil &swap-chain1)
         ;(. dxgi-factory MakeWindowAssociation wnd DXGI_MWA_NO_ALT_ENTER)
          (. swap-chain1 QueryInterface &IID_IDXGISwapChain4 (cast @@Void &swap-chain))
          (def frame-index (. swap-chain GetCurrentBackBufferIndex))
          (. dxgi-factory Release)
          (. swap-chain1 Release)

          (print "RTV Heap")
          (def rtv-desc (D3D12_DESCRIPTOR_HEAP_DESC 2 #_D3D12_DESCRIPTOR_HEAP_TYPE_RTV 2u
                                                    D3D12_DESCRIPTOR_HEAP_FLAG_NONE))
          (def rtv-heap (@ID3D12DescriptorHeap))
          (. device CreateDescriptorHeap &rtv-desc &IID_ID3D12DescriptorHeap (cast @@Void &rtv-heap))
          (. rtv-heap SetName "RTV Heap")
          (def rtv-size (. device GetDescriptorHandleIncrementSize 2 #_D3D12_DESCRIPTOR_HEAP_TYPE_RTV))
          (def rtv-handle (. rtv-heap GetCPUDescriptorHandleForHeapStart))

          (print "Render Targets")
         ;; TODO loops, array element dereference
          (def render-targets (array @ID3D12Resource 2u))
          (def render-target (@ID3D12Resource))
          (. swap-chain GetBuffer 0u &IID_ID3D12Resource (cast @@Void &render-target))
          (. device CreateRenderTargetView render-target nil rtv-handle)
          (. render-target SetName "Back Buffer #0")
          (aset render-targets 0u render-target)

         ;; TODO add to member rtv-handle.ptr
          (def rtv-handle (D3D12_CPU_DESCRIPTOR_HANDLE (+ (.ptr rtv-handle) rtv-size)))
          (. swap-chain GetBuffer 1u &IID_ID3D12Resource (cast @@Void &render-target))
          (. device CreateRenderTargetView render-target nil rtv-handle)
         (. render-target SetName "Back Buffer #1")
          (aset render-targets 1u render-target)

          (print "Cmd Allocator")
          (def cmd-alloc (@ID3D12CommandAllocator))
          (. device CreateCommandAllocator D3D12_COMMAND_LIST_TYPE_DIRECT
             &IID_ID3D12CommandAllocator (cast @@Void &cmd-alloc))
          (. cmd-alloc SetName "Main Allocator")

          (print "Root Signature")
          (def root-signature-desc
            (D3D12_ROOT_SIGNATURE_DESC 0u nil 0u nil D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT))
          (def root-signature (@ID3D12RootSignature))
          (def signature (@ID3DBlob))
          (def error     (@ID3DBlob))
          (D3D12SerializeRootSignature &root-signature-desc D3D_ROOT_SIGNATURE_VERSION_1 &signature &error)
          (. device CreateRootSignature 0u (. signature GetBufferPointer) (. signature GetBufferSize)
             &IID_ID3D12RootSignature (cast @@Void &root-signature))
          (. signature Release)
          (. root-signature SetName "Basic Root Signature")

          (print "Shaders")
          (def shaders "struct PSInput { float4 position : SV_POSITION; float4 color : COLOR; };
                        PSInput VSMain(float4 position : POSITION, float4 color : COLOR) {
                          PSInput result;
                          result.position = position;
                          result.color = color;
                          return result;
                        }
                        float4 PSMain(PSInput input) : SV_TARGET { return input.color; }")
          (def shader-flags 0u #_D3DCOMPILE_DEBUG) ; D3DCOMPILE_SKIP_OPTIMIZATION
          (def vertex-shader (@ID3DBlob))
          (def pixel-shader  (@ID3DBlob))
          (D3DCompile shaders (strlen shaders) "vertex" nil nil "VSMain" "vs_5_0"
                      shader-flags 0u &vertex-shader &error)
          (D3DCompile shaders (strlen shaders) "pixel" nil nil "PSMain" "ps_5_0"
                      shader-flags 0u &pixel-shader &error)

          (print "Graphics Pipeline")
          (def input-element-descs (array D3D12_INPUT_ELEMENT_DESC 2u))
          (aset input-element-descs 0u (D3D12_INPUT_ELEMENT_DESC "POSITION" 0u DXGI_FORMAT_R32G32B32_FLOAT 0u 0u
                                                                 D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA 0u))
          (aset input-element-descs 1u (D3D12_INPUT_ELEMENT_DESC "COLOR" 0u DXGI_FORMAT_R32G32B32A32_FLOAT 0u 12u
                                                                 D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA 0u))

          (def rtv-formats (array DXGI_FORMAT 1u))
          (aset rtv-formats 0u DXGI_FORMAT_R8G8B8A8_UNORM)

          (def blend-rts (array D3D12_RENDER_TARGET_BLEND_DESC 1u))
          (aset blend-rts 0u (D3D12_RENDER_TARGET_BLEND_DESC 0 0 0 0 0 0 0 0 0 15))

          (def gfx-pipeline-state-desc
           (D3D12_GRAPHICS_PIPELINE_STATE_DESC root-signature
            (D3D12_SHADER_BYTECODE (. vertex-shader GetBufferPointer) (. vertex-shader GetBufferSize))
            (D3D12_SHADER_BYTECODE (. pixel-shader GetBufferPointer) (. pixel-shader GetBufferSize))
            (D3D12_SHADER_BYTECODE) (D3D12_SHADER_BYTECODE) (D3D12_SHADER_BYTECODE)
            (D3D12_STREAM_OUTPUT_DESC)
            (D3D12_BLEND_DESC 0 0 blend-rts) 0xFFFFFFFFu
            (D3D12_RASTERIZER_DESC D3D12_FILL_MODE_SOLID D3D12_CULL_MODE_BACK 0 0 0f 0f 1)
            (D3D12_DEPTH_STENCIL_DESC)
            (D3D12_INPUT_LAYOUT_DESC (.ptr input-element-descs) 2u) 0
            D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE 1u
            rtv-formats DXGI_FORMAT_UNKNOWN (DXGI_SAMPLE_DESC 1u)))
          (def gfx-pipeline-state (@ID3D12PipelineState))
          (. device CreateGraphicsPipelineState &gfx-pipeline-state-desc &IID_ID3D12PipelineState
             (cast @@Void &gfx-pipeline-state))
          (. vertex-shader Release)
          (. pixel-shader Release)
          (. gfx-pipeline-state SetName "Basic Pipeline")

          (print "Cmd List")
          (def cmd-list (@ID3D12GraphicsCommandList4))
          (. device CreateCommandList 0u D3D12_COMMAND_LIST_TYPE_DIRECT
             cmd-alloc gfx-pipeline-state &IID_ID3D12GraphicsCommandList4 (cast @@Void &cmd-list))
          (. cmd-list Close)
          (. cmd-list SetName "Everything")

          (print "Vertex Data")
          (def aspect (/ 640f 480f))
          (def vertices (array Float32 [0f (* .25f aspect) 0f 1f 0f 0f 1f
                                        .25f (* -0.25f aspect) 0f 0f 1f 0f 1f
                                        -0.25f (* -0.25f aspect) 0f 0f 0f 1f 1f]))

          (print "Vertex Buffer")
          (def vertex-buffer-props (D3D12_HEAP_PROPERTIES D3D12_HEAP_TYPE_UPLOAD 0 0 1u 1u))
          (def vertex-buffer-desc (D3D12_RESOURCE_DESC D3D12_RESOURCE_DIMENSION_BUFFER
                                   0u (* 3u (* 4 (+ 3 4))) 1u 1u 1u DXGI_FORMAT_UNKNOWN
                                   (DXGI_SAMPLE_DESC 1u) D3D12_TEXTURE_LAYOUT_ROW_MAJOR))
          (def read-range (D3D12_RANGE))
          (def vertex-buffer (@ID3D12Resource))
          (def vertex-data (@Void))
          (. device CreateCommittedResource &vertex-buffer-props D3D12_HEAP_FLAG_NONE
             &vertex-buffer-desc 2755 #_D3D12_RESOURCE_STATE_GENERIC_READ nil
             &IID_ID3D12Resource (cast @@Void &vertex-buffer))
          (. vertex-buffer SetName "Triangle Vertices")
          (. vertex-buffer Map 0u &read-range &vertex-data)
          (memcpy vertex-data (.ptr vertices) 84u)
          (. vertex-buffer Unmap 0u nil)

          (def vertex-buffer-view (D3D12_VERTEX_BUFFER_VIEW (. vertex-buffer GetGPUVirtualAddress) 84u 28u))

          (print "Fence")
          (def fence-value 1u)
          (def fence-event (CreateEventA nil 0 0 nil))
          (def fence (@ID3D12Fence))
          (. device CreateFence 0u D3D12_FENCE_FLAG_NONE &IID_ID3D12Fence (cast @@Void &fence))
          (. fence SetName "Frame Fence")

          (def wait-for-previous-frame
           (fn []
             (let [n fence-value]
               (. queue Signal fence n)
               (def fence-value (+ 1 n))
               (if #_when (< (. fence GetCompletedValue) n)
                (do
                  (. fence SetEventOnCompletion n fence-event)
                  (WaitForSingleObject fence-event 0xFFFFFFFFu #_INFINITE)))
               (def frame-index (. swap-chain GetCurrentBackBufferIndex)))))

          (def viewport (D3D12_VIEWPORT 0f 0f 640f 480f 0f 1f))
          (def scissor (D3D12_RECT 0 0 640 480))

          (def clear-color (array Float32 4u))
          (aset clear-color 1u .2f)
          (aset clear-color 2u .4f)
          (aset clear-color 3u 1f)

          (def cmd-lists (array @ID3D12CommandList 1u))
          (aset cmd-lists 0u cmd-list)

          (def on-render
           (fn []
            (. cmd-alloc Reset)
            (. cmd-list Reset cmd-alloc gfx-pipeline-state)

            (. cmd-list SetGraphicsRootSignature root-signature)
            (. cmd-list RSSetViewports 1u &viewport)
            (. cmd-list RSSetScissorRects 1u &scissor)

            (let [barrier (D3D12_RESOURCE_BARRIER D3D12_RESOURCE_BARRIER_TYPE_TRANSITION
                           D3D12_RESOURCE_BARRIER_FLAG_NONE
                           (D3D12_RESOURCE_TRANSITION_BARRIER (aget render-targets frame-index)
                            D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES
                            D3D12_RESOURCE_STATE_PRESENT
                            D3D12_RESOURCE_STATE_RENDER_TARGET))]
             (. cmd-list ResourceBarrier 1u &barrier))

            (def rtv-handle (. rtv-heap GetCPUDescriptorHandleForHeapStart))
            (def rtv-handle (D3D12_CPU_DESCRIPTOR_HANDLE (+ (.ptr rtv-handle) (* frame-index rtv-size))))
            (. cmd-list OMSetRenderTargets 1u &rtv-handle 0 nil)

            (. cmd-list ClearRenderTargetView rtv-handle (.ptr clear-color) 0u nil)
            (. cmd-list IASetPrimitiveTopology D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST)
            (. cmd-list IASetVertexBuffers 0u 1u &vertex-buffer-view)
            (. cmd-list DrawInstanced 3u 1u 0u 0u)

            (let [barrier (D3D12_RESOURCE_BARRIER D3D12_RESOURCE_BARRIER_TYPE_TRANSITION
                           D3D12_RESOURCE_BARRIER_FLAG_NONE
                           (D3D12_RESOURCE_TRANSITION_BARRIER (aget render-targets frame-index)
                            D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES
                            D3D12_RESOURCE_STATE_RENDER_TARGET
                            D3D12_RESOURCE_STATE_PRESENT))]
             (. cmd-list ResourceBarrier 1u &barrier))

            (. cmd-list Close)
            (. queue ExecuteCommandLists 1u (.ptr cmd-lists))
            (. swap-chain Present 1u 0u)
            (wait-for-previous-frame)))

          (. dbg-device ReportLiveDeviceObjects D3D12_RLDO_DETAIL)
          (Sleep 500u)
         ;@(cast @UInt64 0) ; CRASH AW YEAH
          (def running false)
          (WaitForSingleObject dbg-thread 0xFFFFFFFFu)
          (CloseHandle dbg-thread)
          (def running true)

          (def msg (MSG))
          (def run
           (fn []
            (ShowWindow wnd 5)
            (UpdateWindow wnd)
            (while running
             (while (!= (PeekMessageA &msg nil 0u 0u 1u #_PM_REMOVE) 0)
               (if (= WM_QUIT (.message msg))
                 (def running false)
                 (do (TranslateMessage &msg)
                     (DispatchMessageA &msg))))
             (on-render))))
         )}.read("dxgi").eval(env).print;

    version(none)
    version(Windows)
    q{(do
        (print "HELLO WINDOWS")
        (def wndproc (fn ^WNDPROC wndproc [hwnd msg l w]
                      (print hwnd msg l w)
                      (DefWindowProcA hwnd msg l w)))
        (print "PROC")
        (def proc (fn ^_beginthread_proc_type proc [_]
                   (print "HI FROM THREAD")
                   (let [inst (GetModuleHandleA nil)
                         _ (print "INST " inst)
                         wndclass (RegisterClassExA &(WNDCLASSEXA
                                                      (sizeof WNDCLASSEXA)
                                                      3u wndproc 0 0 inst
                                                      nil nil nil nil "Hello" nil))
                         _ (print "WNDCLS " wndclass)
                         wnd (CreateWindowExA
                              0u "Hello" "Hello Windows!" 13565952u -1 -1 -1 -1 nil nil inst nil)
                         _ (print "WND " wnd)
                         msg (MSG)]
                    (ShowWindow wnd 5)
                    (UpdateWindow wnd)
                    (print "SHOW")
                    (while (GetMessageA &msg nil 0 0)
                      (TranslateMessage &msg)
                      (DispatchMessageA &msg))
                    0u)))
        (print "START THREAD")
        (_beginthread proc 4096u nil)
    )}.read("win32 app").eval(env).print;

    // version (none)
    version (linux)
    q{(ffi C
       {:wrap true}
       {:lib "libxcb.so"
        :include ["xcb/xcb.h"]
        :import [;; Constants
                 X_PROTOCOL
                 X_PROTOCOL_REVISION
                 X_TCP_PORT
                 XCB_CONN_ERROR
                 XCB_CONN_CLOSED_EXT_NOTSUPPORTED
                 XCB_CONN_CLOSED_MEM_INSUFFICIENT
                 XCB_CONN_CLOSED_REQ_LEN_EXCEED
                 XCB_CONN_CLOSED_PARSE_ERR
                 XCB_CONN_CLOSED_INVALID_SCREEN
                 XCB_CONN_CLOSED_FDPASSING_FAILED
                 #_XCB_TYPE_PAD
                 XCB_NONE
                 XCB_COPY_FROM_PARENT
                 XCB_CURRENT_TIME
                 XCB_NO_SYMBOL
                 XCB_KEY_PRESS
                 XCB_KEY_RELEASE
                 XCB_BUTTON_PRESS
                 XCB_BUTTON_RELEASE
                 XCB_MOTION_NOTIFY
                 XCB_ENTER_NOTIFY
                 XCB_LEAVE_NOTIFY
                 XCB_FOCUS_OUT
                 XCB_KEYMAP_NOTIFY
                 XCB_EXPOSE
                 XCB_GRAPHICS_EXPOSURE
                 XCB_NO_EXPOSURE
                 XCB_VISIBILITY_NOTIFY
                 XCB_CREATE_NOTIFY
                 XCB_DESTROY_NOTIFY
                 XCB_UNMAP_NOTIFY
                 XCB_MAP_NOTIFY
                 XCB_MAP_REQUEST
                 XCB_REPARENT_NOTIFY
                 XCB_CONFIGURE_NOTIFY
                 XCB_CONFIGURE_REQUEST
                 XCB_GRAVITY_NOTIFY
                 XCB_RESIZE_REQUEST
                 XCB_CIRCULATE_NOTIFY
                 XCB_CIRCULATE_REQUEST
                 XCB_PROPERTY_NOTIFY
                 XCB_SELECTION_CLEAR
                 XCB_SELECTION_REQUEST
                 XCB_SELECTION_NOTIFY
                 XCB_COLORMAP_NOTIFY
                 XCB_CLIENT_MESSAGE
                 XCB_MAPPING_NOTIFY
                 XCB_GE_GENERIC
                 XCB_REQUEST
                 XCB_VALUE
                 XCB_WINDOW
                 XCB_PIXMAP
                 XCB_ATOM
                 XCB_CURSOR
                 XCB_FONT
                 XCB_MATCH
                 XCB_DRAWABLE
                 XCB_ACCESS
                 XCB_ALLOC
                 XCB_COLORMAP
                 XCB_G_CONTEXT
                 XCB_ID_CHOICE
                 XCB_NAME
                 XCB_LENGTH
                 XCB_IMPLEMENTATION
                 XCB_CREATE_WINDOW
                 XCB_CHANGE_WINDOW_ATTRIBUTES
                 XCB_GET_WINDOW_ATTRIBUTES
                 XCB_DESTROY_WINDOW
                 XCB_DESTROY_SUBWINDOWS
                 XCB_CHANGE_SAVE_SET
                 XCB_REPARENT_WINDOW
                 XCB_MAP_WINDOW
                 XCB_MAP_SUBWINDOWS
                 XCB_UNMAP_WINDOW
                 XCB_UNMAP_SUBWINDOWS
                 XCB_CONFIGURE_WINDOW
                 XCB_CIRCULATE_WINDOW
                 XCB_GET_GEOMETRY
                 XCB_QUERY_TREE
                 XCB_INTERN_ATOM
                 XCB_GET_ATOM_NAME
                 XCB_CHANGE_PROPERTY
                 XCB_DELETE_PROPERTY
                 XCB_GET_PROPERTY
                 XCB_LIST_PROPERTIES
                 XCB_SET_SELECTION_OWNER
                 XCB_GET_SELECTION_OWNER
                 XCB_CONVERT_SELECTION
                 XCB_SEND_EVENT
                 XCB_GRAB_POINTER
                 XCB_UNGRAB_POINTER
                 XCB_GRAB_BUTTON
                 XCB_UNGRAB_BUTTON
                 XCB_CHANGE_ACTIVE_POINTER_GRAB
                 XCB_GRAB_KEYBOARD
                 XCB_UNGRAB_KEYBOARD
                 XCB_GRAB_KEY
                 XCB_UNGRAB_KEY
                 XCB_ALLOW_EVENTS
                 XCB_GRAB_SERVER
                 XCB_UNGRAB_SERVER
                 XCB_QUERY_POINTER
                 XCB_GET_MOTION_EVENTS
                 XCB_TRANSLATE_COORDINATES
                 XCB_WARP_POINTER
                 XCB_SET_INPUT_FOCUS
                 XCB_GET_INPUT_FOCUS
                 XCB_QUERY_KEYMAP
                 XCB_OPEN_FONT
                 XCB_CLOSE_FONT
                 XCB_QUERY_FONT
                 XCB_QUERY_TEXT_EXTENTS
                 XCB_LIST_FONTS
                 XCB_LIST_FONTS_WITH_INFO
                 XCB_SET_FONT_PATH
                 XCB_GET_FONT_PATH
                 XCB_CREATE_PIXMAP
                 XCB_FREE_PIXMAP
                 XCB_CREATE_GC
                 XCB_CHANGE_GC
                 XCB_COPY_GC
                 XCB_SET_DASHES
                 XCB_SET_CLIP_RECTANGLES
                 XCB_FREE_GC
                 XCB_CLEAR_AREA
                 XCB_COPY_AREA
                 XCB_COPY_PLANE
                 XCB_POLY_POINT
                 XCB_POLY_LINE
                 XCB_POLY_SEGMENT
                 XCB_POLY_RECTANGLE
                 XCB_POLY_ARC
                 XCB_FILL_POLY
                 XCB_POLY_FILL_RECTANGLE
                 XCB_POLY_FILL_ARC
                 XCB_PUT_IMAGE
                 XCB_GET_IMAGE
                 XCB_POLY_TEXT_8
                 XCB_POLY_TEXT_16
                 XCB_IMAGE_TEXT_8
                 XCB_IMAGE_TEXT_16
                 XCB_CREATE_COLORMAP
                 XCB_FREE_COLORMAP
                 XCB_COPY_COLORMAP_AND_FREE
                 XCB_INSTALL_COLORMAP
                 XCB_UNINSTALL_COLORMAP
                 XCB_LIST_INSTALLED_COLORMAPS
                 XCB_ALLOC_COLOR
                 XCB_ALLOC_NAMED_COLOR
                 XCB_ALLOC_COLOR_CELLS
                 XCB_ALLOC_COLOR_PLANES
                 XCB_FREE_COLORS
                 XCB_STORE_COLORS
                 XCB_STORE_NAMED_COLOR
                 XCB_QUERY_COLORS
                 XCB_LOOKUP_COLOR
                 XCB_CREATE_CURSOR
                 XCB_CREATE_GLYPH_CURSOR
                 XCB_FREE_CURSOR
                 XCB_RECOLOR_CURSOR
                 XCB_QUERY_BEST_SIZE
                 XCB_QUERY_EXTENSION
                 XCB_LIST_EXTENSIONS
                 XCB_CHANGE_KEYBOARD_MAPPING
                 XCB_GET_KEYBOARD_MAPPING
                 XCB_CHANGE_KEYBOARD_CONTROL
                 XCB_GET_KEYBOARD_CONTROL
                 XCB_BELL
                 XCB_CHANGE_POINTER_CONTROL
                 XCB_GET_POINTER_CONTROL
                 XCB_SET_SCREEN_SAVER
                 XCB_GET_SCREEN_SAVER
                 XCB_CHANGE_HOSTS
                 XCB_LIST_HOSTS
                 XCB_SET_ACCESS_CONTROL
                 XCB_SET_CLOSE_DOWN_MODE
                 XCB_KILL_CLIENT
                 XCB_ROTATE_PROPERTIES
                 XCB_FORCE_SCREEN_SAVER
                 XCB_SET_POINTER_MAPPING
                 XCB_GET_POINTER_MAPPING
                 XCB_SET_MODIFIER_MAPPING
                 XCB_GET_MODIFIER_MAPPING
                 XCB_NO_OPERATION
                 ;; Types
                 xcb_connection_t
                 xcb_generic_reply_t
                 xcb_generic_event_t
                 xcb_raw_generic_event_t
                 xcb_ge_event_t
                 xcb_generic_error_t
                 xcb_void_cookie_t
                 xcb_auth_info_t
                 xcb_extension_t
                 xcb_char2b_t
                 xcb_char2b_iterator_t
                 xcb_window_t
                 xcb_window_iterator_t
                 xcb_pixmap_t
                 xcb_pixmap_iterator_t
                 xcb_cursor_t
                 xcb_cursor_iterator_t
                 xcb_font_t
                 xcb_font_iterator_t
                 xcb_gcontext_t
                 xcb_gcontext_iterator_t
                 xcb_colormap_t
                 xcb_colormap_iterator_t
                 xcb_atom_t
                 xcb_atom_iterator_t
                 xcb_drawable_t
                 xcb_drawable_iterator_t
                 xcb_fontable_t
                 xcb_fontable_iterator_t
                 xcb_bool32_t
                 xcb_bool32_iterator_t
                 xcb_visualid_t
                 xcb_visualid_iterator_t
                 xcb_timestamp_t
                 xcb_timestamp_iterator_t
                 xcb_keysym_t
                 xcb_keysym_iterator_t
                 xcb_keycode_t
                 xcb_keycode_iterator_t
                 xcb_keycode32_t
                 xcb_keycode32_iterator_t
                 xcb_button_t
                 xcb_button_iterator_t
                 xcb_point_t
                 xcb_point_iterator_t
                 xcb_rectangle_t
                 xcb_rectangle_iterator_t
                 xcb_arc_t
                 xcb_arc_iterator_t
                 xcb_format_t
                 xcb_format_iterator_t
                 xcb_visual_class_t
                 xcb_visualtype_t
                 xcb_visualtype_iterator_t
                 xcb_depth_t
                 xcb_depth_iterator_t
                 xcb_event_mask_t
                 xcb_backing_store_t
                 xcb_screen_t
                 xcb_screen_iterator_t
                 xcb_setup_request_t
                 xcb_setup_request_iterator_t
                 xcb_setup_failed_t
                 xcb_setup_failed_iterator_t
                 xcb_setup_authenticate_t
                 xcb_setup_authenticate_iterator_t
                 xcb_image_order_t
                 xcb_setup_t
                 xcb_setup_iterator_t
                 xcb_mod_mask_t
                 xcb_key_but_mask_t
                 xcb_window_enum_t
                 xcb_key_press_event_t
                 xcb_key_release_event_t
                 xcb_button_mask_t
                 xcb_button_press_event_t
                 xcb_button_release_event_t
                 xcb_motion_t
                 xcb_motion_notify_event_t
                 xcb_notify_detail_t
                 xcb_notify_mode_t
                 xcb_enter_notify_event_t
                 xcb_leave_notify_event_t
                 xcb_focus_in_event_t
                 xcb_focus_out_event_t
                 xcb_keymap_notify_event_t
                 xcb_expose_event_t
                 xcb_graphics_exposure_event_t
                 xcb_no_exposure_event_t
                 xcb_visibility_t
                 xcb_visibility_notify_event_t
                 xcb_create_notify_event_t
                 xcb_destroy_notify_event_t
                 xcb_unmap_notify_event_t
                 xcb_map_notify_event_t
                 xcb_map_request_event_t
                 xcb_reparent_notify_event_t
                 xcb_configure_notify_event_t
                 xcb_configure_request_event_t
                 xcb_gravity_notify_event_t
                 xcb_resize_request_event_t
                 xcb_place_t
                 xcb_circulate_notify_event_t
                 xcb_circulate_request_event_t
                 xcb_property_t
                 xcb_property_notify_event_t
                 xcb_selection_clear_event_t
                 xcb_time_t
                 xcb_atom_enum_t
                 xcb_selection_request_event_t
                 xcb_selection_notify_event_t
                 xcb_colormap_state_t
                 xcb_colormap_enum_t
                 xcb_colormap_notify_event_t
                 xcb_client_message_data_t
                 xcb_client_message_data_iterator_t
                 xcb_client_message_event_t
                 xcb_mapping_t
                 xcb_mapping_notify_event_t
                 xcb_ge_generic_event_t
                 xcb_request_error_t
                 xcb_value_error_t
                 xcb_window_error_t
                 xcb_pixmap_error_t
                 xcb_atom_error_t
                 xcb_cursor_error_t
                 xcb_font_error_t
                 xcb_match_error_t
                 xcb_drawable_error_t
                 xcb_access_error_t
                 xcb_alloc_error_t
                 xcb_colormap_error_t
                 xcb_g_context_error_t
                 xcb_id_choice_error_t
                 xcb_name_error_t
                 xcb_length_error_t
                 xcb_implementation_error_t
                 xcb_window_class_t
                 xcb_cw_t
                 xcb_back_pixmap_t
                 xcb_gravity_t
                 xcb_create_window_value_list_t
                 xcb_create_window_request_t
                 xcb_change_window_attributes_value_list_t
                 xcb_change_window_attributes_request_t
                 xcb_map_state_t
                 xcb_get_window_attributes_cookie_t
                 xcb_get_window_attributes_request_t
                 xcb_get_window_attributes_reply_t
                 xcb_destroy_window_request_t
                 xcb_destroy_subwindows_request_t
                 xcb_set_mode_t
                 xcb_change_save_set_request_t
                 xcb_reparent_window_request_t
                 xcb_map_window_request_t
                 xcb_map_subwindows_request_t
                 xcb_unmap_window_request_t
                 xcb_unmap_subwindows_request_t
                 xcb_config_window_t
                 xcb_stack_mode_t
                 xcb_configure_window_value_list_t
                 xcb_configure_window_request_t
                 xcb_circulate_t
                 xcb_circulate_window_request_t
                 xcb_get_geometry_cookie_t
                 xcb_get_geometry_request_t
                 xcb_get_geometry_reply_t
                 xcb_query_tree_cookie_t
                 xcb_query_tree_request_t
                 xcb_query_tree_reply_t
                 xcb_intern_atom_cookie_t
                 xcb_intern_atom_request_t
                 xcb_intern_atom_reply_t
                 xcb_get_atom_name_cookie_t
                 xcb_get_atom_name_request_t
                 xcb_get_atom_name_reply_t
                 xcb_prop_mode_t
                 xcb_change_property_request_t
                 xcb_delete_property_request_t
                 xcb_get_property_type_t
                 xcb_get_property_cookie_t
                 xcb_get_property_request_t
                 xcb_get_property_reply_t
                 xcb_list_properties_cookie_t
                 xcb_list_properties_request_t
                 xcb_list_properties_reply_t
                 xcb_set_selection_owner_request_t
                 xcb_get_selection_owner_cookie_t
                 xcb_get_selection_owner_request_t
                 xcb_get_selection_owner_reply_t
                 xcb_convert_selection_request_t
                 xcb_send_event_dest_t
                 xcb_send_event_request_t
                 xcb_grab_mode_t
                 xcb_grab_status_t
                 xcb_cursor_enum_t
                 xcb_grab_pointer_cookie_t
                 xcb_grab_pointer_request_t
                 xcb_grab_pointer_reply_t
                 xcb_ungrab_pointer_request_t
                 xcb_button_index_t
                 xcb_grab_button_request_t
                 xcb_ungrab_button_request_t
                 xcb_change_active_pointer_grab_request_t
                 xcb_grab_keyboard_cookie_t
                 xcb_grab_keyboard_request_t
                 xcb_grab_keyboard_reply_t
                 xcb_ungrab_keyboard_request_t
                 xcb_grab_t
                 xcb_grab_key_request_t
                 xcb_ungrab_key_request_t
                 xcb_allow_t
                 xcb_allow_events_request_t
                 xcb_grab_server_request_t
                 xcb_ungrab_server_request_t
                 xcb_query_pointer_cookie_t
                 xcb_query_pointer_request_t
                 xcb_query_pointer_reply_t
                 xcb_timecoord_t
                 xcb_timecoord_iterator_t
                 xcb_get_motion_events_cookie_t
                 xcb_get_motion_events_request_t
                 xcb_get_motion_events_reply_t
                 xcb_translate_coordinates_cookie_t
                 xcb_translate_coordinates_request_t
                 xcb_translate_coordinates_reply_t
                 xcb_warp_pointer_request_t
                 xcb_input_focus_t
                 xcb_set_input_focus_request_t
                 xcb_get_input_focus_cookie_t
                 xcb_get_input_focus_request_t
                 xcb_get_input_focus_reply_t
                 xcb_query_keymap_cookie_t
                 xcb_query_keymap_request_t
                 xcb_query_keymap_reply_t
                 xcb_open_font_request_t
                 xcb_close_font_request_t
                 xcb_font_draw_t
                 xcb_fontprop_t
                 xcb_fontprop_iterator_t
                 xcb_charinfo_t
                 xcb_charinfo_iterator_t
                 xcb_query_font_cookie_t
                 xcb_query_font_request_t
                 xcb_query_font_reply_t
                 xcb_query_text_extents_cookie_t
                 xcb_query_text_extents_request_t
                 xcb_query_text_extents_reply_t
                 xcb_str_t
                 xcb_str_iterator_t
                 xcb_list_fonts_cookie_t
                 xcb_list_fonts_request_t
                 xcb_list_fonts_reply_t
                 xcb_list_fonts_with_info_cookie_t
                 xcb_list_fonts_with_info_request_t
                 xcb_list_fonts_with_info_reply_t
                 xcb_set_font_path_request_t
                 xcb_get_font_path_cookie_t
                 xcb_get_font_path_request_t
                 xcb_get_font_path_reply_t
                 xcb_create_pixmap_request_t
                 xcb_free_pixmap_request_t
                 xcb_gc_t
                 xcb_gx_t
                 xcb_line_style_t
                 xcb_cap_style_t
                 xcb_join_style_t
                 xcb_fill_style_t
                 xcb_fill_rule_t
                 xcb_subwindow_mode_t
                 xcb_arc_mode_t
                 xcb_create_gc_value_list_t
                 xcb_create_gc_request_t
                 xcb_change_gc_value_list_t
                 xcb_change_gc_request_t
                 xcb_copy_gc_request_t
                 xcb_set_dashes_request_t
                 xcb_clip_ordering_t
                 xcb_set_clip_rectangles_request_t
                 xcb_free_gc_request_t
                 xcb_clear_area_request_t
                 xcb_copy_area_request_t
                 xcb_copy_plane_request_t
                 xcb_coord_mode_t
                 xcb_poly_point_request_t
                 xcb_poly_line_request_t
                 xcb_segment_t
                 xcb_segment_iterator_t
                 xcb_poly_segment_request_t
                 xcb_poly_rectangle_request_t
                 xcb_poly_arc_request_t
                 xcb_poly_shape_t
                 xcb_fill_poly_request_t
                 xcb_poly_fill_rectangle_request_t
                 xcb_poly_fill_arc_request_t
                 xcb_image_format_t
                 xcb_put_image_request_t
                 xcb_get_image_cookie_t
                 xcb_get_image_request_t
                 xcb_get_image_reply_t
                 xcb_poly_text_8_request_t
                 xcb_poly_text_16_request_t
                 xcb_image_text_8_request_t
                 xcb_image_text_16_request_t
                 xcb_colormap_alloc_t
                 xcb_create_colormap_request_t
                 xcb_free_colormap_request_t
                 xcb_copy_colormap_and_free_request_t
                 xcb_install_colormap_request_t
                 xcb_uninstall_colormap_request_t
                 xcb_list_installed_colormaps_cookie_t
                 xcb_list_installed_colormaps_request_t
                 xcb_list_installed_colormaps_reply_t
                 xcb_alloc_color_cookie_t
                 xcb_alloc_color_request_t
                 xcb_alloc_color_reply_t
                 xcb_alloc_named_color_cookie_t
                 xcb_alloc_named_color_request_t
                 xcb_alloc_named_color_reply_t
                 xcb_alloc_color_cells_cookie_t
                 xcb_alloc_color_cells_request_t
                 xcb_alloc_color_cells_reply_t
                 xcb_alloc_color_planes_cookie_t
                 xcb_alloc_color_planes_request_t
                 xcb_alloc_color_planes_reply_t
                 xcb_free_colors_request_t
                 xcb_color_flag_t
                 xcb_coloritem_t
                 xcb_coloritem_iterator_t
                 xcb_store_colors_request_t
                 xcb_store_named_color_request_t
                 xcb_rgb_t
                 xcb_rgb_iterator_t
                 xcb_query_colors_cookie_t
                 xcb_query_colors_request_t
                 xcb_query_colors_reply_t
                 xcb_lookup_color_cookie_t
                 xcb_lookup_color_request_t
                 xcb_lookup_color_reply_t
                 xcb_lookup_color_request_t
                 xcb_lookup_color_reply_t
                 xcb_pixmap_enum_t
                 xcb_create_cursor_request_t
                 xcb_font_enum_t
                 xcb_create_glyph_cursor_request_t
                 xcb_free_cursor_request_t
                 xcb_recolor_cursor_request_t
                 xcb_query_shape_of_t
                 xcb_query_best_size_cookie_t
                 xcb_query_best_size_request_t
                 xcb_query_best_size_reply_t
                 xcb_query_extension_cookie_t
                 xcb_query_extension_request_t
                 xcb_query_extension_reply_t
                 xcb_list_extensions_cookie_t
                 xcb_list_extensions_request_t
                 xcb_list_extensions_reply_t
                 xcb_change_keyboard_mapping_request_t
                 xcb_get_keyboard_mapping_cookie_t
                 xcb_get_keyboard_mapping_request_t
                 xcb_get_keyboard_mapping_reply_t
                 xcb_kb_t
                 xcb_led_mode_t
                 xcb_auto_repeat_mode_t
                 xcb_change_keyboard_control_value_list_t
                 xcb_change_keyboard_control_request_t
                 xcb_get_keyboard_control_cookie_t
                 xcb_get_keyboard_control_request_t
                 xcb_get_keyboard_control_reply_t
                 xcb_bell_request_t
                 xcb_change_pointer_control_request_t
                 xcb_get_pointer_control_cookie_t
                 xcb_get_pointer_control_request_t
                 xcb_get_pointer_control_reply_t
                 xcb_blanking_t
                 xcb_exposures_t
                 xcb_set_screen_saver_request_t
                 xcb_get_screen_saver_cookie_t
                 xcb_get_screen_saver_request_t
                 xcb_get_screen_saver_reply_t
                 xcb_host_mode_t
                 xcb_family_t
                 xcb_change_hosts_request_t
                 xcb_host_t
                 xcb_host_iterator_t
                 xcb_list_hosts_cookie_t
                 xcb_list_hosts_request_t
                 xcb_list_hosts_reply_t
                 xcb_access_control_t
                 xcb_set_access_control_request_t
                 xcb_close_down_t
                 xcb_set_close_down_mode_request_t
                 xcb_kill_t
                 xcb_kill_client_request_t
                 xcb_rotate_properties_request_t
                 xcb_screen_saver_t
                 xcb_force_screen_saver_request_t
                 xcb_mapping_status_t
                 xcb_set_pointer_mapping_cookie_t
                 xcb_set_pointer_mapping_request_t
                 xcb_set_pointer_mapping_reply_t
                 xcb_get_pointer_mapping_cookie_t
                 xcb_get_pointer_mapping_request_t
                 xcb_get_pointer_mapping_reply_t
                 xcb_map_index_t
                 xcb_set_modifier_mapping_cookie_t
                 xcb_set_modifier_mapping_request_t
                 xcb_set_modifier_mapping_reply_t
                 xcb_get_modifier_mapping_cookie_t
                 xcb_get_modifier_mapping_request_t
                 xcb_get_modifier_mapping_reply_t
                 xcb_no_operation_request_t
                 ;; Functions
                 xcb_flush
                 xcb_get_maximum_request_length
                 xcb_prefetch_maximum_request_length
                 xcb_wait_for_event
                 xcb_poll_for_event
                 xcb_poll_for_queued_event
                 xcb_wait_for_special_event
                 xcb_register_for_special_xge
                 xcb_unregister_for_special_event
                 xcb_request_check
                 xcb_discard_reply
                 xcb_discard_reply64
                 xcb_get_extension_data
                 xcb_prefetch_extension_data
                 xcb_get_setup
                 xcb_get_file_descriptor
                 xcb_connection_has_error
                 xcb_connect_to_fd
                 xcb_disconnect
                 xcb_parse_display
                 xcb_connect
                 xcb_connect_to_display_with_auth_info
                 xcb_generate_id
                 xcb_char2b_next
                 xcb_char2b_end
                 xcb_window_next
                 xcb_window_end
                 xcb_pixmap_next
                 xcb_pixmap_end
                 xcb_cursor_next
                 xcb_cursor_end
                 xcb_font_next
                 xcb_font_end
                 xcb_gcontext_next
                 xcb_gcontext_end
                 xcb_colormap_next
                 xcb_colormap_end
                 xcb_atom_next
                 xcb_atom_end
                 xcb_drawable_next
                 xcb_drawable_end
                 xcb_fontable_next
                 xcb_fontable_end
                 xcb_bool32_next
                 xcb_bool32_end
                 xcb_visualid_next
                 xcb_visualid_end
                 xcb_timestamp_next
                 xcb_timestamp_end
                 xcb_keysym_next
                 xcb_keysym_end
                 xcb_keycode_next
                 xcb_keycode_end
                 xcb_keycode32_next
                 xcb_keycode32_end
                 xcb_button_next
                 xcb_button_end
                 xcb_point_next
                 xcb_point_end
                 xcb_rectangle_next
                 xcb_rectangle_end
                 xcb_arc_next
                 xcb_arc_end
                 xcb_format_next
                 xcb_format_end
                 xcb_visualtype_next
                 xcb_visualtype_end
                 xcb_depth_sizeof
                 xcb_depth_visuals
                 xcb_depth_visuals_length
                 xcb_depth_visuals_iterator
                 xcb_depth_next
                 xcb_depth_end
                 xcb_screen_sizeof
                 xcb_screen_allowed_depths_length
                 xcb_screen_allowed_depths_iterator
                 xcb_screen_next
                 xcb_screen_end
                 xcb_setup_request_sizeof
                 xcb_setup_request_authorization_protocol_name
                 xcb_setup_request_authorization_protocol_name_length
                 xcb_setup_request_authorization_protocol_name_end
                 xcb_setup_request_authorization_protocol_data
                 xcb_setup_request_authorization_protocol_data_length
                 xcb_setup_request_authorization_protocol_data_end
                 xcb_setup_request_next
                 xcb_setup_request_end
                 xcb_setup_failed_sizeof
                 xcb_setup_failed_reason
                 xcb_setup_failed_reason_length
                 xcb_setup_failed_reason_end
                 xcb_setup_failed_next
                 xcb_setup_failed_end
                 xcb_setup_authenticate_sizeof
                 xcb_setup_authenticate_reason
                 xcb_setup_authenticate_reason_length
                 xcb_setup_authenticate_reason_end
                 xcb_setup_authenticate_next
                 xcb_setup_authenticate_end
                 xcb_setup_sizeof
                 xcb_setup_vendor
                 xcb_setup_vendor_length
                 xcb_setup_vendor_end
                 xcb_setup_pixmap_formats
                 xcb_setup_pixmap_formats_length
                 xcb_setup_pixmap_formats_iterator
                 xcb_setup_roots_length
                 xcb_setup_roots_iterator
                 xcb_setup_next
                 xcb_setup_end
                 xcb_client_message_data_end
                 xcb_client_message_data_end
                 xcb_create_window_value_list_serialize
                 xcb_create_window_value_list_unpack
                 xcb_create_window_value_list_sizeof
                 xcb_create_window_sizeof
                 xcb_create_window_checked
                 xcb_create_window
                 xcb_create_window_aux_checked
                 xcb_create_window_aux
                 xcb_create_window_value_list
                 xcb_change_window_attributes_value_list_serialize
                 xcb_change_window_attributes_value_list_unpack
                 xcb_change_window_attributes_value_list_sizeof
                 xcb_change_window_attributes_sizeof
                 xcb_change_window_attributes_checked
                 xcb_change_window_attributes
                 xcb_change_window_attributes_aux_checked
                 xcb_change_window_attributes_aux
                 xcb_change_window_attributes_value_list
                 xcb_get_window_attributes
                 xcb_get_window_attributes_unchecked
                 xcb_get_window_attributes_reply
                 xcb_destroy_window_checked
                 xcb_destroy_window
                 xcb_destroy_subwindows_checked
                 xcb_destroy_subwindows
                 xcb_change_save_set_checked
                 xcb_change_save_set
                 xcb_reparent_window_checked
                 xcb_reparent_window
                 xcb_map_window_checked
                 xcb_map_window
                 xcb_map_subwindows_checked
                 xcb_map_subwindows
                 xcb_unmap_window_checked
                 xcb_unmap_window
                 xcb_unmap_subwindows_checked
                 xcb_unmap_subwindows
                 xcb_configure_window_value_list_serialize
                 xcb_configure_window_value_list_unpack
                 xcb_configure_window_value_list_sizeof
                 xcb_configure_window_sizeof
                 xcb_configure_window_checked
                 xcb_configure_window
                 xcb_configure_window_aux_checked
                 xcb_configure_window_aux
                 xcb_configure_window_value_list
                 xcb_circulate_window_checked
                 xcb_circulate_window
                 xcb_get_geometry
                 xcb_get_geometry_unchecked
                 xcb_get_geometry_reply
                 xcb_query_tree
                 xcb_query_tree_unchecked
                 xcb_query_tree_children
                 xcb_query_tree_children_length
                 xcb_query_tree_children_end
                 xcb_query_tree_reply
                 xcb_intern_atom_sizeof
                 xcb_intern_atom
                 xcb_intern_atom_unchecked
                 xcb_intern_atom_reply
                 xcb_get_atom_name_sizeof
                 xcb_get_atom_name
                 xcb_get_atom_name_unchecked
                 xcb_get_atom_name_name
                 xcb_get_atom_name_name_length
                 xcb_get_atom_name_name_end
                 xcb_get_atom_name_reply
                 xcb_change_property_sizeof
                 xcb_change_property_checked
                 xcb_change_property
                 xcb_change_property_data
                 xcb_change_property_data_length
                 xcb_change_property_data_end
                 xcb_delete_property_checked
                 xcb_delete_property
                 xcb_get_property_sizeof
                 xcb_get_property
                 xcb_get_property_unchecked
                 xcb_get_property_value
                 xcb_get_property_value_length
                 xcb_get_property_value_end
                 xcb_get_property_reply
                 xcb_list_properties
                 xcb_list_properties_unchecked
                 xcb_list_properties_atoms
                 xcb_list_properties_atoms_length
                 xcb_list_properties_atoms_end
                 xcb_list_properties_reply
                 xcb_set_selection_owner_checked
                 xcb_set_selection_owner
                 xcb_get_selection_owner
                 xcb_get_selection_owner_unchecked
                 xcb_get_selection_owner_reply
                 xcb_convert_selection_checked
                 xcb_convert_selection
                 xcb_send_event_checked
                 xcb_send_event
                 xcb_grab_pointer
                 xcb_grab_pointer_unchecked
                 xcb_grab_pointer_reply
                 xcb_ungrab_pointer_checked
                 xcb_ungrab_pointer
                 xcb_grab_button_checked
                 xcb_grab_button
                 xcb_ungrab_button_checked
                 xcb_ungrab_button
                 xcb_change_active_pointer_grab_checked
                 xcb_change_active_pointer_grab
                 xcb_grab_keyboard
                 xcb_grab_keyboard_unchecked
                 xcb_grab_keyboard_reply
                 xcb_ungrab_keyboard_checked
                 xcb_ungrab_keyboard
                 xcb_grab_key_checked
                 xcb_grab_key
                 xcb_ungrab_key_checked
                 xcb_ungrab_key
                 xcb_allow_events_checked
                 xcb_allow_events
                 xcb_grab_server_checked
                 xcb_grab_server
                 xcb_ungrab_server_checked
                 xcb_ungrab_server
                 xcb_query_pointer
                 xcb_query_pointer_unchecked
                 xcb_query_pointer_reply
                 xcb_timecoord_next
                 xcb_timecoord_end
                 xcb_get_motion_events_sizeof
                 xcb_get_motion_events
                 xcb_get_motion_events_unchecked
                 xcb_get_motion_events_events
                 xcb_get_motion_events_events_length
                 xcb_get_motion_events_events_iterator
                 xcb_get_motion_events_reply
                 xcb_translate_coordinates
                 xcb_translate_coordinates_unchecked
                 xcb_warp_pointer_checked
                 xcb_warp_pointer
                 xcb_set_input_focus_checked
                 xcb_set_input_focus
                 xcb_get_input_focus
                 xcb_get_input_focus_unchecked
                 xcb_get_input_focus_reply
                 xcb_query_keymap
                 xcb_query_keymap_unchecked
                 xcb_query_keymap_reply
                 xcb_open_font_sizeof
                 xcb_open_font_checked
                 xcb_open_font
                 xcb_open_font_name
                 xcb_open_font_name_length
                 xcb_open_font_name_end
                 xcb_close_font_checked
                 xcb_close_font
                 xcb_fontprop_next
                 xcb_fontprop_end
                 xcb_charinfo_next
                 xcb_charinfo_end
                 xcb_query_font_sizeof
                 xcb_query_font
                 xcb_query_font_unchecked
                 xcb_query_font_properties
                 xcb_query_font_properties_length
                 xcb_query_font_properties_iterator
                 xcb_query_font_char_infos
                 xcb_query_font_char_infos_length
                 xcb_query_font_char_infos_iterator
                 xcb_query_font_reply
                 xcb_query_text_extents
                 xcb_query_text_extents_unchecked
                 xcb_query_text_extents_reply
                 xcb_str_sizeof
                 xcb_str_name
                 xcb_str_name_length
                 xcb_str_name_end
                 xcb_str_next
                 xcb_str_end
                 xcb_list_fonts_sizeof
                 xcb_list_fonts
                 xcb_list_fonts_unchecked
                 xcb_list_fonts_names_length
                 xcb_list_fonts_names_iterator
                 xcb_list_fonts_reply
                 xcb_list_fonts_with_info_sizeof
                 xcb_list_fonts_with_info
                 xcb_list_fonts_with_info_unchecked
                 xcb_list_fonts_with_info_properties
                 xcb_list_fonts_with_info_properties_length
                 xcb_list_fonts_with_info_properties_iterator
                 xcb_list_fonts_with_info_name
                 xcb_list_fonts_with_info_name_length
                 xcb_list_fonts_with_info_name_end
                 xcb_list_fonts_with_info_reply
                 xcb_set_font_path_sizeof
                 xcb_set_font_path_checked
                 xcb_set_font_path
                 xcb_set_font_path_font_length
                 xcb_set_font_path_font_iterator
                 xcb_set_font_path_sizeof
                 xcb_get_font_path
                 xcb_get_font_path_unchecked
                 xcb_get_font_path_path_length
                 xcb_get_font_path_path_iterator
                 xcb_get_font_path_reply
                 xcb_create_pixmap_checked
                 xcb_create_pixmap
                 xcb_free_pixmap_checked
                 xcb_free_pixmap
                 xcb_create_gc_value_list_serialize
                 xcb_create_gc_value_list_unpack
                 xcb_create_gc_value_list_sizeof
                 xcb_create_gc_sizeof
                 xcb_create_gc_checked
                 xcb_create_gc
                 xcb_create_gc_aux_checked
                 xcb_create_gc_aux
                 xcb_create_gc_value_list
                 xcb_change_gc_value_list_serialize
                 xcb_change_gc_value_list_unpack
                 xcb_change_gc_value_list_sizeof
                 xcb_change_gc_sizeof
                 xcb_change_gc_checked
                 xcb_change_gc
                 xcb_change_gc_aux_checked
                 xcb_change_gc_aux
                 xcb_change_gc_value_list
                 xcb_copy_gc_checked
                 xcb_copy_gc
                 xcb_set_dashes_sizeof
                 xcb_set_dashes_checked
                 xcb_set_dashes
                 xcb_set_dashes_dashes
                 xcb_set_dashes_dashes_length
                 xcb_set_dashes_dashes_end
                 xcb_set_clip_rectangles_sizeof
                 xcb_set_clip_rectangles_checked
                 xcb_set_clip_rectangles
                 xcb_set_clip_rectangles_rectangles
                 xcb_set_clip_rectangles_rectangles_length
                 xcb_set_clip_rectangles_rectangles_iterator
                 xcb_free_gc_checked
                 xcb_free_gc
                 xcb_clear_area_checked
                 xcb_clear_area
                 xcb_copy_area_checked
                 xcb_copy_area
                 xcb_copy_plane_checked
                 xcb_copy_plane
                 xcb_poly_point_sizeof
                 xcb_poly_point_checked
                 xcb_poly_point
                 xcb_poly_point_points
                 xcb_poly_point_points_length
                 xcb_poly_point_points_iterator
                 xcb_poly_line_sizeof
                 xcb_poly_line_checked
                 xcb_poly_line
                 xcb_poly_line_points
                 xcb_poly_line_points_length
                 xcb_poly_line_points_iterator
                 xcb_segment_next
                 xcb_segment_end
                 xcb_poly_segment_sizeof
                 xcb_poly_segment_checked
                 xcb_poly_segment
                 xcb_poly_segment_segments
                 xcb_poly_segment_segments_length
                 xcb_poly_segment_segments_iterator
                 xcb_poly_rectangle_sizeof
                 xcb_poly_rectangle_checked
                 xcb_poly_rectangle
                 xcb_poly_rectangle_rectangles
                 xcb_poly_rectangle_rectangles_length
                 xcb_poly_rectangle_rectangles_iterator
                 xcb_poly_arc_checked
                 xcb_poly_arc
                 xcb_poly_arc_arcs
                 xcb_poly_arc_arcs_length
                 xcb_poly_arc_arcs_iterator
                 xcb_fill_poly_sizeof
                 xcb_fill_poly_checked
                 xcb_fill_poly
                 xcb_fill_poly_points
                 xcb_fill_poly_points_length
                 xcb_fill_poly_points_iterator
                 xcb_poly_fill_rectangle_sizeof
                 xcb_poly_fill_rectangle_checked
                 xcb_poly_fill_rectangle
                 xcb_poly_fill_rectangle_rectangles
                 xcb_poly_fill_rectangle_rectangles_length
                 xcb_poly_fill_rectangle_rectangles_iterator
                 xcb_poly_fill_arc_sizeof
                 xcb_poly_fill_arc_checked
                 xcb_poly_fill_arc
                 xcb_poly_fill_arc_arcs
                 xcb_poly_fill_arc_arcs_length
                 xcb_poly_fill_arc_arcs_iterator
                 xcb_put_image_sizeof
                 xcb_put_image_checked
                 xcb_put_image
                 xcb_put_image_data
                 xcb_put_image_data_length
                 xcb_put_image_data_end
                 xcb_get_image_sizeof
                 xcb_get_image
                 xcb_get_image_unchecked
                 xcb_get_image_data
                 xcb_get_image_data_length
                 xcb_get_image_data_end
                 xcb_get_image_reply
                 xcb_poly_text_8_sizeof
                 xcb_poly_text_8_checked
                 xcb_poly_text_8
                 xcb_poly_text_8_items
                 xcb_poly_text_8_items_length
                 xcb_poly_text_8_items_end
                 xcb_poly_text_16_sizeof
                 xcb_poly_text_16_checked
                 xcb_poly_text_16
                 xcb_poly_text_16_items
                 xcb_poly_text_16_items_length
                 xcb_poly_text_16_items_end
                 xcb_image_text_8_sizeof
                 xcb_image_text_8_checked
                 xcb_image_text_8
                 xcb_image_text_8_string
                 xcb_image_text_8_string_length
                 xcb_image_text_8_string_end
                 xcb_image_text_16_sizeof
                 xcb_image_text_16_checked
                 xcb_image_text_16
                 xcb_image_text_16_string
                 xcb_image_text_16_string_length
                 xcb_image_text_16_string_iterator
                 xcb_create_colormap_checked
                 xcb_create_colormap
                 xcb_free_colormap_checked
                 xcb_free_colormap
                 xcb_copy_colormap_and_free_checked
                 xcb_copy_colormap_and_free
                 xcb_install_colormap_checked
                 xcb_install_colormap
                 xcb_uninstall_colormap_checked
                 xcb_uninstall_colormap
                 xcb_list_installed_colormaps_sizeof
                 xcb_list_installed_colormaps
                 xcb_list_installed_colormaps_unchecked
                 xcb_list_installed_colormaps_cmaps
                 xcb_list_installed_colormaps_cmaps_length
                 xcb_list_installed_colormaps_cmaps_end
                 xcb_list_installed_colormaps_reply
                 xcb_alloc_color
                 xcb_alloc_color_unchecked
                 xcb_alloc_color_reply
                 xcb_alloc_named_color_sizeof
                 xcb_alloc_named_color
                 xcb_alloc_named_color_unchecked
                 xcb_alloc_named_color_reply
                 xcb_alloc_color_cells_sizeof
                 xcb_alloc_color_cells
                 xcb_alloc_color_cells_unchecked
                 xcb_alloc_color_cells_pixels
                 xcb_alloc_color_cells_pixels_length
                 xcb_alloc_color_cells_pixels_end
                 xcb_alloc_color_cells_masks
                 xcb_alloc_color_cells_masks_length
                 xcb_alloc_color_cells_masks_end
                 xcb_alloc_color_cells_reply
                 xcb_alloc_color_planes_sizeof
                 xcb_alloc_color_planes
                 xcb_alloc_color_planes_unchecked
                 xcb_alloc_color_planes_pixels
                 xcb_alloc_color_planes_pixels_length
                 xcb_alloc_color_planes_pixels_end
                 xcb_alloc_color_planes_reply
                 xcb_free_colors_sizeof
                 xcb_free_colors_checked
                 xcb_free_colors
                 xcb_free_colors_pixels
                 xcb_free_colors_pixels_length
                 xcb_free_colors_pixels_end
                 xcb_coloritem_next
                 xcb_coloritem_end
                 xcb_store_colors_sizeof
                 xcb_store_colors_checked
                 xcb_store_colors
                 xcb_store_colors_items
                 xcb_store_colors_items_length
                 xcb_store_colors_items_iterator
                 xcb_store_named_color_sizeof
                 xcb_store_named_color_checked
                 xcb_store_named_color
                 xcb_store_named_color_name
                 xcb_store_named_color_name_length
                 xcb_store_named_color_name_end
                 xcb_rgb_next
                 xcb_rgb_end
                 xcb_query_colors_sizeof
                 xcb_query_colors_unchecked
                 xcb_query_colors_colors
                 xcb_query_colors_colors_length
                 xcb_query_colors_colors_iterator
                 xcb_query_colors_reply
                 xcb_lookup_color_sizeof
                 xcb_lookup_color
                 xcb_lookup_color_unchecked
                 xcb_lookup_color_reply
                 xcb_create_cursor_checked
                 xcb_create_cursor
                 xcb_create_glyph_cursor_checked
                 xcb_create_glyph_cursor
                 xcb_free_cursor_checked
                 xcb_free_cursor
                 xcb_recolor_cursor_checked
                 xcb_recolor_cursor
                 xcb_query_best_size
                 xcb_query_best_size_unchecked
                 xcb_query_best_size_reply
                 xcb_query_extension_sizeof
                 xcb_query_extension
                 xcb_query_extension_unchecked
                 xcb_query_extension_reply
                 xcb_list_extensions_sizeof
                 xcb_list_extensions
                 xcb_list_extensions_unchecked
                 xcb_list_extensions_names_length
                 xcb_list_extensions_names_iterator
                 xcb_list_extensions_reply
                 xcb_change_keyboard_mapping_sizeof
                 xcb_change_keyboard_mapping_checked
                 xcb_change_keyboard_mapping
                 xcb_change_keyboard_mapping_keysyms
                 xcb_change_keyboard_mapping_keysyms_length
                 xcb_change_keyboard_mapping_keysyms_end
                 xcb_get_keyboard_mapping_sizeof
                 xcb_get_keyboard_mapping
                 xcb_get_keyboard_mapping_unchecked
                 xcb_get_keyboard_mapping_keysyms
                 xcb_get_keyboard_mapping_keysyms_length
                 xcb_get_keyboard_mapping_keysyms_end
                 xcb_get_keyboard_mapping_reply
                 xcb_change_keyboard_control_value_list_serialize
                 xcb_change_keyboard_control_value_list_unpack
                 xcb_change_keyboard_control_value_list_sizeof
                 xcb_change_keyboard_control_sizeof
                 xcb_change_keyboard_control_checked
                 xcb_change_keyboard_control
                 xcb_change_keyboard_control_aux_checked
                 xcb_change_keyboard_control_aux
                 xcb_get_keyboard_control
                 xcb_get_keyboard_control_unchecked
                 xcb_get_keyboard_control_reply
                 xcb_bell_checked
                 xcb_bell
                 xcb_change_pointer_control_checked
                 xcb_change_pointer_control
                 xcb_get_pointer_control
                 xcb_get_pointer_control_unchecked
                 xcb_get_pointer_control_reply
                 xcb_set_screen_saver_checked
                 xcb_set_screen_saver
                 xcb_get_screen_saver
                 xcb_get_screen_saver_unchecked
                 xcb_get_screen_saver_reply
                 xcb_change_hosts_sizeof
                 xcb_change_hosts_checked
                 xcb_change_hosts
                 xcb_change_hosts_address
                 xcb_change_hosts_address_length
                 xcb_change_hosts_address_end
                 xcb_host_sizeof
                 xcb_host_address
                 xcb_host_address_length
                 xcb_host_address_end
                 xcb_host_next
                 xcb_host_end
                 xcb_list_hosts_sizeof
                 xcb_list_hosts
                 xcb_list_hosts_unchecked
                 xcb_list_hosts_hosts_length
                 xcb_list_hosts_hosts_iterator
                 xcb_list_hosts_reply
                 xcb_set_access_control_checked
                 xcb_set_access_control
                 xcb_set_close_down_mode_checked
                 xcb_set_close_down_mode
                 xcb_kill_client_checked
                 xcb_kill_client
                 xcb_rotate_properties_sizeof
                 xcb_rotate_properties_checked
                 xcb_rotate_properties
                 xcb_rotate_properties_atoms
                 xcb_rotate_properties_atoms_length
                 xcb_rotate_properties_atoms_end
                 xcb_force_screen_saver_checked
                 xcb_force_screen_saver
                 xcb_set_pointer_mapping_sizeof
                 xcb_set_pointer_mapping
                 xcb_set_pointer_mapping_unchecked
                 xcb_set_pointer_mapping_reply
                 xcb_get_pointer_mapping_sizeof
                 xcb_get_pointer_mapping
                 xcb_get_pointer_mapping_unchecked
                 xcb_get_pointer_mapping_map
                 xcb_get_pointer_mapping_map_length
                 xcb_get_pointer_mapping_map_end
                 xcb_get_pointer_mapping_reply
                 xcb_set_pointer_mapping_sizeof
                 xcb_set_pointer_mapping
                 xcb_set_pointer_mapping_unchecked
                 xcb_set_pointer_mapping_reply
                 xcb_get_modifier_mapping_sizeof
                 xcb_get_modifier_mapping
                 xcb_get_modifier_mapping_unchecked
                 xcb_get_modifier_mapping_keycodes
                 xcb_get_modifier_mapping_keycodes_length
                 xcb_get_modifier_mapping_keycodes_end
                 xcb_no_operation_checked
                 xcb_no_operation]}
       )}.read("xcb").eval(env).print;

    version (linux)
    q{(do
        (print "HELLO LINUX")
        (def scr (SInt32))
        (def conn (xcb_connect nil &scr))
        (def setup (xcb_get_setup conn))
        (let [iter (xcb_setup_roots_iterator setup)]
         (def screen (.data iter))
         (print iter))

        (def wnd (xcb_generate_id conn))
        (xcb_create_window conn XCB_COPY_FROM_PARENT wnd (.root screen)
         0 0 640 480 0 XCB_WINDOW_CLASS_INPUT_OUTPUT (.root_visual screen) 0u nil)
        (xcb_map_window conn wnd)
        (xcb_flush conn)
          ;; TODO loop, when-not, bit-and, set!
        #_
        (loop (let [e (xcb_wait_for_event conn)]
               (when-not e (break))
               (print (bit-and (.response_type e) 0x80)
                )))
        (print "BYE")
        nil
       )}.read("linux app").eval(env).print;

    version (none)
    version (OSX) {} else
    q{(ffi C
       ;;{:flags   ["-DVK_USE_PLATFORM_MACOS_MVK"]}
       {:flags ["-ID:\\Development\\VulkanSDK\\1.1.108.0\\Include"
                "-DWIN32_MEAN_AND_LEAN"]
        :wrap true}
       {:lib     #?(:linux "libvulkan.so"
                    :windows "vulkan-1.dll")
        :include ["vulkan/vulkan.h"]
        :import  [VK_HEADER_VERSION VK_NULL_HANDLE VK_API_VERSION_1_0 VK_API_VERSION_1_1
                  VkFlags VkBool32 VkDeviceSize VkSampleMask
                  VkInstance VkPhysicalDevice VkDevice VkQueue VkSemaphore
                  VkCommandBuffer VkFence VkDeviceMemory VkBuffer VkImage
                  VkEvent VkQueryPool VkBufferView VkShaderModule
                  VkPipelineCache VkPipelineLayout VkRenderPass VkPipeline
                  VkDescriptorSetLayout VkSampler VkDescriptorPool
                  VkDescriptorSet VkFramebuffer VkCommandPool
                  VkPipelineCacheHeaderVersion VkResult VkStructureType
                  VkSystemAllocationScope VkInternalAllocationType
                  VkFormat VkImageType VkImageTiling VkPhysicalDeviceType
                  VkQueryType VkSharingMode VkImageViewType VkComponentSwizzle
                  VkVertexInputRate VkPrimitiveTopology VkPolygonMode
                  VkFrontFace VkCompareOp VkStencilOp VkLogicOp VkBlendFactor
                  VkBlendOp VkDynamicState VkFilter VkSamplerMipmapMode
                  VkSamplerAddressMode VkBorderColor VkDescriptorType
                  VkAttachmentLoadOp VkAttachmentStoreOp VkPipelineBindPoint
                  VkCommandBufferLevel VkIndexType VkSubpassContents
                  VkObjectType VkVendorId VkInstanceCreateFlags
                  VkFormatFeatureFlagBits VkFormatFeatureFlags
                  VkImageUsageFlagBits VkImageUsageFlags VkImageCreateFlagBits
                  VkImageCreateFlags VkSampleCountFlagBits VkSampleCountFlags
                  VkQueueFlagBits VkQueueFlags VkMemoryPropertyFlagBits
                  VkMemoryPropertyFlags VkMemoryHeapFlagBits VkMemoryHeapFlags
                  VkDeviceCreateFlags VkDeviceQueueCreateFlagBits
                  VkDeviceQueueCreateFlags VkPipelineStageFlagBits
                  VkPipelineStageFlagBits VkPipelineStageFlags VkMemoryMapFlags
                  VkImageAspectFlagBits VkImageAspectFlags
                  VkSparseImageFormatFlagBits VkSparseImageFormatFlags
                  VkSparseMemoryBindFlagBits VkSparseMemoryBindFlags
                  VkFenceCreateFlagBits VkFenceCreateFlags
                  VkSemaphoreCreateFlags VkEventCreateFlags
                  VkQueryPoolCreateFlags VkQueryPipelineStatisticFlagBits
                  VkQueryPipelineStatisticFlags VkQueryResultFlagBits
                  VkQueryResultFlags VkBufferCreateFlagBits
                  VkBufferCreateFlags VkBufferUsageFlagBits VkBufferUsageFlags
                  VkBufferViewCreateFlags VkImageViewCreateFlags
                  VkPipelineCacheCreateFlags VkPipelineCreateFlagBits
                  VkPipelineCreateFlags VkPipelineShaderStageCreateFlags
                  VkShaderStageFlagBits VkPipelineVertexInputStateCreateFlags
                  VkPipelineInputAssemblyStateCreateFlags
                  VkPipelineTessellationStateCreateFlags
                  VkPipelineViewportStateCreateFlags
                  VkPipelineRasterizationStateCreateFlags
                  VkCullModeFlagBits VkCullModeFlags
                  VkPipelineMultisampleStateCreateFlags
                  VkPipelineDepthStencilStateCreateFlags
                  VkPipelineColorBlendStateCreateFlags
                  VkColorComponentFlagBits VkColorComponentFlags
                  VkPipelineDynamicStateCreateFlags VkPipelineLayoutCreateFlags
                  VkShaderStageFlags VkSamplerCreateFlags
                  VkDescriptorSetLayoutCreateFlagBits
                  VkDescriptorSetLayoutCreateFlags
                  VkDescriptorPoolCreateFlagBits VkDescriptorPoolCreateFlags
                  VkDescriptorPoolResetFlags VkFramebufferCreateFlags
                  VkRenderPassCreateFlags VkAttachmentDescriptionFlagBits
                  VkAttachmentDescriptionFlags VkSubpassDescriptionFlagBits
                  VkSubpassDescriptionFlags VkAccessFlagBits VkAccessFlags
                  VkDependencyFlagBits VkDependencyFlags
                  VkCommandPoolCreateFlagBits VkCommandPoolCreateFlags
                  VkCommandPoolResetFlagBits VkCommandPoolResetFlags
                  VkCommandBufferUsageFlagBits VkCommandBufferUsageFlags
                  VkQueryControlFlagBits VkQueryControlFlags
                  VkCommandBufferResetFlagBits VkCommandBufferResetFlags
                  VkStencilFaceFlagBits VkStencilFaceFlags VkApplicationInfo
                  VkInstanceCreateInfo PFN_vkAllocationFunction
                  PFN_vkReallocationFunction PFN_vkFreeFunction
                  PFN_vkInternalAllocationNotification
                  PFN_vkInternalFreeNotification VkAllocationCallbacks
                  VkPhysicalDeviceFeatures VkFormatProperties VkExtent3D
                  VkImageFormatProperties VkPhysicalDeviceLimits
                  VkPhysicalDeviceSparseProperties VkPhysicalDeviceProperties
                  VkQueueFamilyProperties VkMemoryType VkMemoryHeap
                  VkPhysicalDeviceMemoryProperties PFN_vkVoidFunction
                  VkDeviceQueueCreateInfo VkDeviceCreateInfo
                  VkExtensionProperties VkLayerProperties VkSubmitInfo
                  VkMemoryAllocateInfo VkMappedMemoryRange VkMemoryRequirements
                  VkSparseImageFormatProperties VkSparseImageMemoryRequirements
                  VkSparseMemoryBind VkSparseBufferMemoryBindInfo
                  VkSparseImageOpaqueMemoryBindInfo VkImageSubresource VkOffset3D
                  VkSparseImageMemoryBind VkSparseImageMemoryBindInfo
                  VkBindSparseInfo VkFenceCreateInfo VkSemaphoreCreateInfo
                  VkEventCreateInfo VkQueryPoolCreateInfo VkBufferCreateInfo
                  VkBufferViewCreateInfo VkImageCreateInfo VkSubresourceLayout
                  VkComponentMapping VkImageSubresourceRange
                  VkImageViewCreateInfo VkShaderModuleCreateInfo
                  VkPipelineCacheCreateInfo VkSpecializationMapEntry
                  VkSpecializationInfo VkPipelineShaderStageCreateInfo
                  VkVertexInputBindingDescription
                  VkVertexInputAttributeDescription
                  VkPipelineVertexInputStateCreateInfo
                  VkPipelineInputAssemblyStateCreateInfo
                  VkPipelineTessellationStateCreateInfo VkViewport VkOffset2D
                  VkRect2D VkPipelineViewportStateCreateInfo
                  VkPipelineRasterizationStateCreateInfo
                  VkPipelineMultisampleStateCreateInfo VkStencilOpState
                  VkPipelineDepthStencilStateCreateInfo
                  VkPipelineColorBlendAttachmentState
                  VkPipelineColorBlendStateCreateInfo
                  VkPipelineDynamicStateCreateInfo VkGraphicsPipelineCreateInfo
                  VkComputePipelineCreateInfo VkPushConstantRange
                  VkPipelineLayoutCreateInfo VkSamplerCreateInfo
                  VkDescriptorSetLayoutBinding VkDescriptorSetLayoutCreateInfo
                  VkDescriptorPoolSize VkDescriptorPoolCreateInfo
                  VkDescriptorSetAllocateInfo VkDescriptorImageInfo
                  VkDescriptorBufferInfo VkWriteDescriptorSet VkCopyDescriptorSet
                  VkFramebufferCreateInfo VkAttachmentDescription
                  VkAttachmentReference VkSubpassDescription VkSubpassDependency
                  VkRenderPassCreateInfo VkCommandPoolCreateInfo
                  VkCommandBufferAllocateInfo VkCommandBufferInheritanceInfo
                  VkCommandBufferBeginInfo VkBufferCopy VkImageSubresourceLayers
                  VkImageCopy VkImageBlit VkBufferImageCopy VkClearColorValue
                  VkClearDepthStencilValue VkClearValue VkClearAttachment
                  VkClearRect VkImageResolve VkMemoryBarrier
                  VkBufferMemoryBarrier VkImageMemoryBarrier
                  VkRenderPassBeginInfo VkDispatchIndirectCommand
                  VkDrawIndexedIndirectCommand VkDrawIndirectCommand
                  VkBaseOutStructure VkBaseInStructure
                  vkCreateInstance vkEnumeratePhysicalDevices
                  vkGetPhysicalDeviceFeatures vkGetPhysicalDeviceFormatProperties
                  vkGetPhysicalDeviceImageFormatProperties
                  vkGetPhysicalDeviceProperties
                  vkGetPhysicalDeviceQueueFamilyProperties
                  vkGetPhysicalDeviceMemoryProperties vkGetInstanceProcAddr
                  vkGetDeviceProcAddr vkCreateDevice vkDestroyDevice
                  vkEnumerateInstanceExtensionProperties
                  vkEnumerateDeviceExtensionProperties
                  vkEnumerateInstanceLayerProperties
                  vkEnumerateDeviceLayerProperties
                  vkGetDeviceQueue vkQueueSubmit vkQueueWaitIdle vkDeviceWaitIdle
                  vkAllocateMemory vkFreeMemory vkMapMemory vkUnmapMemory
                  vkFlushMappedMemoryRanges vkInvalidateMappedMemoryRanges
                  vkGetDeviceMemoryCommitment vkGetDeviceMemoryCommitment
                  vkBindBufferMemory vkBindImageMemory
                  vkGetBufferMemoryRequirements vkGetImageMemoryRequirements
                  vkGetImageSparseMemoryRequirements
                  vkGetPhysicalDeviceSparseImageFormatProperties
                  vkQueueBindSparse vkCreateFence vkDestroyFence vkResetFences
                  vkGetFenceStatus vkWaitForFences vkCreateSemaphore
                  vkDestroySemaphore vkCreateEvent vkDestroyEvent vkGetEventStatus
                  vkSetEvent vkResetEvent vkCreateQueryPool vkDestroyQueryPool
                  vkGetQueryPoolResults vkCreateBuffer vkDestroyBuffer
                  vkCreateBufferView vkDestroyBufferView vkCreateImage
                  vkDestroyImage vkGetImageSubresourceLayout vkCreateImageView
                  vkDestroyImageView vkCreateShaderModule vkDestroyShaderModule
                  vkCreatePipelineCache vkDestroyPipelineCache
                  vkGetPipelineCacheData vkMergePipelineCaches
                  vkCreateGraphicsPipelines vkCreateComputePipelines
                  vkDestroyPipeline vkCreatePipelineLayout
                  vkDestroyPipelineLayout vkCreateSampler vkDestroySampler
                  vkCreateDescriptorSetLayout vkDestroyDescriptorSetLayout
                  vkCreateDescriptorPool vkDestroyDescriptorPool
                  vkResetDescriptorPool vkAllocateDescriptorSets
                  vkFreeDescriptorSets vkUpdateDescriptorSets vkCreateFramebuffer
                  vkDestroyFramebuffer vkCreateRenderPass vkDestroyRenderPass
                  vkGetRenderAreaGranularity vkCreateCommandPool
                  vkDestroyCommandPool vkResetCommandPool
                  vkAllocateCommandBuffers vkFreeCommandBuffers
                  vkBeginCommandBuffer vkEndCommandBuffer vkResetCommandBuffer
                  vkCmdBindPipeline vkCmdSetViewport vkCmdSetScissor
                  vkCmdSetLineWidth vkCmdSetDepthBias vkCmdSetBlendConstants
                  vkCmdSetDepthBounds vkCmdSetStencilCompareMask
                  vkCmdSetStencilWriteMask vkCmdSetStencilWriteMask
                  vkCmdSetStencilReference vkCmdBindDescriptorSets
                  vkCmdBindIndexBuffer vkCmdBindVertexBuffers vkCmdDraw
                  vkCmdDrawIndexed vkCmdDrawIndirect vkCmdDrawIndexedIndirect
                  vkCmdDispatch vkCmdDispatchIndirect vkCmdCopyBuffer
                  vkCmdCopyImage vkCmdBlitImage vkCmdCopyBufferToImage
                  vkCmdCopyImageToBuffer vkCmdUpdateBuffer vkCmdFillBuffer
                  vkCmdClearColorImage vkCmdClearDepthStencilImage
                  vkCmdClearAttachments vkCmdResolveImage vkCmdSetEvent
                  vkCmdResetEvent vkCmdWaitEvents vkCmdPipelineBarrier
                  vkCmdBeginQuery vkCmdEndQuery vkCmdResetQueryPool
                  vkCmdWriteTimestamp vkCmdCopyQueryPoolResults
                  vkCmdPushConstants vkCmdBeginRenderPass vkCmdNextSubpass
                  vkCmdEndRenderPass vkCmdExecuteCommands]}
       )}.read("vulkan").eval(env);

    version(none)
    version(Windows)
    q{(do
       (def vk-app-info (VkApplicationInfo VK_STRUCTURE_TYPE_APPLICATION_INFO
                         nil "Hello World" 1u "Vile" 1u 4194304u)) ;; TODO vk api version
       (def vk-inst-info (VkInstanceCreateInfo VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
                          nil 0u &vk-app-info 0u nil 0u nil))
       (def vk-inst (VkInstance))
       (vkCreateInstance &vk-inst-info nil &vk-inst)
       )}.read("vulkan app").eval(env);

    version(none) // TODO lib, mangles -> load -> impls -> jit call
    q{(ffi C++
       {:flags ["-I."]}
       {:lib "lib/imgui/ImGUI/x64/Debug/ImGUI.dll"
        :include ["lib/imgui/imgui.h"]
        :import [ImDrawChannel ImDrawCmd ImDrawData ImDrawList ImDrawListSharedData
                 ImDrawListSplitter ImDrawVert ImFont ImFontAtlas ImFontConfig
                 ImFontGlyph ImFontGlyphRangesBuilder ImColor ImGuiContext ImGuiIO
                 ImGuiInputTextCallbackData ImGuiListClipper ImGuiOnceUponAFrame
                 ImGuiPayload ImGuiSizeCallbackData ImGuiStorage ImGuiStyle
                 ImGuiTextBuffer ImGuiTextFilter ImTextureID ImGuiID ImWchar
                 ImGuiCol ImGuiCol_
                 ImGuiCond ImGuiCond_
                 ImGuiDataType ImGuiDataType_
                 ImGuiDir ImGuiDir_
                 ImGuiKey ImGuiKey_
                 ImGuiNavInput ImGuiNavInput_
                 ImGuiMouseCursor ImGuiMouseCursor_
                 ImGuiStyleVar ImGuiStyleVar_
                 ImDrawCornerFlags
                 ImDrawListFlags
                 ImFontAtlasFlags
                 ImGuiBackendFlags
                 ImGuiColorEditFlags
                 ImGuiColumnsFlags
                 ImGuiConfigFlags
                 ImGuiComboFlags
                 ImGuiDragDropFlags
                 ImGuiFocusedFlags
                 ImGuiHoveredFlags
                 ImGuiInputTextFlags
                 ImGuiSelectableFlags
                 ImGuiTabBarFlags
                 ImGuiTabItemFlags
                 ImGuiTreeNodeFlags
                 ImGuiWindowFlags
                 ImGuiInputTextCallback
                 ImGuiSizeCallback
                 ImS8 ImU8
                 ImS16 ImU16
                 ImS32 ImU32
                 ImS64 ImU64
                 ImVec2 ImVec4
                 ImGui
                 ImDrawCallback ImDrawIdx]}
       )}.read("imgui").eval(env);

    version(none)
    q{(ffi C++
       {:flags ["-I./lib/glm"]
        :wrap true}
       {:include ["glm/vec3.hpp"]
        :import [glm]}
       )}.read("glm").eval(env);
  }

  void term() {
    foreach (handle; libs) close(handle);
    libs.clear();
  }

  void* open(string file) {
    assert(file.length);
    if (auto p = file in libs) return *p;
    version (Posix) {
      // writeln("LOADING ", file);
      auto h = dl.dlopen(file.toStringz, dl.RTLD_LAZY | dl.RTLD_GLOBAL);
      enforce(h, text("Failed to dlopen: ", file, "\n", dl.dlerror));
	  }
    else version (Windows) {
      auto h = win32.LoadLibraryA(file.toStringz);
      enforce(h, win32Error);
	  }
    else static assert(0);
    return libs[file] = h;
  }

  void close(string file) {
    if (auto p = file in libs) {
      close(*p);
      libs.remove(file);
    }
  }
  static void close(void* handle) {
    version (Posix) enforce(dl.dlclose(handle) == 0, dl.dlerror.to!string);
    else version (Windows) enforce(win32.FreeLibrary(handle), win32Error);
    else static assert(0);
  }

  void* load(void* handle, in char* name) {
    assert(handle);
    version (Posix) auto s = dl.dlsym(handle, name);
    else version (Windows) auto s = win32.GetProcAddress(handle, name);
    else static assert(0);
    // enforce(s, text("Failed to load symbol: ", name));
    if (!s) writeln(text("Missing symbol: ", name));
    return s;
  }

  version(none)
  size_t fnNumParams(FnType* fn, bool objC, Struct* parent = null) {
    auto extra = (parent ? 1 : 0) + fn.structRetFix;
    version (OSX) return fn.args.length + (objC ? 2 : 0) + extra;
    else return fn.args.length + extra;
  }

  /// Creates a FFI closure for the given fn, calling into fun, returns the imp.
  void* closure(.Fn* fn, typeof(ffi_closure.fun) fun, bool objC = false) {
    assert(fn);
    assert(fun);
    void* ptr; // TODO struct params
    auto size = Lambda.sizeof + size_t.sizeof * fn.fnType.totalArgs; //fnNumParams(fn.fnType, objC);
    auto c = cast(Lambda*)ffi_closure_alloc(size, &ptr);
    enforce(c, "Failed to create FFI closure");
    scope (failure) ffi_closure_free(c);
    Analyzer.doCompile(fn.fnType, &c.fn, objC);
    auto rc = ffi_prep_closure_loc(&c.closure, &c.fn.cif, fun, fn, &ptr);
    enforce(rc == FFI_OK);
    return ptr;
  }

  struct Lambda {
    alias closure this;
    ffi_closure closure;
    Fn          fn;
  }

  struct Fn {
    @disable this();

    ffi_cif      cif;
    ffi_type*[0] types;

    // TODO: merge with call
    version (OSX)
    Any callObjC(ref Env env, .Fn* fn, id obj, List* args) {
      assert(fn);
      assert(fn.type);
      assert(fn.isFFI);
      assert(fn.ffi);
      assert(fn.external.sel);
      assert(obj);
      auto fnt = fn.fnType;
      auto num = fn.params.length;
      enforce((num == 0 && !args) || (args && num == args.length));
      num += 2;

      auto paramSize = size_t.sizeof * num;
      auto params = cast(void**)alloca(paramSize + fnt.argsSize + ffi_arg.sizeof * 2);
      auto values = cast(void*)params + paramSize;
      *cast(void**)values = obj;
      params[0] = values;
      values += size_t.sizeof;
      *cast(void**)values = fn.external.sel;
      params[1] = values;
      values += size_t.sizeof;
      for (auto n = 2; n < num; n++) {
        // writeln("PARAM ", fn.params[n-2].name);
        params[n] = values;
        auto t = fnt.args[n-2];
        fromAny(args.data.eval(env), values, t);
        values += max(ffi_arg.sizeof, t.size);
        args = args.next;
      }

      return call(fnt, fn.ffi, params);
    }

    Any callThis(ref Env env, .Fn* fn, void* fnp, void* self, List* args) {
      assert(fn);
      assert(fn.type);
      assert(fn.isMember);
      assert(fn.isFFI);
      assert(fnp);
      assert(self);
      auto fnt = fn.fnType;
      auto num = fnt.totalArgs;
      //enforce((num == 0 && !args) || (args && num == args.length));
      //num++;

      auto paramSize = size_t.sizeof * num;
      auto params = cast(void**)alloca(paramSize + fnt.argsSize);
      auto values = cast(void*)params + paramSize;
      *cast(void**)values = self;
      params[0] = values;
      values += size_t.sizeof;
      if (fnt.structRetFix) num--;
      for (auto n = 1; n < num; n++) {
        params[n] = values;
        auto t = fnt.args[n - 1];
        fromAny(args.data.eval(env), values, t);
        values += max(ffi_arg.sizeof, t.size);
        args = args.next;
      }
      return call(fnt, fnp, params, values);
    }

    Any call(ref Env env, .Fn* fn, List* args, void* BUG = null) {
      assert(fn);
      assert(fn.type);
      assert(fn.isFFI);
      assert(fn.ffi);
      auto fnt = fn.fnType;
      auto num = fnt.totalArgs;
      //enforce((num == 0 && !args) || (args && num == args.length)); // TODO variadic

      auto paramSize = size_t.sizeof * num;
      auto params = cast(void**)alloca(paramSize + fnt.argsSize);
      auto values = cast(void*)params + paramSize;
      if (fnt.structRetFix) num--;
      foreach (n; 0..num) {
        params[n] = values;
        auto t = fnt.args[n];
        fromAny(args.data.eval(env), values, t);
        values += max(ffi_arg.sizeof, t.size);
        args = args.next;
      }

      return call(fnt, fn.ffi, params, values);
    }

    Any call(FnType* fnt, void* fnp, void** params, void* values) {
      auto retType = fnt.ret.aliased;
      if (retType.native == Type.Native.Struct) {
        auto s = cast(Struct*)retType;
        auto val = cast(Val*)new ubyte[16 + retType.size].ptr;
        auto arg = cast(ffi_arg*)(cast(void*)val + 16);
        val.type = fnt.ret;
        if (fnt.structRetFix) {
          params[fnt.totalArgs-1] = values;
          *cast(void**)values = arg;
          arg = null;
        }
        ffi_call(&cif, fnp, arg, params);
        return Any(val);
      }
      else {
        ffi_arg retval = void;
        ffi_call(&cif, fnp, &retval, params);
        return toAny(retval, fnt.ret);
      }
    }

    static Any toAny(ffi_arg v, Type* expected) {
      // writeln("TOANY ", v & 0xFFFF, " ", expected.sym.name);
      auto t = expected.aliased;
      final switch (t.native) with (Type.Native) {
      case Void:     return nil;
      case Bool:     return v ? boolTrue : boolFalse;
      case SInt8:    v &= 0xFF; goto case SInt64;
      case SInt16:   v &= 0xFFFF; goto case SInt64;
      case SInt32:   v &= 0xFFFFFFFF; goto case SInt64;
      case SInt64:   return .Any(cast(long)v);
      case UInt8:    v &= 0xFF; goto case UInt64;
      case UInt16:   v &= 0xFFFF; goto case UInt64;
      case UInt32:   v &= 0xFFFFFFFF; goto case UInt64;
      case UInt64:   return .Any(cast(ulong)v);
      case Float16:  assert(0); // TODO
      case Float32:  return .Any(*cast(float*)&v);
      case Float64:  return .Any(&new Prim(*cast(double*)&v).val);
      case Float80:  assert(0); // TODO
      case Char:     return .Any(cast(dchar)v);
      case Enum:     return .Any(cast(long)v); // TODO enum type
      case Pointer:
        auto p = cast(void*)v;
        if (!p) return nil;
        t = t.next.aliased;
        switch (t.native) {
        case Void:     return .Any(p);
        case NSObject: version (OSX) return toAny(v, t);
        default:
        }
        return .Any(&new Prim(expected, p).val);
      case NSObject:
        version (OSX) {
          if (!v) return nil;
          return .Any(cast(ulong)v, .Any.Tag.ObjC);
        }
      case Type:
      case Meta:
      case Alias:
      case Any:
      case Nil:
      case BigInt:
      case Array:
      case DynArray:
      case Struct:
      case Trait:
      case Fn:
      case DObject:
      case Generic:
      case Temp:     assert(0, t.native.to!string);
      }
    }

    // TODO rewrite
    static void* toParam(Any v, Type* expected) {
      with (Type.Native) {
        while (expected.native == Alias || expected.native == Enum) {
          expected = expected.next;
        }
      }
      auto t = expected.native;
      final switch (v.tag) with (Any.Tag) with (Type) {
      case Bool:
        enforce(t >= Native.SInt8 && t <= Native.UInt64);
        return cast(void*)v.isTrue;
      case Char:
        enforce(t == Native.Char);
        return cast(void*)v.character;
      case SInt:
        if (t >= Native.UInt8 && t <= Native.UInt64) {
          return cast(void*)v.uinteger; // TODO sign
        }
        enforce(t >= Native.SInt8 && t <= Native.SInt64,
                format("Want %s but got %s", t, v.tag));
        return cast(void*)v.sinteger;
      case UInt:
        enforce(t >= Native.UInt8 && t <= Native.UInt64,
                format("Want %s but got %s", t, v.tag));
        return cast(void*)v.uinteger;
      case Float:
        enforce(t >= Native.Float32 && t <= Native.Float80);
        return cast(void*)v.floating;
      case Nil:
        enforce(t == Native.Pointer);
        return null;
      case Ptr:
        enforce(t == Native.Pointer && expected.next is typeVoid);
        return v.ptr;
      case Val:
        auto val = v.val;
        switch (t) {
        case Native.Fn:
          auto fnt = val.type.aliased;
          auto fn = cast(.Fn*)val;
          enforce(expected is fnt);
          enforce(fn.isFFI || fn.isClosure);
          enforce(fn.ffi);
          return fn.ffi;
        case Native.Pointer:
          enforce(expected.next);
          auto a = expected.next.aliased;
          if (val.type is typeMeta) {
            return toParam((cast(.Meta*)val).data, expected);
          }
          else if (val.type is typeStr) {
            enforce(a is typeUInt8);
            return cast(void*)(cast(Str*)val).data.toStringz;
          }
          else if (val.type.next && a is val.type.aliased.next.aliased) {
            return (cast(Prim*)val).ptr;
          }
          else {
            enforce(0, expected.toString);
            assert(0);
          }
        default:
          enforce(0, t.to!string);
          assert(0);
        }
      case ObjC:
        // enforce(t == Native.Pointer);
        // enforce(expected.next.native == Native.NSObject);
        version (OSX) return cast(void*)v.uinteger;
        else assert(0);
      // case Meta:
      //   return toParam((cast(.Meta*)v.val).data, expected);
      case Sym:
      case Key:
      case Obj:
        enforce(0, "Invalid native argument");
        assert(0);
      }
    }
  }

  enum ParseMode {
    Wrap      = 1 << 0,
    C         = 1 << 1,
    CPlusPlus = 1 << 2,
    ObjC      = 1 << 3,
    Full      = 1 << 4,
    NoCPP     = 1 << 5,
    CWrap         = Wrap | C,
    CPlusPlusWrap = Wrap | CPlusPlus
  }

  struct Opts {
    string[]  flags; /// Extra cmd-line flags to pass to clang
    string[]  files; /// Array of source files to include
    Lib   []  libs;  /// Libraries and symbols to load
    ParseMode mode;  /// Language and compiler options
  }

  struct Lib { // TODO: matching "ns"
    string file;    /// Filename of the shared library
    Sym*[] symbols; /// Array of symbols to load and intern
    void*  handle;
  }

  /// Parsed pre-processor #define macro.
  struct CMacro {
    string   name;
    string   expr;
    string[] args;
    Kind     kind;
    enum Kind {
      Alias,
      Expr,
      Flag
    }
  }

  enum CMacroOp : ubyte {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Greater,
    Less,
    GreaterEq,
    LessEq,
    Not,
    And,
    Or,
    BitNot,
    BitAnd,
    BitOr,
    BitXor,
    BitShl,
    BitShr
  }
  struct CMacroOpInfo {
    Sym* sym;
    byte precedence;
  }
  static __gshared CMacroOpInfo[CMacroOp.max+1] cMacroOps;
  // static __gshared byte[CMacroOp.max+1] cMacroOpsPrecedence;

  /// Consumes the output of `clang -E -dM`.
  CMacro[string] parseCMacros(string o) {
    enum prefix   = "#define ";
    enum shortest = "a 0".length + prefix.length; // Minimum valid line length.
    size_t         end;
    CMacro[string] map;
    foreach (line; o.splitLines) {
      enforce(line.length >= shortest, line);
      enforce(line[0..prefix.length] == prefix);
      if (line[prefix.length] == '_' && line[prefix.length + 1] == '_')
        continue;

      string[]    params;
      CMacro.Kind kind;
      auto idx = line.indexOf(' ', prefix.length);
      enforce(idx > prefix.length, line);

      if (line[idx - 1] == ')') {
        kind = CMacro.Kind.Expr;
        end  = line.indexOf('(', prefix.length);
        enforce(end > prefix.length && end < idx);
        auto i = idx - 1;
        if (end != i) params = line[end + 1..i].split(',');
      }
      else if (idx + 1 == line.length) {
        end  = idx;
        kind = CMacro.Kind.Flag;
      }
      else {
        end = idx;

        auto ch = line[idx + 1];
        switch (ch) with (CMacro.Kind) {
        case '-':
        case '0':..
        case '9':
        case '"':
        case '(': kind = Expr;  break;
        default:  kind = line[idx + 1..$].indexOf('(') == -1 ? Alias : Expr;
        }
      }

      auto name = line[prefix.length..end];
      map[name] = CMacro(name, line[idx + 1..$], params, kind);
    }
    return map;
  }

  static __gshared {
    string[] includePath;
    string[] libPath;

    version (OSX) string[] frameworkPath;
  }

  version (OSX)
  static void setupLibPath() {
    auto rt = execute(["ld", "-v", "--help"]);
    enforce(rt.status == 0, rt.output);
    Appender!(string[]) path;
    auto skip = true;
    foreach (line; rt.output.splitLines) {
      if (skip) {
        if (line.startsWith("Library")) skip = false;
        continue;
      }
      if (line.startsWith("Framework")) {
        // TODO here
        break;
      }
      path.put(line[1..$].idup);
    }
    enforce(path.data.length, "Empty lib path");
    libPath = path.data;
  }

  else version (linux)
  static void setupLibPath() {
    auto rt = execute(["clang", "-m64", "-Xlinker", "--verbose"]);
    auto re = regex(`SEARCH_DIR\("=([^"]+)"\)`);
    enforce(rt.status == 1, rt.output);
    Appender!(string[]) path;
    foreach (m; rt.output.matchAll(re)) path.put(m[1]);
    enforce(path.data.length, "Empty lib path");
    libPath = path.data;
  }

  else version (Windows)
  static void setupLibPath() {

  }

  void setupIncludePath() {
    setupLibPath();
    auto proc = pipeProcess(["clang", "-v", "-E", "-x", "c", "-"]);
    proc.stdin.close();
    auto rc = proc.pid.wait();
    if (rc != 0) {
      foreach (line; proc.stderr.byLine) stderr.writeln(line);
      enforce(0, "clang failed with code " ~ rc.to!string);
    }
    version(Windows) immutable eol = 1;
    else immutable eol = 0;
    Appender!(string[]) path;
    auto skip = true;
    version (OSX) typeof(path) framework;
    foreach (line; proc.stderr.byLine) {
      if (skip) {
        if (line.startsWith("#include <...>")) skip = false;
        continue;
      }
      if (line.startsWith("End")) break;
      enum frameworkSuffix = " (framework directory)";
      if (line.endsWith(frameworkSuffix)) {
        version (OSX) framework.put(line[1..$].idup);
        else assert(0);
      }
      else path.put(("-I" ~ line[1..$-eol]).assumeUnique);
    }
    enforce(path.data.length, "Empty include path");
    includePath = path.data;
    version (OSX) frameworkPath = framework.data;
  }

  void addIncludePath(ref Appender!(string[]) a) {
    foreach (p; includePath) a.put(p);
  }

  Any include(Opts* opts) {
    if (opts.files.empty) return nil;
    if (includePath.empty) setupIncludePath();

    Analyzer ctx;
    scope auto perfInclude = Perf.Scoped("include");

    // Generate a wrapper source file.
    string fileName;
    auto wrap = opts.files.length > 1 || opts.mode & ParseMode.Wrap;
    if (wrap || opts.mode != ParseMode.C && opts.files[0].endsWith(".h")) {
      scope auto prefWrapper = Perf.Scoped("wrapper");
      Appender!string s;
      foreach (f; opts.files) {
        s.put("#include <");
        s.put(f);
        s.put(">\n");
      }

      version(Windows) {
        auto root = rt.tempPath;
      }
      else {
        immutable root = "/tmp/";
      }

      fileName = root ~ "vile_include." ~
        (opts.mode & ParseMode.ObjC ? "m" :
         opts.mode & ParseMode.C    ? "c" : "cpp");

      write(fileName, s.data);
    }
    else {
      fileName = opts.files[0];
    }

    // Run the C Pre-Processor and extract #define directives
    if (!(opts.mode & ParseMode.NoCPP)) {
      scope auto perfCpp = Perf.Scoped("cpp");
      auto args = appender(["clang", "-E", "-dM"]); // TODO add -H
      addIncludePath(args);
      args.put(opts.flags);
      args.put("-Wno-everything");
      args.put("-c");
      args.put(fileName);
      // writeln("CPP FLAGS ", args.data);
      auto ret = execute(args.data);
      enforce(ret.status == 0, ret.output[0..min(ret.output.length, 1024)]);
      ctx.macros = parseCMacros(ret.output);
    }

    // Generate the clang command line
    auto isCPP = opts.mode & ParseMode.CPlusPlus;
    auto args = appender(["clang", "-cc1", "-ast-dump", "-Werror",
                          isCPP ? "-stdlib=libc++ -std=c++14" : "-std=c11"]);
    // addIncludePath(args);
    if (opts.mode & ParseMode.ObjC) args.put("-fblocks");
    // TODO only do this once
    auto proc = pipeProcess(["clang", "-###", "-c", fileName]);
    proc.stdin.close();
    auto rc = proc.pid.wait();
    if (rc != 0) {
      foreach (line; proc.stderr.byLine) stderr.writeln(line);
      enforce(0, "clang failed with code " ~ rc.to!string);
    }
    auto skip = 1;
    foreach (line; proc.stderr.byLine) {
      if (!line.startsWith(" \"")) continue;
      for (size_t i = 1; i < line.length;) {
        auto end = line.indexOf('"', i + 1);
        enforce(end != -1);
        auto param = line[i..end];
        i = end + 3;

        if (skip > 0) {
          skip--;
          continue;
        }

        switch (param) {
        case "-cc1":
        case "-emit-obj":
          break;
        case "-main-file-name":
        case "-o":
          skip = 1;
          break;
        case "-x":
          skip = 2;
          break;
        default: args.put(param.replace("\\\\", "\\").idup);
        }
      }
      break;
    }

    version(Windows) {
      args.put("-Wno-expansion-to-defined");
      args.put("-Wno-nonportable-include-path");
    }
    else version(linux) args.put("-Wno-builtin-requires-header");
    else version(OSX) {
      args.put("-Wno-nullability-completeness");
      args.put("-Wno-expansion-to-defined");
    }
    args.put(opts.flags);
    args.put(fileName);
    // writeln("CFLAGS ", args.data);

    // Transform the source using clang, its AST is simpler to parse.
    {
      scope auto perfClang = Perf.Scoped("clang");
      auto ret = execute(args.data);
      debug write("output.txt", ret.output);
      enforce(ret.status == 0, ret.output[0..min(ret.output.length, 8182)]);
      // TODO: check for warning or error on first line?

      {
        scope auto perfCtx = Perf.Scoped("ctx");
        auto output = ret.output.splitLines;
        enforce(output.length, "empty output");
        ctx.init(output, opts.libs);
      }
    }

    // Generate FFI bindings for the requested symbols and their dependencies.
    try {
      scope auto perfAnalyze = Perf.Scoped("analyze");
      ctx.analyzeUnit(isCPP ? Linkage.CXX : Linkage.C);
    }
    catch (Exception e) {
      writeln(e);
      return nil; // TODO error
    }
    catch (Error e) {
      writeln(ctx.parser.line);
      throw e;
    }

    return nil; // TODO return NS for included file
  }

  alias Addr = ulong; /// Clang AST node address, used for analysis lookups.

  /// Linked list of AST nodes for a single declaration.
  /// They are accumulated in `links` when entering a new node.
  static struct Link {
    Def* def;  /// Matching Vile definition, set before analyzing the node.
    Addr prev; /// Previous AST node of the same definition, analyze before.
    Addr next;
    uint id;   /// Parsed line number, uint.max if already analyzed.

    this(uint id, Addr prev) {
      this.id     = id;
      this.prev   = prev;
    }
  }

  enum DeclEnum : ubyte {
    // Class,
    Struct,
    Union
    // NOTE: 1 bit in Node
  }

  enum DeclProt : ubyte {
    Public,
    Protected,
    Package,
    Private
    // NOTE 2 bits in Node
  }

  enum Linkage : ubyte {
    None,
    C,
    CXX
    // NOTE 2 bits in Node
  }

  /// Parsed line information from clang's AST dump.
  /// Indentation is always read for every line.
  /// Top level lines are always parsed.
  /// The presence of a kind indicates a parsed line.
  static struct Node {
    alias Data = string[7]; // TODO 2. Important parts are (kind, arg1, arg2)
    Data params; /// Type-specific arguments. Often <name> <value>.
    Loc  start;  /// Source range start.
    Loc  end;    /// Source range end.
    Loc  loc;    /// Source location.
    Addr addr;
    Addr prev;
    Addr base;
    mixin(bitfields!(// Node parameters
                     ubyte,    "indent",   7, // Hierarchy level
                     ubyte,    "offset",   7, // Offset of 1st arg after indent
                     Kind,     "kind",     8, // Clang type for this node
                     DeclEnum, "declEnum", 1,
                     DeclProt, "declProt", 2,
                     Linkage,  "linkage",  2,
                     // Analyzis state
                     bool, "parsed",    1, // Fully parsed
                     bool, "analyzing", 1, // Began analysis
                     bool, "analyzed",  1, // Finished analysis
                     // Packed node parameters
                     bool, "callinit",   1,
                     bool, "cinit",      1,
                     bool, "constexpr",  1,
                     bool, "definition", 1,
                     bool, "depth",      1,
                     bool, "implicit",   1,
                     bool, "index",      1,
                     bool, "inline",     1,
                     bool, "isConst",    1,
                     bool, "isDefault",  1,
                     bool, "isDelete",   1,
                     bool, "isDelete_",  1,
                     bool, "isExtern",   1,
                     bool, "isGetter",   1,
                     bool, "isPure",     1,
                     bool, "isRoot",     1,
                     bool, "isStatic",   1,
                     bool, "isSugar",    1,
                     bool, "listinit",   1,
                     bool, "modpriv",    1,
                     bool, "mutable",    1,
                     bool, "nrvo",       1,
                     bool, "referenced", 1,
                     bool, "tls",        1,
                     bool, "tlsDynamic", 1,
                     bool, "trivial",    1,
                     bool, "uneval",     1,
                     bool, "uninst",     1,
                     bool, "used",       1,
                     bool, "virtual",    1,
                     uint, "__padding",  4));

    enum Kind : ubyte {
      AccessSpecDecl,
      AlignValueAttr,
      AlignedAttr,
      AllocSizeAttr,
      AlwaysInlineAttr,
      ArcWeakrefUnavailableAttr,
      ArrayInitIndexExpr,
      ArrayInitLoopExpr,
      ArraySubscriptExpr,
      AsmLabelAttr,
      AttributedType,
      AvailabilityAttr,
      BinaryOperator,
      BlockCommandComment,
      BlockPointerType,
      BreakStmt,
      BuiltinTemplateDecl,
      BuiltinType,
      CFAuditedTransferAttr,
      CFConsumedAttr,
      CFReturnsNotRetainedAttr,
      CFReturnsRetainedAttr,
      CStyleCastExpr,
      CXXBoolLiteralExpr,
      CXXConstCastExpr,
      CXXConstructExpr,
      CXXConstructorDecl,
      CXXConversionDecl,
      CXXCtorInitializer,
      CXXDefaultArgExpr,
      CXXDependentScopeMemberExpr,
      CXXDestructorDecl,
      CXXFunctionalCastExpr,
      CXXMemberCallExpr,
      CXXMethodDecl,
      CXXNoexceptExpr,
      CXXNullPtrLiteralExpr,
      CXXOperatorCallExpr,
      CXXPseudoDestructorExpr,
      CXXRecord,
      CXXRecordDecl,
      CXXReinterpretCastExpr,
      CXXStaticCastExpr,
      CXXTemporaryObjectExpr,
      CXXThisExpr,
      CXXUnresolvedConstructExpr,
      CallExpr,
      CaseStmt,
      CharacterLiteral,
      ClassTemplateDecl,
      ClassTemplatePartialSpecialization,
      ClassTemplatePartialSpecializationDecl,
      ClassTemplateSpecialization,
      ClassTemplateSpecializationDecl,
      CompoundAssignOperator,
      CompoundLiteralExpr,
      CompoundStmt,
      ConditionalOperator,
      ConstAttr,
      ConstantArrayType,
      ConstantExpr,
      ConvertVectorExpr,
      CopyAssignment,
      CopyConstructor,
      DLLImportAttr,
      DecayedType,
      DeclRefExpr,
      DeclStmt,
      DecltypeType,
      DefaultConstructor,
      DefaultStmt,
      DefinitionData,
      DependentNameType,
      DependentScopeDeclRefExpr,
      DependentTemplateSpecializationType,
      DeprecatedAttr,
      Destructor,
      DisableTailCallsAttr,
      ElaboratedType,
      EmptyDecl,
      Enum,
      EnumConstantDecl,
      EnumDecl,
      EnumExtensibilityAttr,
      EnumType,
      ExprWithCleanups,
      ExtVectorType,
      Field,
      FieldDecl,
      FlagEnumAttr,
      FloatingLiteral,
      ForStmt,
      FormatArgAttr,
      FormatAttr,
      FriendDecl,
      FullComment,
      Function,
      FunctionDecl,
      FunctionNoProtoType,
      FunctionProtoType,
      FunctionTemplateDecl,
      GCCAsmStmt,
      GNUNullExpr,
      GotoStmt,
      HTMLEndTagComment,
      HTMLStartTagComment,
      IBActionAttr,
      IBOutletAttr,
      IfStmt,
      ImplicitCastExpr,
      ImplicitValueInitExpr,
      IncompleteArrayType,
      IndirectFieldDecl,
      InitListExpr,
      InjectedClassNameType,
      InlineCommandComment,
      IntegerLiteral,
      LValueReferenceType,
      LabelStmt,
      LinkageSpecDecl,
      MSNoVTableAttr,
      MaterializeTemporaryExpr,
      MaxFieldAlignmentAttr,
      MayAliasAttr,
      MemberExpr,
      MinVectorWidthAttr,
      ModeAttr,
      MoveAssignment,
      MoveConstructor,
      NSConsumedAttr,
      NSConsumesSelfAttr,
      NSErrorDomainAttr,
      NSReturnsRetainedAttr,
      Namespace,
      NamespaceDecl,
      NoDebugAttr,
      NoEscapeAttr,
      NoInlineAttr,
      NoThrowAttr,
      NonNullAttr,
      NonTypeTemplateParmDecl,
      NotTailCalledAttr,
      Null,
      NullStmt,
      ObjCBoolLiteralExpr,
      ObjCBoxableAttr,
      ObjCBridgeAttr,
      ObjCBridgeMutableAttr,
      ObjCBridgeRelatedAttr,
      ObjCCategoryDecl,
      ObjCDesignatedInitializerAttr,
      ObjCExceptionAttr,
      ObjCExplicitProtocolImplAttr,
      ObjCIndependentClassAttr,
      ObjCInterface,
      ObjCInterfaceDecl,
      ObjCInterfaceType,
      ObjCIvarDecl,
      ObjCMessageExpr,
      ObjCMethod,
      ObjCMethodDecl,
      ObjCNSObjectAttr,
      ObjCObjectPointerType,
      ObjCObjectType,
      ObjCPropertyDecl,
      ObjCProtocol,
      ObjCProtocolDecl,
      ObjCRequiresPropertyDefsAttr,
      ObjCRequiresSuperAttr,
      ObjCReturnsInnerPointerAttr,
      ObjCRootClassAttr,
      ObjCTypeParamDecl,
      OffsetOfExpr,
      OpaqueValueExpr,
      Overrides,
      PackExpansionExpr,
      PackedAttr,
      ParagraphComment,
      ParamCommandComment,
      ParenExpr,
      ParenListExpr,
      ParenType,
      ParmVarDecl,
      PointerType,
      PragmaCommentDecl,
      PragmaDetectMismatchDecl,
      PredefinedExpr,
      PureAttr,
      QualType,
      RValueReferenceType,
      Record,
      RecordDecl,
      RecordType,
      RestrictAttr,
      ReturnStmt,
      ReturnsTwiceAttr,
      SelectAnyAttr,
      SentinelAttr,
      ShuffleVectorExpr,
      SizeOfPackExpr,
      StaticAssertDecl,
      StmtExpr,
      StringLiteral,
      SubstNonTypeTemplateParmExpr,
      SubstTemplateTypeParmType,
      SwiftBridgedTypedefAttr,
      SwiftErrorAttr,
      SwiftNameAttr,
      SwiftNewtypeAttr,
      SwiftPrivateAttr,
      SwitchStmt,
      TargetAttr,
      TemplateArgument,
      TemplateSpecializationType,
      TemplateTemplateParmDecl,
      TemplateTypeParm,
      TemplateTypeParmDecl,
      TemplateTypeParmType,
      TextComment,
      TranslationUnitDecl,
      TypeAlias,
      TypeAliasDecl,
      TypeAliasTemplateDecl,
      TypeTraitExpr,
      TypeVisibilityAttr,
      Typedef,
      TypedefDecl,
      TypedefType,
      UnaryExprOrTypeTraitExpr,
      UnaryOperator,
      UnaryTransformType,
      UnavailableAttr,
      UnresolvedLookupExpr,
      UnresolvedMemberExpr,
      UnusedAttr,
      UsedAttr,
      UsingDecl,
      UsingShadowDecl,
      UuidAttr,
      VarDecl,
      VarTemplateDecl,
      VarTemplatePartialSpecializationDecl,
      VarTemplateSpecialization,
      VarTemplateSpecializationDecl,
      Variadic, /// "..."
      VectorType,
      VerbatimBlockComment,
      VerbatimBlockLineComment,
      VerbatimLineComment,
      VisibilityAttr,
      WarnUnusedResultAttr,
      WeakAttr,
      WeakImportAttr,
      WhileStmt,
      _public,
    }
  }

  /// Location metadata. Used in declaration ranges and for symbol positions.
  static struct Loc {
    string file;
    uint   line = uint.max; // TODO: is 0 valid? if not make it the unused value
    uint   col  = uint.max;
  }

  /// Analyzes the output of Parser and interns imports
  static struct Analyzer {
    Parser              parser;     /// Reader from text lines to generic nodes
    Appender!(uint[])   matches;    /// Array of node ids matching unresolved
    RedBlackTree!string found;      /// Symbols found, remove from `unresolved`
    RedBlackTree!string unresolved; /// Symbols to match, requested for import
    Lib[]               libs;       /// Symbols to import from dynamic libraries
    CMacro[string]      macros;     /// Preprocessor macros to match on imports
    Any[string]         macroExprs; /// Parsed preprocessor macros cache
    Addr[string][CNS.N] decls;      /// Lookup of decl names to AST addresses
    Link[Addr]          lookup;     /// Lookup of AST addresses to node chains
    CXXNS*              cxxNamespace; /// C++ current namespace list
    version (OSX) Type* objcInstance; /// Resolve "instancetype" to this type

    string [16] nameStack;
    Struct*[16] structStack;
    int structStackIdx = -1;

    Type*[string] typeLookup;
    Appender!(Type**[]) forwardTypes;

    void checkForward(ref Type* p) {
      if (p.native == Type.Native.Temp) {
        forwardTypes.put(&p);
      }
    }

    void resolveForwards() {
      foreach (p; forwardTypes.data) {
        // TODO
      }
    }

    // TODO keep named symbols across passes, lookup before create; no recreate!
    __gshared CXXNS*[string] cxxNamespaces; /// Lookup for C++ namespaces

    version (OSX) __gshared Struct*[Class] classes;

    /// Symbol namespaces used in C/C++ and Objective-C.
    enum CNS {
      Enum,     /// C enum definitions.
      Struct,   /// C struct definitions.
      Var,      /// Main variable/function/type namespace.
      Category, /// Objective-C category definitions.
      Protocol, /// Objective-C protocol definitions.
      N         /// Number of elements in `CNS`.
    }

    /// User-defined namespace used in C++.
    static struct CXXNS {
      alias ns this;
      NS     ns;
      CXXNS* parent;
      string prefix;
    }

    void init(string[] lines, Lib[] libs) {
      // TODO: no import from libs support; error on extern fn (need lib)
      assert(lines.length);
      assert(libs .length);
      this.parser.init(lines);
      this.libs   = libs;
      found       = new typeof(found)();
      unresolved  = new typeof(unresolved)();
      foreach (ref lib; libs) {
        if (!lib.file.empty) lib.handle = open(lib.file);
        foreach (s; lib.symbols)
          if (auto a = resolveAlias(s.name)) {
            unresolved.insert(a);
          }
      }
      typeLookup["unsigned short"] = typeUInt16;
      typeLookup["long long"] = typeSInt64;
      typeLookup["unsigned long long"] = typeUInt64;
    }

    Node* node(uint id) { return &parser.lines[id]; }

    Type* resolve(CNS cns, alias fn)(Addr addr) {
      Link* link;
      do {
        link = addr in lookup;
        enforce(link, format("Missing link: %s %x", cns, addr));
        // writefln("RESOLVE %s %x %s def=%x", link.id, addr, node(link.id).params[0], link.def);
        addr = link.next;
      } while (addr);
      if (link.def) {
        assert(link.def.type is typeType);
        return cast(Type*)link.def;
      }
      auto id = link.id; // Don't pass link.id by ref to fn()
      auto def = fn(id);
      assert(def is link.def);
      assert(def.type is typeType);
      return cast(Type*)def;
    }

    alias resolveType     = resolve!(CNS.Var,      analyze);
    alias resolveEnum     = resolve!(CNS.Enum,     analyzeEnum);
    alias resolveStruct   = resolve!(CNS.Struct,   analyzeData);
    alias resolveProtocol = resolve!(CNS.Protocol, analyzeObjcProtocol);

    Type* resolveNamespaced(string name, ptrdiff_t idx) {
      return typeVoid; // TODO
    }

    Type* resolveNamed(string name, CNS cns) {
      auto idx = name.indexOf("::");
      if (idx != -1) return resolveNamespaced(name, idx);
      auto sym  = Sym.intern(name);
      auto hash = sym.toHash;
      if (auto p = name in decls[cns]) { // TODO hashed
        auto def = resolveType(*p);
        enforce(def.type is typeType);
        return cast(Type*)def;
      }
      if (cns == CNS.Struct && name == "__va_list_tag") return typeAny;
      if (structStackIdx != -1) {
        for (auto i = structStackIdx; i >= 0; i--) {
          if (auto m = structStack[i].lookup(sym)) {
            if (m.kind == Struct.Member.Kind.Type) return m.type;
          }
        }
      }
      enforce(0, text(cns, " not found: ", name));
      assert(0);
    }

    /// Recursively follow a #define alias chain.
    string resolveAlias(string name) {
      if (auto p = name in macros) {
        if (p.kind != CMacro.Kind.Alias) return null;
        if (p.expr != name) name = resolveAlias(p.expr);
      }
      return name;
    }

    string nsPrefix(string prefix, string name) {
      auto path = new char[prefix.length + name.length + 2];
      auto sep  = prefix.length + name.length;
      path[0..prefix.length] = prefix;
      path[prefix.length..sep] = name;
      path[sep..$] = "::";
      return path.assumeUnique;
    }

    /// Imports symbol definitions and implementations from parsed clang output.
    void analyzeUnit(Linkage defaultLinkage) {
      parser.parseTopLevel(defaultLinkage);

      auto root = node(0);
      assert(root.parsed);
      enforce(root.kind == Node.Kind.TranslationUnitDecl, root.kind.to!string);
      root.analyzed = true;

      // Resolve the top-level definitions to import.
      int nsStackIdx;
      int nsIndent;
      string[8] nsStack = void;
      nsStack[0] = null;
      for (auto id = 1u; id < parser.lines.length; id++) {
        matchAll(id);

        auto ast = node(id);
        if (ast.linkage != Linkage.None) {
          auto inNS = nsStack[nsStackIdx];
          if (ast.kind == Node.Kind.NamespaceDecl) {
            nsIndent += 2;
            nsStackIdx++;
            nsStack[nsStackIdx] = nsPrefix(inNS, ast.params[0]);
          }
          else if (ast.indent <= nsIndent) {
            nsIndent -= 2;
            nsStackIdx--;
            assert(nsStackIdx >= 0);
          }

          matchTopLevel(id, inNS);
        }
      }
      foreach (f; found) unresolved.removeKey(f);
      enforce(unresolved.empty, text("Unresolved: ", unresolved));

      // Recursively analyze imports and their type dependencies.
      foreach (id; matches.data) analyze(id, true);

      // Make sure all links with definitions are fully analyzed.
      foreach (ref link; lookup) if (link.next == 0) finish(&link);

      // Resolve imports and load implementations from dynamic libraries.
      auto vars = vc.currentNS.vars;
      foreach (ref lib; libs) {
      loopSymbols:
        foreach (s; lib.symbols) {
          if (auto pm = s.name in macros) {
            try final switch (pm.kind) with (CMacro.Kind) {
            case Flag:
              writeln("FLAG ", pm.name);
              continue;
            case Alias:
            case Expr:
              // TODO
              // writeln("DEFINE ", pm.name, " ", pm.args, " = ", pm.expr);
              Any v;
              if (auto p = pm.name in macroExprs) v = *p;
              else v = buildCMacro(pm);
              // writeln("SEXP ", v.toString);
              vc.currentNS.define(Sym.intern(pm.name), v);
              continue;
            }
            catch (Throwable e) {
              writeln("DEFINE ", pm.name, " ", pm.expr, e);
              continue;
            }
          }
          auto p = s.toHash in vars;
          if (!p) {
            foreach (n; 0..cast(int)CNS.N) {
              if (auto t = s.name in decls[n]) {
                auto addr = *t;
                Link* link;
                do {
                  link = &lookup[addr];
                  addr = link.next;
                } while (addr);
                auto type = cast(Type*)link.def;
                auto a = type.aliased;
                if (a.type is typeType) {
                  switch (a.native) with (Type.Native) {
                  case Enum:
                    foreach (ref value; (cast(.Enum*)a).values) {
                      vc.currentNS.define(value.sym, value.data);
                    }
                    break;
                  default:
                  }
                }
                else {
                  assert(type.type is typeNS, text(type.sym.name, " ", type.type.sym.name));
                }
                vc.currentNS.define(type.sym, Any(&type.val));
                continue loopSymbols;
              }
            }
            writeln("TODO: missing ", *s);
            continue;
          }
          auto var = *p;
          switch (var.data.type.native) with (Type.Native) {
          case Enum:
            foreach (ref value; (cast(.Enum*)var.data.type).values) {
              vc.currentNS.define(value.sym, value.data);
            }
            break;
          case Fn: // Load the function pointer to be passed to ffi_call().
            auto fn = cast(.Fn*)var.data.val;
            assert(fn.sym is s, text(*fn.sym, " vs ", *s));
            assert(fn.isFFI);
            enforce(lib.handle, "missing :lib");
            fn.ffi = load(lib.handle, cast(char*)s.name.toStringz);
            break;
          case Pointer: // Load the variable pointer to a singleton.
            enforce(lib.handle, "missing :lib");
            var.data = .Any(load(lib.handle, cast(char*)s.name.toStringz));
            break;
          default:
            // writeln("IMPORT: ", var.data.toString); // TODO disallow dups first
          }
        }
      }
    }

    static size_t matchParen(string s, size_t idx) {
      auto level = 1;
      do {
        enforce(idx < s.length);
        switch (s[++idx]) {
        case '(': level++; break;
        case ')': level--; break;
        default:
        }
      } while (level > 0);
      return idx;
    }

    Any evalCMacro(string s) {
      // writeln("  M '", s, '\'');
      s = s.strip;
      enforce(s.length);
      switch (s[0]) {
      case '-':
      case '0':..
      case '9':
        bool unsigned;
        while (!isHexDigit(s[$-1])) {
          auto ch = s[$-1];
          s = s[0..$-1];
          switch (ch.toLower) {
          case 'l': break; // size += 4?
          case 'u': unsigned = true; break;
          default: enforce(0, "Unknown number suffix: " ~ ch);
          }
        }
        version (Windows) {
          s = s.replaceAll(oldIntegerSuffix, "");
        }
        enforce(!unsigned || s[0] != '-');
        Any v;
        /**/ if (s.startsWith("0x"))   return Any(s[2..$].to!ulong(16));
        else if (s.startsWith("-0x"))  return Any(-s[3..$].to!long(16));
        else if (s.indexOf('.') != -1) return Any(s.to!float);
        else if (unsigned)             return Any(s.to!ulong);
        else                           return Any(s.to!long);
      case '"':
        enforce(s[$-1] == '"');
        return Any(&new Str(s[1..$-1].dup).val); // TODO escapes
      case '*':
        auto list = new List(Any(symDeref));
        list.next = new List(evalCMacro(s[1..$]));
        return Any(&list.val);
      case '(':
        auto beg = 1L;
        auto ofs = 1L;
        auto end = matchParen(s, 0);
        int numOps;
        string[16] exprs = void;
        CMacroOp[15] ops = void;
        void addOp0(CMacroOp op) {
          if (beg==ofs) {
            ++ofs; // Don't add op if only one operand, ie (*foo)
          }
          else {
            exprs[numOps] = s[beg..ofs];
            ops[numOps++] = op;
            beg = ++ofs;
          }
          // writeln(op.name);
        }
        void addOp1(dchar next, CMacroOp then, CMacroOp otherwise) {
          if (s[ofs+1] == next) { addOp0(then); beg++; ofs++; }
          else { addOp0(otherwise); }
        }
        void addOp2(dchar next1, CMacroOp then1, dchar next2, CMacroOp then2, CMacroOp otherwise) {
          /**/ if (s[ofs+1] == next1) { addOp0(then1); beg++; ofs++; }
          else if (s[ofs+1] == next2) { addOp0(then2); beg++; ofs++; }
          else { addOp0(otherwise); }
        }
      expr:
        while (ofs < end) {
          // TODO assignments
          // TODO objc @ strings
          // TODO # concat
          switch (s[ofs]) {
          case '(': ofs = matchParen(s, ofs); break;
          case '+': addOp0(CMacroOp.Add); break;
          case '-': addOp0(CMacroOp.Sub); break;
          case '*': addOp0(CMacroOp.Mul); break;
          case '/': addOp0(CMacroOp.Div); break;
          case '%': addOp0(CMacroOp.Mod); break;
          case '~': addOp0(CMacroOp.BitNot); break;
          case '^': addOp0(CMacroOp.BitXor); break;
          case '|': addOp1('|', CMacroOp.Or,  CMacroOp.BitOr);  break;
          case '&': addOp1('&', CMacroOp.And, CMacroOp.BitAnd); break;
          case '<': addOp2('<', CMacroOp.BitShl, '=', CMacroOp.LessEq,    CMacroOp.Less);    break;
          case '>': addOp2('>', CMacroOp.BitShr, '=', CMacroOp.GreaterEq, CMacroOp.Greater); break;
          case '!': addOp1('=', CMacroOp.Neq, CMacroOp.Not); break;
          case '=':
            enforce(s[++ofs] == '=', s);
            addOp0(CMacroOp.Eq); break;
          default: ofs++;
          }
        }
        assert(ofs == end);
        if (numOps == 0) {
          // writeln("  ", beg, " ", ofs, " ", end, " ", s.length-1, s[beg..ofs]);
          if (ofs == s.length - 1) {
            // writeln("  VAL ", s[beg..ofs]);
            return evalCMacro(s[beg..ofs]);
          }
          // (BYTE)a
          // (123)|b
          else {
            auto idx = ofs;
            while (s[++idx].isWhite) {}
            switch (s[idx]) {
            case '+': case '-': case '*': case '/': case '%':
            case '~': case '^': case '|': case '&': case '<': case '>':
            case '!': case '=':
              beg--;
              end = s.length;
              ofs = idx;
              goto expr;
            default:
            }
            // writeln("  CAST ", s[beg..ofs], " => ", s[ofs+1..$]);
            auto sym = resolveCMacro(s[beg..ofs].strip, true);
            auto expr = evalCMacro(s[ofs+1..$]);
            auto rest = new List(sym, new List(expr));
            return Any(&new List(Any(symCast), rest).val);
          }
        }
        else {
          // writeln("  EXPR ", exprs[0..numOps], " ", s[beg..ofs]);
          // (1 + 2 * 3) => (+ 1 (* 2 3))
          // (1 + 2 + 3) => (+ 1 2 3)
          // (1 * 2 + 3) => (+ (* 1 2) 3)
          exprs[numOps] = s[beg..ofs];
          List* head;
          auto expr = new List(evalCMacro(exprs[0]));
          auto prec = 0;
          foreach (n; 0..numOps) {
            auto op = &cMacroOps[ops[n]];
            auto call = new List(Any(op.sym));
            if (op.precedence > prec) {
              call.next = expr;
              head = call;
            }
            else {
              assert(head);
              assert(expr.next);
              call.next = expr.next;
              expr.next = new List(Any(&call.val));
              expr = call.next;
            }
            assert(call.next);
            call.next.next = new List(evalCMacro(exprs[n + 1]));
            prec = op.precedence;
          }
          return Any(&head.val);
        }
      default:
        auto idx = s.indexOf('(');
        if (idx == -1) {
          // writeln("  VAR ", s);
          return resolveCMacro(s.strip);
        }
        else {
          auto beg = idx + 1;
          auto end = s.length - 1;
          enforce(s[end] == ')');
          auto f = s[0..idx].strip;
          if (f == "__pragma") {
            enforce(s[idx] == '(');
            idx = matchParen(s, idx + 1);
            return evalCMacro(s[idx + 1..$]);
          }
          auto form = new List(resolveCMacro(f));
          auto tail = form;
          auto count = 0;
          string[16] params = void;
          auto n = beg + 1;
          if (n < end) {
            for (; n < end; n++) {
              switch (s[n]) {
              case ',':
                params[count++] = s[beg..n];
                beg = n + 1;
                break;
              case '(':
                n = matchParen(s, n);
                break;
              default:
              }
            }
            params[count++] = s[beg..end];
          }
          // writeln("  CALL ", form.data.toString, " ", params[0..count]);
          foreach (p; params[0..count]) {
            tail.next = new List(evalCMacro(p));
            tail = tail.next;
          }
          return Any(&form.val);
        }
      }
    }

    Any buildCMacro(CMacro* m) {
      // writeln(m.kind, " ", m.name, " ", m.args);
      // enforce(m.kind == CMacro.Kind.Expr);
      while (m.kind == CMacro.Kind.Alias) {
        auto prev = m;
        m = m.expr in macros;
        //enforce(m);
        if (!m) {
          if (auto p = Sym.intern(prev.expr).toHash in vc.currentNS.vars) {
            return (*p).data;
          }
          else {
            writeln("Missing macro ", prev.name, " ", prev.expr);
            return nil;
          }
        }
      }
      auto v = evalCMacro(m.expr);
      auto env = Env(rt.rootEnv);
      env.ns = vc.currentNS;
      if (m.args) {
        enforce(v.isVal);
        auto form = new List(Any(symFn));
        auto params = new Any[m.args.length];
        foreach (i, arg; m.args) {
          params[i] = Any(Sym.intern(arg));
        }
        form.next = new List(Any(Sym.intern(m.name)));
        form.next.next = new List(Any(&new Vec(params).val));
        form.next.next.next = new List(v);
        // writeln("SEXP ", form.toString);
        v = Any(&form.val).eval(env);
      }
      else {
        // writeln("SEXP ", v.toString);
        v = v.eval(env);
      }
      macroExprs[m.name] = v;
      return v;
    }

    Any resolveCMacro(string name, bool isType = false) {
      if (auto p = name in macroExprs) return *p;
      if (auto m = name in macros) return buildCMacro(m);
      return Any(isType ? parseType(name).sym : Sym.intern(name));
    }

    void matchAll(uint id) {
      auto ast = node(id);
      switch (ast.kind) with (Node.Kind) {
      default: return;
      case ObjCCategoryDecl:
      case ObjCProtocolDecl:
      case ObjCInterfaceDecl:
      case CXXRecordDecl:
      case RecordDecl:
      case EnumDecl:
      case NamespaceDecl:
      case TypedefDecl:
      case ClassTemplateDecl:
      case ClassTemplateSpecializationDecl:
      }
      lookup[ast.addr] = Link(id, ast.prev); // Index by AST node address.
      if (ast.prev) {
        auto p = ast.prev in lookup;
        enforce(p, format("Missing prev %x for %x", ast.prev, ast.addr));
        enforce(p.next == 0);
        p.next = ast.addr;
      }
    }

    /// Indexes top-level definitions for lookup during analyze().
    void matchTopLevel(uint id, string path) {
      auto overwrite = true;
      auto ast = node(id);
      CNS cns;
      switch (ast.kind) with (Node.Kind) {
      case EnumDecl:
        // if (ast.params[1].empty) return; // Ignore anonymous TODO doesnt work?
        cns = CNS.Enum;
        break;
      case CXXRecordDecl:
      case RecordDecl:
        if (ast.params[0] == "definition") return; // Ignore anonymous
        overwrite = ast.params[1] == "definition";
        cns = CNS.Struct;
        break;
      case ObjCCategoryDecl: cns = CNS.Category; break;
      case ObjCProtocolDecl: cns = CNS.Protocol; break;
      default:               cns = CNS.Var;
      }
      auto name = ast.params[0];
      if (!name.empty) {
        if (path) name = path ~ name;
        // writefln("ADDR %s %x", name, ast.addr);
        if (name !in decls[cns] || overwrite) {
          decls[cns][name] = ast.addr; // Index by last definition name.
          if (name in unresolved) {
            matches.put(id); // Force analyze() of this definition.
            found.insert(name);  // Defer removing from `unresolved`.
          }
        }
      }
    }

    string getTemplateParams(ref uint id, ubyte indent) {
      id++;
      auto ast = node(id);
      enforce(ast.indent > indent);
      enforce(ast.kind == Node.Kind.DefinitionData);
      skipChildren(id, indent);
      Appender!string params;
      while (true) {
        ast = node(id);
        if (ast.indent <= indent) {
          id--;
          return params.data;
        }
        if (ast.kind != Node.Kind.TemplateArgument) {
          return params.data;
        }

        enforce(params.data.length == 0, "TODO template param sep syntax?");
        params.put(ast.params[0]);
      }
    }

    void finish(Link* link) {
      if (link.def is null) return;
      if (link.prev == 0) return;
      auto prev = link.prev in lookup;
      prev.def = link.def;
      analyze(prev.id);
      finish(prev);
    }

    /// Interns a foreign definition.
    Def* analyze(uint id, bool topLevel = false) {
      auto ast = node(id);
      if (ast.analyzed) {
        if (topLevel) return null;
        return lookup[ast.addr].def.enforce("Missing def");
      }
      // writefln("ANALYZE %s %x %s %s", id, ast.addr, ast.kind, ast.params[0]);
      switch (ast.kind) with (Node.Kind) {
      // C
      case VarDecl:           return analyzeVar (id);
      case EnumDecl:          return analyzeEnum(id);
      case RecordDecl:        return analyzeData(id);
      case TypedefDecl:       return analyzeType(id);
      case FunctionDecl:      return analyzeFn  (id);
      // C++
      case NamespaceDecl:     return analyzeNS  (id);
      case CXXRecordDecl:     return analyzeData(id);
      case ClassTemplateDecl: return analyzeTmpl(id);
      case UsingDecl:
      case UsingShadowDecl:
      case FullComment:       return null; // TODO: namespace data
      // Objective-C
      case ObjCCategoryDecl:  version (OSX) return analyzeObjcCategory (id);
      case ObjCProtocolDecl:  version (OSX) return analyzeObjcProtocol (id);
      case ObjCInterfaceDecl: version (OSX) return analyzeObjcInterface(id);
      // Unsupported
      default:
        enforce(0, text("ANALYZE: ", id, " ", ast.kind));
        assert(0);
      }
    }

    /// Called on visited AST children.
    alias ADg = void delegate(ref Node, Def*);
    alias CDg = Def* delegate();
    alias EDg = void delegate(Def*);

    Def* walkForwards(Link* link, scope CDg create) {
      if (link.def) return link.def;
      if (link.prev == 0) return create();
      debug {
        auto ast = node(link.id);
        // writefln("PREV %s %s %x", ast.kind, ast.params[0], ast.addr);
      }
      auto prev = link.prev in lookup;
      return prev.def ? prev.def : walkForwards(prev, create);
    }

    /// Iterates AST children. Calls analyze() until child.indent <= indent.
    Def* visit(bool useLink = true)(ref uint id,
                                    scope ADg act,
                                    scope CDg create,
                                    EDg enter = null) {
      auto top = id;
      auto ast = node(id);
      // writefln("VISIT %s %x %s %s", id, ast.addr, ast.kind, ast.params[0]);
      assert(ast.parsed, id.to!string);
      assert(!ast.analyzed,  ast.params[0]);
      assert(!ast.analyzing, text(id, " ", ast.params[0]));
      auto last = parser.parseLines(id + 1, ast.indent);
      auto max  = parser.lines.length;
      static if (useLink) {
        auto link = ast.addr in lookup;
        assert(link, format("Missing link: %s %x", id, ast.addr));
        assert(link.id == id, format("%x %s %s", ast.addr, link.id, id));
        auto def = walkForwards(link, create);
        link.def = def;
        assert(def);
      }
      else {
        auto def = create();
      }
      if (enter) enter(def);
      ast.analyzing = true;
      id++;
      while (id < max) {
        auto child = node(id);
        assert(child.parsed);
        if (child.indent <= ast.indent) break;
        assert(!child.analyzed, parser.text[id]);
        act(*child, def);
        child.analyzed = true;
        id++;
      }
      ast.analyzed = true;
      // writeln("L ", top, " ", ast.kind, " ", ast.params[0]);
      assert(last == uint.max || id == last, text(last, " got ", id));
      return def;
    }

    // TODO: implicit typedef A 'A' -> actual doesn't "prev" it
    // TODO: index type by name after create

    version (OSX)
    IMP getObjcImpl(Class cls, SEL sel, bool isStatic) {
      assert(cls);
      assert(sel);
      auto m = isStatic
        ? class_getClassMethod(cls, sel) // TODO class_getMethodImplementation doesnt work?
        : class_getInstanceMethod(cls, sel);
      auto ret = method_getImplementation(m);
      // if (!ret) writeln("Missing imp: ", class_getName(cls).to!string, ' ', sel_getName(sel).to!string);
      return ret;
    }

    version (OSX)
    .Fn* objcPropFn(Sym* sym, const(char)* name, Class cls, Struct* type, bool isStatic, bool isGetter, Type* arg) {
      auto sel = sel_getUid(name);
      auto imp = getObjcImpl(cls, sel, isStatic);
      if (!imp) return null;
      auto fn = new .Fn(sym);
      fn.ffi          = imp;
      fn.isObjC       = true;
      fn.isStatic     = isStatic;
      fn.external.sel = sel;
      FnType* fnt;
      FnType.Opts opts;
      opts.abi = FnType.ABI.C;
      opts.objc = true;
      if (isGetter) {
        fnt = FnType.intern(arg, null, opts);
      }
      else {
        fnt = FnType.intern(typeVoid, [arg], opts);
        fn.params = [sym_];
      }
      compile(fnt, true);
      fn.type = cast(Type*)fnt;
      return fn;
    }

    /// Interns an Objective-C category definition.
    alias analyzeObjcCategory = analyzeObjcInterface!(true, false);
    /// Interns an Objective-C protocol definition.
    alias analyzeObjcProtocol = analyzeObjcInterface!(false, true);
    /// Interns an Objective-C interface definition.
    version (OSX)
    Def* analyzeObjcInterface(bool cat = false, bool proto = false)(uint id) {
      auto cns  = proto ? CNS.Protocol : CNS.Var; // isolate protocols
      auto ast  = node(id);
      auto sym  = Sym.intern(ast.params[0]);
      auto hash = sym.toHash;
      auto save = objcInstance;
      assert(ast.kind == Node.Kind.ObjCCategoryDecl  ||
             ast.kind == Node.Kind.ObjCInterfaceDecl ||
             ast.kind == Node.Kind.ObjCProtocolDecl);
      // writeln("OBJC: ", id, " ", ast.params[0], " cat=", cat, " proto=", proto);
      Def* create() {
        auto stype = new Struct(sym, Type.Native.NSObject);
        // Objective-C accesses implementations independently of shared library.
        // Load them now since the associated library has already been opened.
        auto name = sym.name.toStringz;
        static if (proto) {
          stype.isProtocol = true;
          stype.objc = cast(Class)objc_getProtocol(name);
          // if (!stype.objc) writeln("Protocol not found: " ~ sym.name);
        }
        else static if (!cat) {
          stype.objc = objc_getClass(name);
          // enforce(stype.objc, "Class not found: " ~ sym.name);
          if (!stype.objc) writeln("Class not found: ", sym.name);
          else classes[stype.objc] = stype;
        }
        else {
          stype.isCategory = true;
        }
        auto type = &stype.type;
        return &type.def;
      }
      void enter(Def* def) { // Resolve instancetype to this type.
        objcInstance = cast(Type*)def;
      }
      // writeln(isProtocol ? "PROTO " : "IFACE ", id, " ",
      //         type.def.sym.toString, " ", ast.loc.file);
      Appender!(Type*[]) protos;
      Appender!(Struct.Member[]) fields;
      void child(ref Node child, Def* def) {
        auto type = cast(Struct*)def;
        auto xs   = child.params;
        switch (child.kind) with (Node.Kind) {
        // case "super": // Required up to NSObject, must be another interface.
        case ObjCInterface:
          static if (proto) enforce(0, text(id));
          else {
            if (type.next) {
              // TODO: enforce same
              break;
            }
            type.next = resolveType(child.addr);
            enforce(type.next.native == Type.Native.NSObject);
            static if (cat) (cast(Struct*)type.next).exts ~= &type.type;
          }
          break;
        case ObjCProtocol: // Protocols implemented by this interface.
          protos.put(resolveProtocol(child.addr));
          break;
        case ObjCRootClassAttr: // Root of the Objective-C class hierarchy.
          enforce(!cat && !proto);
          enforce(ast.params[0] == "NSObject" ||
                  ast.params[0] == "NSProxy",
                  ast.params[0]);
          // TODO: disallow super
          break;
        case ObjCExplicitProtocolImplAttr: // TODO
          break;
        case ObjCIvarDecl: // TODO
          // 0 name
          // 1 type
          // + attr
          break;
        case ObjCPropertyDecl: // Variable at the class or object scope.
          auto prop = new Prop(Sym.intern(xs[0]), parseType(xs[1]));
          checkForward(prop.type);
          loop: foreach (attr; xs[2..$]) {
            switch (attr) {
            case "": break loop;
            case "readonly": prop.readonly = true; break;
            case "atomic":   prop.atomic   = true; break;
            case "class":    prop.isStatic = true; break;
            default: enforce(attr);
            }
          }
          string getter;
          string setter; // TODO can override name?
          while (true) {
            auto next = id + 1;
            auto attr = node(next);
            if (attr.indent <= child.indent) break;
            switch (attr.kind) {
            case ObjCMethod:
              enforce(attr.isGetter);
              getter = attr.params[0];
              break;
            case AvailabilityAttr: break;
            case CFReturnsNotRetainedAttr: break;
            case IBOutletAttr: break;
            case ObjCReturnsInnerPointerAttr: break;
            case SwiftPrivateAttr: break;
            default: enforce(0, text("PROP: ", next, " ", attr.kind));
            }
            id = next;
          }
          if (!proto) {
            auto name = prop.sym.name.toStringz;
            auto cls = cast(Class)(cat ? type.next.objc : type.objc);
            auto ivar = prop.isStatic
              ? class_getClassVariable(cls, name)
              : class_getInstanceVariable(cls, name);
            prop.ivarOffset = ivar ? cast(short)ivar_getOffset(ivar) : -1;
            Sym* sym;
            if (getter.empty) sym = prop.sym;
            else {
              name = getter.toStringz;
              sym = Sym.intern(getter);
            }
            prop.get = objcPropFn(sym, name, cls, type, prop.isStatic, true, prop.arg);
            if (prop.get && !prop.readonly) {
              if (setter.empty) {
                auto s = prop.sym.name;
                setter = text("set", s[0].toUpper, s[1..$], ':');
              }
              prop.set = objcPropFn(Sym.intern(setter), setter.toStringz, cls,
                                    type, prop.isStatic, false, prop.arg);
            }
          }
          fields.put(Struct.Member(prop));
          break;
        case ObjCMethodDecl: // Function at the class or object scope.
          if (child.implicit) {
            skipChildren(id, child.indent);
            break;
          }
          auto fn = cast(.Fn*)analyzeFn(id, type, true);
          static if (!proto) {
            fn.ffi = getObjcImpl(cast(Class)(cat ? type.next.objc : type.objc),
                                 fn.external.sel, fn.isStatic);
          }
          fields.put(Struct.Member(fn));
          id--;
          break;
        case ObjCTypeParamDecl:
          type.objcGeneric = true;
          skipChildren(id, ast.indent);
          break;
        case ObjCExceptionAttr: break;
        case ObjCRequiresPropertyDefsAttr: break;
        case ArcWeakrefUnavailableAttr: break;
        case IBOutletAttr: break;
        case AvailabilityAttr: // TODO attributes
        case DeprecatedAttr:
        case UnavailableAttr:
        case UnusedAttr: break;
        case VisibilityAttr:
          // xs[0] == "Default"
          break;
        default: writeln("OBJC CHILD: ", id, " ", parser.text[id]);
        }
      }
      auto loc = id;
      auto def = visit(id, &child, &create, &enter);
      assert(objcInstance is cast(Type*)def);
      objcInstance = save;
      if (protos.data.length) {
        auto type = cast(Struct*)def;
        enforce(type.protos.empty || type.protos.length == protos.data.length);
        type.protos = protos.data;
      }
      return setMembers(loc, def, fields.data, 0);
    }
    else
    Def* analyzeObjcInterface(bool cat = false, bool proto = false)(uint id) {
      assert(0);
    }

    Def* setMembers(uint id, Def* def, Struct.Member[] fields, size_t size) {
      auto type = cast(Struct*)def;
      if (!fields.empty) {
        auto dbg = def.sym.name == "DXGI_RGB";
        auto calcAlign = type.alignment == Type.invalidAlign;
        auto alignment = calcAlign ? 0 : type.alignment;
        foreach (ref m; fields) {
          checkForward(m.type);
          if (calcAlign && m.type && m.type.alignment > alignment) {
            alignment = m.type.alignment;
          }
        }
        enforce(type.fields.empty || type.fields is fields);
        copyMeta(id, def);
        type.fields = fields;
        type.alignment = alignment.to!ushort;
        if (type.isUnion) {
          size = 0;
          foreach (ref m; fields) {
            m.offset = 0;
            if (m.type.size > size) size = m.type.size;
          }
        }
        alignAddr(size, alignment);
        type.size = size.to!ushort;
      }
      else if (type.alignment == Type.invalidAlign) {
        type.alignment = 0;
      }
      return def;
    }

    Any analyzeVal(ref uint id, Node* ast, long* next, Def* def) {
      auto child = node(id);
      if (child.indent <= ast.indent) return Any((*next.enforce())++);
      id++;
      Any ret;
      auto xs = child.params;
      switch (child.kind) with (Node.Kind) {
      case ConstantExpr:
      case ImplicitCastExpr:
      case ParenExpr:
        auto t = parseType(xs[0]);
        auto v = analyzeVal(id, ast, null, def);
        ret = v; // TODO cast to t, checkForward
        break;
      case DeclRefExpr:
        enforce(def);
        enforce(xs[1] == "EnumConstant");
        enforce(xs[2].startsWith("0x")); // Points to EnumConstantDecl, not in addr lookup.
        auto t = xs[4] == "int" ? cast(.Type*)def : parseType(xs[4]).aliased;
        switch (t.native) {
        case Type.Native.Enum:
          auto s = Sym.intern(xs[3]);
          ret = (cast(.Enum*)t).lookup(s);
          break;
        case Type.Native.SInt8:..
        case Type.Native.UInt64:
          // TODO resolve enum value, need lookup table, maybe resolve other enum type
          break;
        default:
          enforce(t.native is Type.Native.Enum, text(id, " ", t.native));
        }
        break;
      case CharacterLiteral:
      case IntegerLiteral:
        auto t = parseType(xs[0]);
        ret = t.unsigned ? Any(xs[1].to!ulong) : Any(xs[1].to!long);
        break;
      case UnaryOperator:
        auto a = analyzeVal(id, ast, null, def);
        auto n = 1;
        bool isPrefix;
        if (xs[n] == "prefix") {
          isPrefix = true;
          n++;
        }
        switch (xs[n]) {
        case "-":
          // TODO
          break;
        default:
          enforce(0, xs[n]);
          assert(0);
        }
        break;
      case BinaryOperator:
        auto a = analyzeVal(id, ast, null, def);
        auto b = analyzeVal(id, ast, null, def);
        switch (xs[1]) {
        case "+":
        case "-":
        case "*":
        case "/":
        case "|":
        case "&":
        case "<<":
        case ">>":
          // TODO
          break;
        default:
          enforce(0, xs[1]);
          assert(0);
        }
        break;
      case FullComment:
        skipChildren(id, child.indent);
        id++;
        break;
      case AvailabilityAttr: break;
      default:
        enforce(0, text("VAL: ", child.kind, " ", id));
        assert(0);
      }
      if (next) {
        // assert(0, "TODO"); // set next val; signed/unsigned
      }
      return ret;
    }

    /// Interns a C enumerated type.
    Def* analyzeEnum(uint id) {
      // writeln("ENUM: ", id, " ", node(id).params);
      long init;
      Type* commonType;
      auto useCommonType = true;
      int nextValue;
      Appender!(Enum.Value[]) values;
      void child(ref Node child, Def* def) {
        switch (child.kind) with (Node.Kind) {
        case EnumConstantDecl:
          Type* type;
          if (child.params[1] == def.sym.name) {
            type = typeSInt32;
          }
          else {
            type = parseType(child.params[1]);
          }
          if (useCommonType) {
            if (!commonType) commonType = type;
            else if (commonType !is type) useCommonType = false;
          }
          id++;
          values.put(.Enum.Value(Sym.intern(child.params[0]),
                                 analyzeVal(id, &child, &init, def)));
          (cast(.Enum*)def).values = values.data;
          id--;
          break;
        case FullComment:
          skipChildren(id, child.indent); // TODO comments
          break;
        case AvailabilityAttr: // TODO attributes
        case EnumExtensibilityAttr:
        case FlagEnumAttr: break;
        case SwiftNameAttr: break;
        case NSErrorDomainAttr: break;
        default: writeln("ENUM CHILD: ", parser.text[id]);
        }
      }
      auto ast = node(id);
      assert(ast.kind == Node.Kind.EnumDecl);
      Def* create() {
        Sym*  sym;
        Type* type;
        auto xs = node(id).params;
        if (!xs[1].empty) {
          sym = Sym.intern(xs[0]);
          type = parseType(xs[1]);
        }
        else if (parser.text[id][$-1] == '\'') {
          sym  = symAnon;
          type = parseType(xs[0]);
        }
        else sym = Sym.intern(xs[0]);
        auto e   = new Enum(sym);
        if (type) {
          e.next   = type;
          checkForward(e.next);
        }
        return &e.def;
      }
      auto loc = id;
      auto def = visit(id, &child, &create);
      auto e   = cast(Enum*)def;
      if (!values.data.empty) {
        assert(e.values is values.data);
        copyMeta(loc, def);
      }
      if (e.next) {
        enforce(!useCommonType || !commonType || commonType is e.next,
                text("Enum got ", *e.next, " expecting ", *commonType));
      }
      else if (useCommonType) e.next = commonType;
      else e.next = typeSInt32;
      e.size = e.next.size;
      e.alignment = e.next.alignment;
      return def;
    }

    /// Interns a foreign struct/union type.
    alias void delegate(Type*) AddMemberDg;
    Def* analyzeData(ref uint id, AddMemberDg addMember = null, int numTmplParams = 0, Generic* generic = null) {
      auto ast = node(id);
      // writefln("DATA %s %x %s", id, ast.addr, ast.params[0]);
      assert(numTmplParams > 0 ?
             (ast.kind == Node.Kind.ClassTemplateSpecializationDecl) :
             (ast.kind == Node.Kind.RecordDecl ||
              ast.kind == Node.Kind.CXXRecordDecl));
      void enter(Def* def) {
        auto s = cast(Struct*)def;
        structStack[++structStackIdx] = s;
        if (s.fields.length == 0) s.alignment = Type.invalidAlign;
      }
      Def* create() {
        auto n  = 0;
        auto xs = ast.params;
        if (xs[n] == "definition") n++; // TODO definition at 0 means anonymous, no definition means forward decl
        auto name = ast.params[n];
        auto sym  = name.empty ? symAnon : Sym.intern(name);
        auto s    = new Struct(sym);
        s.isUnion = ast.declEnum == DeclEnum.Union;
        // writefln("CREATE %s %x", name, s);
        // if (!name.empty) decls[CNS.Struct][name] = ast.addr;
        // TODO: should lookup in existing types (previous passes) first
        //       this will allow these to be built-in and remove this switch
        version (OSX) switch (name) {
        case "objc_class":
        case "objc_object":
        case "objc_selector": s.native = Type.Native.NSObject; break;
        default:
        }
        else version (Windows) {
          if (name == "IUnknown") {
            s.win32Com = true;
          }
        }
        if (addMember) addMember(&s.type);
        return &s.def;
      }
      Appender!(string[]) params;
      Appender!(Type*[]) protos;
      Appender!(Struct.Member[]) fields;
      ushort vtblIdx;
      auto alignment = Type.invalidAlign;
      ubyte bitOffset;
      size_t nextOffset;
      void child(ref Node child, Def* def) {
        auto s = cast(Struct*)def;
        auto xs = child.params;
        switch (child.kind) with (Node.Kind) {
        case _public:
          auto p = child.params[0] in decls[CNS.Struct];
          enforce(p, text("Missing parent type ", child.params[0], " for ", def.sym.toString));
          auto t = resolveStruct(*p);
          protos.put(t);
          s.protos = protos.data;
          vtblIdx += (cast(Struct*)t).numVirtuals;
          break;
        case AccessSpecDecl: break; // TODO
        case FieldDecl: // Ordered and aligned data member with offset.
          enforce(xs[1].length, text(id, " ", child));
          auto type = parseType(xs[1]);
          alignAddr(nextOffset, min(s.alignment, type.alignment));
          fields.put(Struct.Member(xs[0], type, false, nextOffset));
          s.fields = fields.data;
          nextOffset += type.size;
          break;
        case ConstantExpr:
          expect(id, IntegerLiteral);
          goto case;
        case IntegerLiteral:
          auto f = &fields.data[$-1];
          f.bitOffset = bitOffset;
          f.bitSize   = node(id).params[1].to!ubyte;
          bitOffset   = (bitOffset + f.bitSize) & ((1 << 6) - 1);
          // TODO adjust nextOffset & f.offset
          break;
        case IndirectFieldDecl:
        case Field: break; // TODO (anonymous union/struct)
        case CXXRecordDecl:
          if (child.params[0] != "definition" && child.params[0] == ast.params[0]) {
            enforce(def.sym !is symAnon, format("%s %s %s", id, child.params[0], ast.params[0]));
            auto cxx = new Type(Type.Native.Alias, def.sym);
            cxx.next = &s.type;
            break;
          }
          goto case;
        case RecordDecl: // Nested struct/union type.
          auto n = 0;
          if (xs[0] == "definition" && xs[1].length == 0) {
            child.definition = true;
            n++;
          }
          else if (xs[1] == "definition") child.definition = true;
          Type* data;
          void addNested(Type* t) {
            assert(!data);
            data = t;
            if (!child.definition) {
              fields.put(Struct.Member(xs[n], data, true, 0));
              s.fields = fields.data;
            }
          }
          analyzeData(id, &addNested);
          assert(data);
          if (xs[n].empty) { // Anonymous declaration; field of that type next
            auto ast = node(id);
            enforce(ast.kind == Node.Kind.FieldDecl, ast.kind.to!string);
            alignAddr(nextOffset, min(s.alignment, data.alignment));
            auto stype = cast(Struct*)data;
            if (stype.native == Type.Native.Struct && stype.sym is symAnon && child.definition) {
              foreach (ref m; stype.fields) {
                switch (m.kind) with (Struct.Member.Kind) {
                default: assert(0, text(id, " ", m.kind));
                case Data:
                  m.offset += nextOffset.to!ushort;
                  fields.put(m);
                  break;
                }
              }
              s.fields = fields.data;
              nextOffset += data.size;
            }
            else {
              fields.put(Struct.Member("anonymous", data, false, nextOffset));
              s.fields = fields.data;
            }
            nextOffset += data.size;
          }
          else { // Named declaration, can have many users, even out of parent
            // data.sym = Sym.intern(xs[n]);
            id--;
          }
          break;
        case TypedefDecl:
          auto type = cast(Type*)analyzeType(id);
          alignAddr(nextOffset, min(s.alignment, type.alignment));
          fields.put(Struct.Member(xs[0], type, true, nextOffset));
          s.fields = fields.data;
          nextOffset += type.size;
          id--;
          break;
        case VarDecl:
          // TODO static vars
          break;
        case CXXConversionDecl:
          skipChildren(id, child.indent);
          break;
        case CXXConstructorDecl:
        case CXXDestructorDecl:
        case CXXMethodDecl:
          fields.put(Struct.Member(cast(.Fn*)analyzeFn(id, s, false)));
          if (child.virtual) fields.data[$-1].offset = vtblIdx++;
          s.fields = fields.data;
          id--;
          break;
        case FunctionTemplateDecl:
          skipChildren(id, child.indent);
          break;
        case DefinitionData: break;
        case DefaultConstructor: break;
        case CopyConstructor: break;
        case MoveConstructor: break;
        case CopyAssignment: break;
        case MoveAssignment: break;
        case Destructor: break;
        case AlignedAttr:
          expect(id, ConstantExpr, UnaryExprOrTypeTraitExpr);
          auto unary = node(id);
          if (unary.kind == UnaryExprOrTypeTraitExpr) {
            enforce(unary.params[1] == "alignof");
            alignment = parseType(unary.params[0]).alignment;
          }
          else {
            expect(id, IntegerLiteral);
            alignment = node(id).params[1].to!ushort;
          }
          break;
        case MaxFieldAlignmentAttr:
          s.alignment = child.params[0].to!ushort / 8;
          break;
        case PackedAttr: break; // TODO
        case DeprecatedAttr: break;
        case UnavailableAttr: break;
        case VisibilityAttr: break;
        case ObjCBoxableAttr: break;
        case ObjCBridgeAttr: break;
        case ObjCBridgeMutableAttr: break;
        case MSNoVTableAttr: break;
        case UuidAttr:
          version (Windows) {
            s.win32Com = true;
            auto val = cast(Val*)new ubyte[16+UUID.sizeof].ptr;
            auto iid = cast(wintyp.GUID*)(cast(void*)val + 16);
            val.type = parseType("IID");
            *cast(UUID*)iid = parseUUID(child.params[0]);
            version (LittleEndian) { // Should be big-endian but UUID stores ubyte[16]
              iid.Data1 = swapEndian(iid.Data1);
              iid.Data2 = swapEndian(iid.Data2);
              iid.Data3 = swapEndian(iid.Data3);
            }
            vc.currentNS.define(Sym.intern("IID_"~s.sym.name), Any(val));
          }
          break;
        case FullComment:
          skipChildren(id, child.indent);
          break;
        case TemplateArgument:
          enforce(params.data.length < numTmplParams);
          assert(child.params[0] == "type", "TODO non-type tmpl params");
          assert(params.data.empty, "TODO multiple");
          params.put(child.params[1]);
          if (params.data.length == numTmplParams) {
            s.isTemplate = true;
            auto paramsStr = params.data.join(", ");
            auto inst = generic.addInstance(cast(Type*)def, paramsStr);
            s.params = new Def*[params.data.length];
            foreach (i, param; params.data) {
              s.params[i] = &parseType(param).def;
            }
            inst.params = s.params;
            foreach (i, param; s.params) {
              auto t = cast(Type*)param;
              auto a = t.aliased;
              if (a !is t) {
                if (inst.params is s.params) inst.params = inst.params.dup;
                inst.params[i] = &a.def;
              }
            }
            auto name = text(s.sym.name, '<', paramsStr, '>');
            auto link = Link(0, uint.max);
            link.def = def;
            lookup[child.addr] = link;
            decls[CNS.Struct][name] = child.addr;
          }
          break;
        default: writeln(id, " ", child.kind);writeln("DATA: ", child);enforce(0);
        }
      }
      auto loc = id;
      auto def = visit(id, &child, &create, &enter);
      auto type = cast(Struct*)def;
      type.numVirtuals = vtblIdx;
      // assert(type.protos.length == 0 || type.protos is protos.data); // TODO forward decls screw this up
      structStackIdx--;
      assert(structStackIdx >= -1);
      if (alignment != Type.invalidAlign) type.alignment = alignment;
      return setMembers(loc, def, fields.data, nextOffset);
    }

    Def* analyzeTmpl(uint id) {
      auto ast = node(id);
      // writefln("TMPL %s %x %s", id, ast.addr, ast.params[0]);
      assert(ast.kind == Node.Kind.ClassTemplateDecl);
      int numParams;
      bool paramsDone;
      Def* create() {
        auto sym = Sym.intern(ast.params[0]);
        auto s = new Generic(sym);
        return &s.def;
      }
      void child(ref Node child, Def* def) {
        switch (child.kind) with (Node.Kind) {
        case TemplateTypeParmDecl:
          enforce(!paramsDone);
          numParams++;
          break;
        case CXXRecordDecl:
          skipChildren(id, child.indent); // TODO generic decl
          break;
        case ClassTemplateSpecializationDecl:
          enforce(numParams > 0);
          paramsDone = true;
          analyzeData(id, null, numParams, cast(Generic*)def);
          id--;
          break;
        default:
          enforce(0, text("TMPL: ", id, " ", child.kind));
        }
      }
      auto loc = id;
      auto def = visit(id, &child, &create);
      return def;
    }

    alias TypeCtor = Type* function(Type*);

    /// Interns a foreign type alias.
    Def* analyzeType(ref uint id) {
      Type* type;
      TypeCtor[16] ctors = void;
      auto n = 0;
      auto ast = node(id);
      assert(ast.kind == Node.Kind.TypedefDecl);
      // writefln("TYPE: %s %x %s", id, ast.addr, ast.params[0]);
      Def* create() {
        auto type = new Type(Type.Native.Alias, Sym.intern(ast.params[0]));
        auto def  = &type.def;
        copyMeta(id, def);
        return def;
      }
      void child(ref Node child, Def*) {
        switch (child.kind) with (Node.Kind) {
        case BuiltinType:
        case ConstantArrayType:
          // Handled in 'parseType'.
          skipAllChildren(id, child.indent);
          break;
        case TypedefType:
          // Sugar for a typedef referencing another typedef. Has exactly two children.
          // 1. Typedef node containing the target typedef. Has a matching TypedefDecl.
          // 2. *Type node describing the type this typedef refers to.
          // These types are handled in 'parseType'.
          expect(id, Typedef);
          expect(id, BuiltinType, PointerType, TypedefType, ElaboratedType, AttributedType);
          skipAllChildren(id, child.indent);
          break;
        case AttributedType:
          // Applies an `__attribute__((...))` to a type.
          // Has exactly two children: the equivalent and the canonical types.
          next(id);
          auto astEquiv = node(id);
          skipChildren(id, astEquiv.indent);
          expect(id, astEquiv.kind);
          debug auto astCanon = node(id);
          debug enforce(astEquiv.addr == astCanon.addr, text(id, " equivalent != canonical"));
          id--;
          // TODO extract the attribute and apply to the type?
          break;
        case ParenType:
          expect(id, FunctionProtoType);
          goto case;
        case BlockPointerType:
        case FunctionProtoType:
          skipAllChildren(id, node(id).indent);
          break;
        // case BlockPointerType:
        //   // TODO
        //   break;
        case QualType:
          // Type constructors for 'const', 'resrict' and 'volatile'.
          // TODO
          break;
        case PointerType:
          // Type constructor used with elaborated types.
          // Other pointers are resolved in `parseType`.
          ctors[n++] = &Type.pointer;
          break;
        case ElaboratedType:
          // Alias into the 'enum' or 'struct' namespaces.
          expect(id, EnumType, RecordType);
          id--;
          break;
        case EnumType:
          expect(id, Enum);
          assert(!type);
          type = resolveEnum(node(id).addr);
          break;
        case RecordType:
          expect(id, Record, CXXRecord);
          assert(!type);
          type = resolveStruct(node(id).addr);
          break;
        case FullComment:
          skipChildren(id, child.indent);
          break;
        // case SubstTemplateTypeParmType:
        //   skipChildren(id, child.indent);
        //   break;
        case AvailabilityAttr: break;
        case DeprecatedAttr: break;
        // case FormatAttr: break;
        case ObjCIndependentClassAttr: break;
        case ObjCObjectPointerType:
          skipChildren(id, child.indent);
          // expect(id, ObjCObjectType); // can also be ObjCInterfaceType->ObjCInterface
          break;
        case SwiftBridgedTypedefAttr:
        case SwiftNameAttr:
        case SwiftNewtypeAttr: break;
        default:
          enforce(0, text("TYPE: ", id, " ", child.kind));
        }
      }
      auto loc  = id;
      auto def  = visit(id, &child, &create);
      auto decl = cast(Type*)def;
      if (!type) type = parseType(ast.params[1]);
      else foreach_reverse (i; 0..n) type = ctors[i](type); // TODO cache type ctors?

      // Duplicate typedefs, check they both refer to the same type, keep the original definition.
      if (!decl.next) {
        decl.next = type;
        decl.size = type.size;
        decl.alignment = type.alignment;
        checkForward(decl.next);
      }
      else {
        enforce(!decl.next || type.aliased is decl.next.aliased,
                format("%s type=%s decl=%s next=%s", loc, type.sym.name,
                       decl.sym.name, decl.next ? decl.next.sym.name : ""));
      }
      return def;
    }

    /// Interns a foreign variable.
    Def* analyzeVar(uint id) {
      // writeln("VAR: ", id, " ", node(id).params[0]);
      auto ast = node(id);
      Def* create() {
        return &vc.currentNS.define(Sym.intern(ast.params[0]), Unbound.val).def;
      }
      void child(ref Node child, Def* def) {
        switch (child.kind) with (Node.Kind) {
        case FloatingLiteral: break;
        case IntegerLiteral: break;
        default: writeln("VAR: ", id, " ", child);
        }
      }
      auto loc = id;
      auto def = visit!false(id, &child, &create);
      copyMeta(id, def);
      return def;
    }

    /// Interns a foreign function.
    Def* analyzeFn(uint id) { return analyzeFn(id, null, false); }
    /// Interns a foreign function or member.
    Def* analyzeFn(ref uint id, Struct* parent, bool objc) {
      version (OSX) {} else assert(!objc);
      // writeln("FN: ", id, " ", node(id).params[0]);
      string doc;
      Type* ret;
      Appender!(Type*[]) args;
      Appender!(Sym* []) params;
      FnType.Opts opts;
      opts.abi = FnType.ABI.C;
      opts.objc = objc;
      opts.self = parent !is null;
      Def* create() {
        auto ast  = node(id);
        auto name = ast.params[0];
        auto fn   = new .Fn(Sym.intern(name));
        auto rets = ast.params[1];
        enforce(rets.length, text(id, " ", *ast, " ", ast.parsed));
        auto idx  = rets.indexOf('(');
        ret       = parseType(rets[0..idx == -1 ? $ : idx]);
        fn.isMember = opts.self;
        fn.isStatic = ast.isStatic; // For C++/ObjC class members.
        checkForward(ret);
        // TODO might be on fn param type
        if (rets.lastIndexOf("__attribute__((stdcall))") != -1) opts.abi = FnType.ABI.StdCall;
        return &fn.def;
      }
      void child(ref Node child, Def* def) {
        switch (child.kind) with (Node.Kind) {
        case AsmLabelAttr:
          auto label = child.params[0];
          enforce(label.length > 1 && label[0] == '_');
          // TODO: fn.mangledName = label[1..$];
          break;
        case AllocSizeAttr: break;
        case AlwaysInlineAttr: break; // __forceinline
        case AvailabilityAttr: break;
        case DeprecatedAttr: break;
        case UnavailableAttr: break;
        case UnusedAttr: break;
        case PureAttr: break;
        case ConstAttr: break; // TODO
        case NoThrowAttr: break;
        case RestrictAttr: break;
        case FormatAttr: break;
        case FormatArgAttr: break;
        case NonNullAttr: break;
        case NoEscapeAttr: break;
        case ReturnsTwiceAttr: break;
        case SentinelAttr: break;
        case VisibilityAttr: break;
        case DLLImportAttr: break;
        case WarnUnusedResultAttr: break;
        case ObjCDesignatedInitializerAttr: break;
        case ObjCRequiresSuperAttr: break;
        case ObjCReturnsInnerPointerAttr: break;
        case SwiftErrorAttr: break;
        case SwiftNameAttr: break;
        case SwiftPrivateAttr: break;
        case IBActionAttr: break;
        case CFAuditedTransferAttr: break;
        case CFConsumedAttr: break;
        case CFReturnsRetainedAttr: break;
        case CFReturnsNotRetainedAttr: break;
        case NSConsumesSelfAttr: break;
        case NSReturnsRetainedAttr: break;
        case ParmVarDecl:
          Sym* sym;
          int type;
          auto xs = child.params;
          if (xs[1].empty) sym = sym_;
          else {
            sym  = Sym.intern(xs[0]);
            type = 1;
          }
          args.put(parseType(xs[type]));
          params.put(sym);
          break;
        case Variadic: break; // TODO
        case Overrides: break; // TODO
        case DeclRefExpr: break;
        case ParenExpr:
        case UnaryOperator:
        case CompoundStmt:
        case CXXBoolLiteralExpr:
        case CXXCtorInitializer:
        case ExprWithCleanups:
        case FloatingLiteral:
        case ImplicitCastExpr:
        case IntegerLiteral:
          skipChildren(id, child.indent);
          break;
        case ParagraphComment: // TODO more commenting
        case TextComment: break;
        case FullComment:
          enforce(child.params[0].empty && child.params[1].empty);
          doc = parseComment(child.indent, id);
          break;
        default:
          enforce(0, text("FN: ", id, " ", *node(id), " ", child.kind));
        }
      }
      auto loc = id;
      auto def = visit!false(id, &child, &create);
      auto fn  = cast(.Fn*)def;
      if (ret.native == Type.Native.Struct) {
        opts.structRetFix = true;
      }
      version (OSX) if (objc) {
        fn.isObjC = true;
        fn.external.sel = sel_getUid(fn.sym.name.toStringz);
        enforce(fn.external.sel, "Selector not found: " ~ fn.sym.name);
      }
      auto fnt = FnType.intern(ret, args.data, opts);
      foreach (ref arg; params.data) checkForward(arg.type);
      fn.params = params.data;
      fn.type   = cast(Type*)fnt;
      compile(fnt, objc, parent);
      if (parent is null) {
        vc.currentNS.define(fn.sym, Any(&fn.val));
      }
      copyMeta(loc, def);
      return def;
    }

    Def* analyzeNS(uint id) {
      auto ast = node(id);
      auto push = cxxNamespace;
      // writeln("NS: ", id, " ", ast.params[0]);
      Def* create() {
        // TODO can there be multiple NamespaceDecls of the same name?
        auto ns = new CXXNS(NS(Sym.intern(ast.params[0])));
        ns.parent = cxxNamespace;
        ns.prefix = nsPrefix(cxxNamespace ? cxxNamespace.prefix : "", ast.params[0]);
        cxxNamespaces[ns.sym.name] = ns;
        return &ns.def;
      }
      void enter(Def* def) {
        cxxNamespace = cast(CXXNS*)def;
      }
      void child(ref Node child, Def*) {
        if (child.kind == Node.Kind.Namespace) {
          enforce(child.params[0] == ast.params[0]);
          id++;
        }
        analyze(id);
        // TODO intern def in NS
        // skipChildren(id, child.indent); // TODO analyze take ref id
      }
      auto def = visit(id, &child, &create, &enter);
      assert(cxxNamespace is cast(CXXNS*)def);
      cxxNamespace = push;
      return def; // TODO list of locations where defined
    }

    /// Assigns a CIF matching the foreign function type signature.
    static void compile(FnType* fn, bool objC = false, Struct* parent = null) {
      if (fn.ffi) return;
      auto sz  = Fn.sizeof + size_t.sizeof * fn.totalArgs; //fnNumParams(fn, objC, parent);
      auto ffi = cast(Fn*)new ubyte[sz].ptr;
      doCompile(fn, ffi, objC, parent);
    }
    static void doCompile(FnType* fn, Fn* ffi, bool objC, Struct* parent = null) {
      auto num = fn.args.length.to!uint;
      auto ret = toFFI(fn.ret);
      auto arg = ffi.types.ptr;
      auto total = fn.totalArgs;
      auto extra = total - num;
      if (parent) {
        arg[0] = &ffi_type_pointer; // this
      } else version (OSX) if (objC) {
        arg[0] = &ffi_type_pointer; // id
        arg[1] = &ffi_type_pointer; // SEL
      }
      if (fn.structRetFix) {
        extra--;
        arg[total - 1] = &ffi_type_pointer; // ptr to stack struct
        ret = &ffi_type_void;
      }
      foreach (n; 0..num) {
        arg[n+extra] = toFFI(fn.args[n]);
      }
      auto abi = toFFI(fn.abi);
      auto rc = ffi_prep_cif(&ffi.cif, abi, total, ret, arg);
      enforce(rc == FFI_OK, prepError(rc));
      fn.ffi = ffi;
      assert(fn.ffi.cif.abi == abi);

      // auto rc = ffi_prep_cif_var(&ffi.cif, FFI_DEFAULT_ABI, num, x, ret, arg);
    }

    /// Handles ffi_prep_cif errors.
    static string prepError(ffi_status s) {
      switch (s) {
      case FFI_BAD_API:     return "bad api";
      case FFI_BAD_TYPEDEF: return "bad def";
      default: assert(0);
      }
    }

    static int toFFI(FnType.ABI abi) {
      final switch (abi) with (FnType.ABI) {
      case C:
        return FFI_DEFAULT_ABI;
      case StdCall:
        version (Windows) return FFI_STDCALL;
      case Vile:
      case D:
        assert(0);
      }
    }

    /// Convert a Vile type into a FFI type.
    static ffi_type* toFFI(Type* type) {
      final switch (type.native) with (Type.Native) {
      case Bool:
      case SInt8:   return &ffi_type_sint8;
      case SInt16:  return &ffi_type_sint16;
      case SInt32:  return &ffi_type_sint32;
      case SInt64:  return &ffi_type_sint64;
      case UInt8:   return &ffi_type_uint8;
      case UInt16:  return &ffi_type_uint16;
      case UInt32:  return &ffi_type_uint32;
      case UInt64:  return &ffi_type_uint64;
      case Float16: assert(0);
      case Float32: return &ffi_type_float;
      case Float64: return &ffi_type_double;
      case Float80: return &ffi_type_longdouble;
      case BigInt:  assert(0);
      case Char:    return &ffi_type_uchar;
      case Nil:
      case Array:
      case DynArray:
      case Fn:
      case NSObject:
      case Pointer: return &ffi_type_pointer;
      case Void:    return &ffi_type_void;
      case Alias:   return toFFI(type.next);
      case Type:
      case Meta:
      case Any:
      case Trait:
      case DObject:
      case Generic:
      case Temp: enforce(0, text(type.toString, " ", type.native)); assert(0);
      case Struct:
        auto s = cast(.Struct*)type;
        if (!s.ffi) {
          auto num = s.numDataFields;
          enforce(num, s.sym.name);
          s.ffi           = new ffi_type();
          s.ffi.size      = s.size;
          s.ffi.alignment = s.alignment.to!ushort;
          s.ffi.type      = FFI_TYPE_STRUCT;
          s.ffi.elements  = new ffi_type*[num + 1].ptr;
          auto n = 0;
          auto i = 0;
          .Struct.Member* m;
          while ((m = s.nextDataField(i)) != null) {
            s.ffi.elements[n++] = toFFI(m.type);
          }
          enforce(n == num, s.sym.name);
        }
        return s.ffi;
      case Enum:
        return &ffi_type_uint; // TODO enum actual type
      }
    }

    void copyMeta(uint id, Def* def) {
      auto ast   = node(id);
      def.file   = ast.loc.file;
      def.line   = ast.loc.line;
      def.column = ast.loc.col;
    }

    /// Reconstructs a rich documentation comment.
    string parseComment(uint indent, ref uint id) {
      // TODO: rich text
      Appender!string s;
      auto len = parser.lines.length;
      while (++id < len && node(id).indent > indent) {
        auto c = node(id);
        switch (c.kind) with (Node.Kind) {
        case InlineCommandComment:
        case VerbatimBlockLineComment:
        case VerbatimBlockComment:
        case VerbatimLineComment: // Text=<sym>
          // s.put(c.params[1]);
          break;
        case TextComment: // Text=<line>
          // s.put(c.params[1]);
          break;
        case ParamCommandComment:
        case ParagraphComment:
          // s.put("\n");
          break;
        case BlockCommandComment:
          // if (c.params[1] != "brief") {
            // s.put(c.params[1]);
            // s.put(":");
          // }
          break;
        case HTMLStartTagComment:
        case HTMLEndTagComment:
          auto next = id + 1;
          while (next < len && parser.text[next][0] != '|') {
            next++;
          }
          id = next - 1;
          break;
        default: writeln("COMMENT: ", c.kind);
        }
      }
      id--;
      return s.data;
    }

    /// Applies a type constructor to its argument. Returns null for no matches.
    Type* parseQualifiedType(string qualifier, string type, uint intOffset) {
      switch (qualifier[0]) {
      case '[':
        enforce(qualifier[$-1] == ']');
        if (qualifier.length == 2) return typeArray; // TODO DynArray
        auto next = parseType(type);
        auto end = qualifier.length - 1;
        while (true) {
          auto idx = qualifier.lastIndexOf('[', end);
          enforce(idx != -1);
          auto dim = qualifier[idx+1..end].to!uint;
          auto t = new Type(typeArray, Sym.intern(text(next.sym.name, qualifier)),
                            next.size * dim, next.alignment); // TODO proper static array type
          t.native = Type.Native.Array;
          t.next = next;
          if (idx == 0) return t;
          enforce(idx > 1 && qualifier[idx-1] == ']');
          next = t;
          end = idx - 1;
        }
      default:
      }

      switch (qualifier) {
      default: return null;
      // C builtin integer qualifiers; modify offset into `intLookup`.
      case "long":       return parseType(type, intOffset | 1);
      case "const unsigned":
      case "unsigned":   return parseType(type, intOffset | 2);
      case "signed":     return parseType(type, intOffset);
      // C89 volatile qualifier
      case "volatile":   return parseType(type); // TODO
      // C89 const qualifier
      case "const":      return parseType(type); // TODO const types
      // C99 restrict qualifier
      case "restrict":   return parseType(type); // TODO restrict types
      // Objective-C qualifiers
      case "_Nonnull":
      case "_Nullable":
      case "_Null_unspecified":
      case "__kindof":   return parseType(type); // TODO objc attributes
      // C implicit "namespaces"
      case "const struct": // TODO const types
      case "struct":
      case "union": return resolveNamed(type, CNS.Struct);
      case "enum":  return resolveNamed(type, CNS.Enum);
      }
    }

    /// Lookup table for 32/64-bit signed/unsigned integer types.
    __gshared static intLookup =
      [&typeSInt32, &typeSInt64, &typeUInt32, &typeUInt64];

    Type* cacheType(string s, Type* t) {
      if (auto p = s in typeLookup) {
        enforce(*p is t, s);
        return t;
      }
      typeLookup[s] = t;
      return t;
    }

    /// Parses the string representation of a type found in an AST node.
    Type* parseType(string s, uint intOffset = 0) {
      enforce(s.length, "empty type");
      if (auto p = s in typeLookup) return *p;
      // writeln("PARSE TYPE ", s);
      auto last = s.length - 1;
      switch (s[last]) {
      case ' ': return cacheType(s, parseType(s[0..last]));
      case '*': return cacheType(s, Type.pointer  (parseType(s[0..last])));
      case '&': return cacheType(s, Type.reference(parseType(s[0..last])));
      default:
      }
      // TODO char default = unsigned or signed? damnit C
      switch (s) {
      case "unsigned": intOffset |= 2; goto case;
      case "long":
      case "int":     return *intLookup[intOffset];
      case "short":   return intOffset & 2 ? typeSInt16  : typeUInt16;
      case "char":    return intOffset & 2 ? typeSInt8   : typeUInt8;
      case "double":  return intOffset & 1 ? typeFloat80 : typeFloat64;
      case "float":   return typeFloat32;
      case "wchar_t": return typeChar16; // TODO: get clang's width
      case "void":    return typeVoid;
      case "bool":
      case "_Bool":   return typeBool;
      case "...":     return typeVariadic;
      case "instancetype":
        version (OSX) return objcInstance.enforce("instancetype");
      default:
      }

      auto templateIdx = s.indexOf('<');
      if (templateIdx != -1 && s[$-1] == '>') {
        version (OSX) {
          switch (s[0..templateIdx]) {
          default: break;
          case "id": // generic
          case "Class": // TODO?
          case "NSObject": // specific
            auto protos = s[templateIdx+1..$-1];
            if (protos.indexOf(',') != -1) return typeVoid; // TODO multiple conforms
            return resolveNamed(protos, CNS.Protocol);
          }
        }
        auto t = parseType(s[0..templateIdx]);
        version (OSX) {
          if (t.native == Type.Native.NSObject /*&& (cast(Struct*)t).objcGeneric*/) {
            // TODO "tmpl" params can be protocols to conform to if type isnt generic
            return t; // TODO specialize
          }
        }
        enforce(t.native == Type.Native.Generic,
                text(s, " ", t.native, " ", t.toString));
        auto gen = cast(Generic*)t;
        auto name = s[templateIdx+1..$-1];
        auto inst = gen.lookupInstance(name);
        if (inst) return cacheType(s, inst);
        // TODO multiple parameters
        auto param = parseType(name).aliased;
        inst = gen.lookupInstance((cast(Def**)&param)[0..1]);
        enforce(inst, "Template instance not found: ", s);
        return cacheType(s, inst);
      }

      auto abi = FnType.ABI.C;

      // TODO param attributes?
      while (true) {
        enum attrDecl = " __attribute__((";
        auto attrIdx  = s.lastIndexOf(attrDecl);
        if (attrIdx == -1) break;
        enforce(s.endsWith("))"));
        auto attr = s[attrIdx+attrDecl.length..$-2];
        s = s[0..attrIdx];
        switch (attr) {
        default: enforce(0, text("Unknown attribute: ", attr)); assert(0);
        case "cdecl":   abi = FnType.ABI.C;       break;
        case "stdcall": abi = FnType.ABI.StdCall; break;
        }
      }

      // auto idx = s.lastIndexOf(' ', templateIdx == -1 ? s.length : templateIdx);
      auto idx = s.lastIndexOf(' ');
      if (idx != -1) {
        auto idxfn = s.indexOf('(');
        if (idxfn != -1) {
          enforce(s[s.length - 1] == ')', s);
          return parseFnType(parseType(s[0..idxfn++]), s, idxfn, abi);
        }
        while (++idx < s.length && s[idx] == '*') {}
        auto a = s[0..s[idx-1] == ' ' ? idx - 1 : idx];
        auto b = s[idx..$];
        if (auto t = parseQualifiedType(a, b, intOffset)) return cacheType(s, t);
        auto t = parseQualifiedType(b, a, intOffset);
        enforce(t, text("Unknown type qualifier: ", a, " | ", b));
        return cacheType(s, t);
      }
      return cacheType(s, lookupType(s, templateIdx));
    }

    Type* parseFnType(Type* ret, string s, ref ptrdiff_t idx, FnType.ABI abi) {
      // writeln("PARSE FN ", s[idx..$]);
      switch (s[idx++]) {
      case '^': break; // TODO objc block types
      case '*': break;
      default: enforce(0, s);
      }

      switch (s[idx]) {
      case '*':
        return Type.pointer(parseFnType(ret, s, idx, abi));
      case '(':
        idx++;
        ret = parseFnType(ret, s, idx, abi);
        break;
      default:
      }
      idx = s.indexOf(')', idx); // TODO attributes, ie _Nullable
      enforce(idx != -1);
      enforce(idx + 2 < s.length);
      enforce(s[idx++] == ')', s);
      enforce(s[idx++] == '(', s);
      Appender!(Type*[]) params;
      while (true) {
        auto endIdx = s.indexOf(", ", idx);
        auto fnIdx = s.indexOf('(', idx);
        if (endIdx == -1) endIdx = s.length - 1;
        ptrdiff_t nextIdx;
        if (fnIdx != -1 && fnIdx < endIdx) {
          switch (s[++fnIdx]) {
          case '^': break; // TODO objc block types
          case '*': break;
          default: enforce(0, s);
          }
          endIdx = findFnEnd(s, fnIdx);
          enforce(s[endIdx] == '(');
          endIdx = findFnEnd(s, endIdx + 1);
        }
        auto param = s[idx..endIdx];
        if (!param.empty && param != "void") params.put(parseType(param));
        switch (s[endIdx]) {
        default: enforce(0, s); assert(0);
        case ',': idx = endIdx + 2; break;
        case ')':
          idx = endIdx + 1;
          FnType.Opts opts;
          opts.abi = abi;
          return &FnType.intern(ret, params.data, opts).type;
        }
      }
    }

    ptrdiff_t findFnEnd(string s, ptrdiff_t idx) {
      auto level = 1;
      do {
        switch (s[++idx]) {
        case '(': level++; break;
        case ')': level--; break;
        default:
        }
      } while (level);
      return idx + 1;
    }

    /// Matches a named user-defined type. Created first if not found.
    Type* lookupType(string s, size_t templateIdx) {
      auto hash = s.murmurHash3; // TODO reuse
      // writeln("LOOKUP ", s);
      foreach (cns; 0..cast(int)CNS.N) {
        if (auto t = lookupType(s, cast(CNS)cns)) return t;
      }

      size_t start;
      if (templateIdx != -1) {
        auto depth = 1;
        start = templateIdx + 1;
        while (start < s.length) {
          auto ch = s[start++];
          if (ch == '<') depth++;
          else if (ch == '>' && --depth == 0) break;
        }
      }

      auto idx = s.indexOf("::", start);
      if (idx != -1) {
        auto t = parseType(s[0..idx]);
        idx += 2;
        while (true) {
          auto end = s.indexOf("::", idx);
          if (end == -1) end = s.length;
          if (s[end-1] == '>') return parseType(s);
          t = t.lookupType(Sym.intern(s[idx..end]));
          if (!t) {
            auto p = new Placeholder(Type(Type.Native.Temp));
            p.name = s;
            return &p.type;
          }
          if (end == s.length) break;
          idx = end + 2;
        }
        return t;
      }

      enforce(0, "Unknown type: " ~ s);
      assert(0);
    }

    Type* lookupType(string s, CNS cns) {
      if (auto p = s in decls[cns]) {
        auto link = &lookup[*p];
        Def* def;
        if (link.def) def = link.def;
        else def = analyze(link.id);
        enforce(def.type is typeType, def.type.toString);
        return cast(Type*)def;
      }
      return null;
    }

    static struct Placeholder {
      alias type this;
      Type type;
      string name;
    }

    void skipChildren(ref uint id, ubyte indent) {
      auto ls = parser.lines;
      while (++id < ls.length && ls[id].indent > indent) {}
      id--;
    }

    void skipAllChildren(ref uint id, ubyte parentIndent) {
      auto ls = parser.lines;
      auto indent = node(id).indent;
      while (++id < ls.length && ls[id].indent > indent) {}
      debug {
        auto ast = node(id);
        enforce(ast.indent <= parentIndent,
                format("Analyze line %s: unexpected child: %s", id, ast.kind));
      }
      id--;
    }

    void next(ref uint id) {
      debug auto ast = node(id);
      id++;
      debug enforce(node(id).indent > ast.indent,
                    format("Analyze line %s: expected child of %s", id, ast.kind));
    }

    void expect(ref uint id, Node.Kind[] kinds...) {
      id++;
      debug {
        auto ast = node(id);
        enforce(kinds.countUntil(ast.kind) != -1,
                format("Analyze line %s: got %s but expected one of %s",
                       id, ast.kind, kinds));
      }
    }
  }

  /// Parses the output of 'clang -cc1 -ast-dump'
  static struct Parser {
    string[] text;       /// Output split into lines, slices into `output`.
    Node  [] lines;      /// Parsed output information, slices into `text`.
    string   line;       /// Text slice of the line currently being parsed.
    Node*    current;    /// Line information currently being populated.
    int      lineOffset; /// Byte number, used to iterate `line`.

    // Clang outputs "packed" meta data; if it doesn't change don't repeat it.
    // We assume the top-level forms contain the main deltas for this.
    // If not, use the ParseMode.Full flag to parse all lines initially.
    string currentFile;
    int    currentLine;
    int    currentArg;

    void init(string[] output) {
      assert(output.length);
      text  = output;
      lines = new Node[text.length];
    }

    /// Consumes a string from the line, `ch` is consumed but not returned.
    string sliceUntil(dchar ch) {
      auto offset = lineOffset;
      while (lineOffset < line.length && line[lineOffset] != ch) lineOffset++;
      return line[offset..lineOffset++];
    }

    /// Consumes an integer value from the line. Used by `parseLoc`.
    uint parseInt() {
      auto start = lineOffset;
      while (lineOffset < line.length && line[lineOffset].isDigit) lineOffset++;
      return line[start..lineOffset].to!uint;
    }

    /// Consumes a location. "<s>:<n>:<n>", "file:<n>:<n>", "line:<n>" or "col".
    void parseLoc(ref Loc l) {
      string file;
      auto offset = lineOffset;
      while (lineOffset < line.length) {
        if (line[lineOffset] == ':') {
          auto end = lineOffset;
          lineOffset++;
          enforce(lineOffset < line.length);
          auto ch = line[lineOffset];
          if (ch != '\\' && ch != '/') { // Handle windows drive letters in paths.
            file = line[offset..end];
            break;
          }
        }
        lineOffset++;
      }
      parseLoc(l, file);
    }
    void parseLoc(ref Loc l, string file) {
      switch (file) {
      default:
        currentFile = file.replaceAll(pathPattern, "/"); // TODO here? most locs arent used.
        goto case;
      case "line":
        currentLine = parseInt();
        enforce(line[lineOffset++] == ':');
        goto case;
      case "col":
        l.col = parseInt();
      }
      l.file = currentFile;
      l.line = currentLine;
    }

    /// Handles exceptional values found in location representations.
    void parseLocSpecial(ref Loc l) {
      lineOffset++;
      auto text = sliceUntil('>');
      if (text != "invalid sloc") {
        switch (text) {
        case "built-in":
        case "scratch space":
          enforce(line[lineOffset++] == ':');
          parseLoc(l, text);
          break;
        case "<NULL":
          enforce(line[lineOffset++] == '>');
          break;
        default: enforce(0, text);
        }
      }
    }

    Addr parseAddr(string prefix) {
      if (prefix.length && !line[lineOffset..$].startsWith(prefix)) return 0;
      lineOffset += prefix.length;
      enforce(line[lineOffset++] == '0', line);
      enforce(line[lineOffset++] == 'x', line);
      return sliceUntil(' ').to!Addr(16);
    }

    /// Reads the basic information of an AST node. Always run on every line.
    void parsePartial() {
      lineOffset = 0; // Always start at the beginning of lines.
    prefix: while (true) {
        switch (line[lineOffset]) {
        case ' ':
        case '|':
        case '`':
        case '-': lineOffset++; break;
        default:  current.indent = lineOffset.to!byte; break prefix;
        }
      }
      if (line[lineOffset] == '*') return;
      // Try matching the entire line after the prefix.
      switch (line[lineOffset..$]) {
      case "<<<NULL>>>": current.kind = Node.Kind.Null;     return;
      case "...":        current.kind = Node.Kind.Variadic; return;
      default:
      }
      // AST node kind.
      auto name = sliceUntil(' '); // TODO: simplify
      switch (name) {
      case "getter":
        current.isGetter = true;
        goto case;
      case "array_filler:":
      case "super":
      case "origin":
      case "original": name = sliceUntil(' '); break;
      default:
      }
      try {
        current.kind = name.to!(Node.Kind);
      }
      catch (Throwable e) {
        switch (name) {
        case "public":     current.kind = Node.Kind._public;   break;
        case "Overrides:": current.kind = Node.Kind.Overrides; break;
        default: _errs = true; writeln(line);writeln(name); return;
        }
      }
      if (lineOffset == line.length) return;
      switch (current.kind) with (Node.Kind) {
      case _public:
      case CopyAssignment:
      case CopyConstructor:
      case CXXCtorInitializer:
      case DefaultConstructor:
      case DefinitionData:
      case Destructor:
      case MoveAssignment:
      case MoveConstructor:
      case Overrides:
      case TemplateArgument:
        break;
      default:
        // AST node addresses. Only used for analysis lookups.
        current.addr = parseAddr("");
        current.prev = parseAddr("prev ");
        current.base = parseAddr("parent ");
      }
      current.offset = (lineOffset - current.indent).to!ubyte;
      assert(current.indent + current.offset == lineOffset);
    }

    bool _errs; // HACK REMOVE

    /// Partially parses every line, fully parses top-level ones.
    void parseTopLevel(Linkage defaultLinkage) {
      assert(text.length);
      for (auto id = 0; id < text.length; id++) { // Fast partial pass
        line    = text[id];
        current = &lines[id];
        parsePartial();
      }
      if (_errs) exit(0);
      auto indent = 2;
      auto linkageIdx = 0;
      Linkage[32] linkageStack = void;
      linkageStack[0] = defaultLinkage;
      for (auto id = 0; id < text.length; id++) { // Full pass on top-level
        auto ast = &lines[id];
        if (ast.indent <= indent) {
          current    = ast;
          line       = text[id];
          lineOffset = ast.indent + ast.offset;
          parseLine();
          current.parsed = true;

          if (ast.kind == Node.Kind.LinkageSpecDecl) {
            indent += 2;
            linkageIdx++;
            Linkage linkage;
            switch (ast.params[0]) {
            case "C":   linkage = Linkage.C; break;
            case "C++": linkage = Linkage.CXX; break;
            default: enforce(0, .text("Unknown linkage: ", ast.params[0], " ", id));
            }
            linkageStack[linkageIdx] = linkage;
            continue;
          }

          if (ast.kind == Node.Kind.NamespaceDecl) {
            indent += 2;
            auto linkage = linkageStack[linkageIdx];
            linkageIdx++;
            linkageStack[linkageIdx] = linkage;
          }
          else if (ast.indent < indent && id != 0) {
            indent = ast.indent;
            linkageIdx--;
            assert(linkageIdx >= 0);
          }

          ast.linkage = linkageStack[linkageIdx];
        }
      }
    }

    /// Consumes all children lines of a specific line.
    uint parseLines(uint id, ubyte indent) {
      // writeln("PARSE ", id, " ", lines[id].parsed);
      if (lines[id].parsed) {
        debug {
          while (id < lines.length && lines[id].indent > indent)
            id++;
        }
        else return uint.max;
      }
      else while (id < lines.length) {
        current = &lines[id];
        line    = text[id];
        if (current.indent <= indent) break;
        lineOffset = current.indent + current.offset;
        parseLine();
        current.parsed = true;
        id++;
      }
      return id;
    }

    /// Consumes the current line.
    void parseLine() {
      // writeln("PARSE LINE ", lineOffset, " ", line.length, " ", line);
      if (lineOffset >= line.length) return;
      currentArg = 0;
      // Source range and symbol position.
      if (line[lineOffset] == '<') {
        // Range start.
        if (line[++lineOffset] == '<') parseLocSpecial(current.start);
        else parseLoc(current.start);

        // Range end.
        if (line[lineOffset] == ',') {
          lineOffset++;
          enforce(line[lineOffset++] == ' ');
          if (line[lineOffset] == '<') parseLocSpecial(current.end);
          else parseLoc(current.end);
        }
        enforce(line[lineOffset++] == '>');
        if (lineOffset == line.length) return;

        // This is tricky; the symbol position is optional and creates
        // an ambiguity with some AST node kinds. TODO clean up
        enforce(line[lineOffset++] == ' ');
        auto ch = line[lineOffset];
        if (ch == '<') parseLocSpecial(current.loc);
        else if (ch == '\'' || ch == '"' || ch == '[') goto params;
        else {
          size_t offset = lineOffset;
          auto filePos = line.indexOf(':', lineOffset); // file:line:col
          if (filePos != -1) {
            auto next = filePos + 1;
            if (next < line.length && (line[next] == '\\' || line[next] == '/')) { // windows drive letter
              filePos = line.indexOf(':', next + 1);
              if (filePos != -1) offset = filePos;
            }
          }
          auto wordPos = line.indexOf(' ', offset); // foo
          auto attrPos = line.indexOf('=', offset); // Prop=Value
          //writefln("FILE %s WORD %s ATTR %s LINE %s", filePos, wordPos, attrPos, line.length);
          if (wordPos == -1) wordPos = line.length;
          if (attrPos != -1 && attrPos < wordPos) { // No position but K/V pair.
            auto attrStart = attrPos + 1;
            enforce(line[attrStart] == '"');
            auto attrEnd = line.lastIndexOf('"');
            enforce(attrEnd != -1 && attrEnd != attrStart);
            current.params[0] = line[lineOffset..attrPos];
            current.params[1] = line[attrStart + 1 .. attrEnd];
            currentArg = 2;
            lineOffset = attrEnd.to!uint + 1;
          }
          else if (filePos == -1 || wordPos < filePos) { // Special node kinds.
            auto word = line[lineOffset..wordPos];
            // lineOffset = wordPos.to!uint + 1;
            switch (word) {
            case "macos":
            case "watchos":
            case "tvos":
            case "ios":
            case "swift":
              enforce(current.kind == Node.Kind.AvailabilityAttr);
              // TODO minimum os version
              return;
            default:
              switch (current.kind) with (Node.Kind) {
              default: enforce(0, line); assert(0);
              case MaxFieldAlignmentAttr:
                current.params[currentArg] = line[wordPos+1..$];
                return;
              case UuidAttr:
                current.params[currentArg] = line[wordPos+2..$-1];
                return;
              case NonNullAttr:
                lineOffset--;
                break;
              case AlignedAttr:
              case AllocSizeAttr:
              case AlwaysInlineAttr:
              case AvailabilityAttr:
              case ConstAttr:
              case EnumExtensibilityAttr:
              case FlagEnumAttr:
              case FormatAttr:
              case FormatArgAttr:
              case NoThrowAttr:
              case PureAttr:
              case RestrictAttr:
              case ReturnsTwiceAttr:
              case SentinelAttr:
              case UnavailableAttr:
              case UnusedAttr:
              case VisibilityAttr:
              case WarnUnusedResultAttr:
              case ObjCBridgeAttr:
              case ObjCBridgeMutableAttr:
              case ObjCReturnsInnerPointerAttr:
              case SwiftErrorAttr:
              case SwiftNameAttr:
              case SwiftNewtypeAttr:
              case CFAuditedTransferAttr:
              case NSErrorDomainAttr:
                return; // TODO
              }
            }
          }
          else { // Generic node, if we get here we have a symbol position!
            auto file  = line[lineOffset..filePos];
            lineOffset = filePos.to!uint + 1;
            parseLoc(current.loc, file);
          }
        }

        if (lineOffset == line.length) return;
        enforce(line[lineOffset++] == ' ', .text(lineOffset, " ", line.length, " ", line));
      }

    params: // Parse the remaining line as parameters of the AST node.
      string arg;
      while (lineOffset < line.length) {
        // Delimited parameters.
        switch (line[lineOffset]) {
        case '"': // String literal.
          lineOffset++;
          current.params[currentArg++] = sliceUntil('"'); // TODO: escape
          if (lineOffset == line.length) return;
          enforce(line[lineOffset++] == ' ');
          continue;
        case '\'': // Type name as 'sugar':'basic'. Keep the sugar type.
          lineOffset++;
          debug if (currentArg >= current.params.length)
            writeln("PARAMS OVERFLOW: ", line);
          current.params[currentArg++] = sliceUntil('\'');
          if (lineOffset < line.length) {
            if (line[lineOffset] == ':') {
              lineOffset++;
              enforce(line[lineOffset++] == '\'');
              sliceUntil('\'');
            }
            if (lineOffset == line.length) return;
            enforce(line[lineOffset++] == ' ');
          }
          continue;
        default:
        }

        // TODO depend on argument position! node kind!
        arg = sliceUntil(' ');
        switch (arg) {
        // Type
        case "extern": current.isExtern = true; continue;
        case "static": current.isStatic = true; continue;
        case "sugar":  current.isSugar  = true; continue;
        // case "dependent":
        case "instantiation_dependent":
        case "variably_modified":
        case "contains_unexpanded_pack":
        case "expansions":
        // case "alias":
          writeln("TODO:", arg, " >> ", line);
          return;

        // case "depth": current.depth = true; continue;
        // case "index": current.index = true; continue;

        case "underlying_type":
          writeln("TODO:", arg, " >> ", line);
          return;

        // enum decl
        // case "class":  current.declEnum = DeclEnum.Class;  continue;
        case "struct": current.declEnum = DeclEnum.Struct; continue;
        case "union":  current.declEnum = DeclEnum.Union;  continue;

        // record decl
        // case "definition": current.definition = true; continue;

        // function decl
        case "inline": current.inline = true; continue;
        case "virtual": current.virtual = true; continue;
        case "__module_private__": current.modpriv = true; continue;
        case "pure": current.isPure = true; continue;
        case "default": current.isDefault = true; continue;
        // case "_delete": current.isDelete_ = true; continue;
        // case "delete": current.isDelete = true; continue;
        case "trivial": current.trivial = true; continue;
        case "noexcept-unevaluated": current.uneval = true; continue;
        case "noexcept-uninstantiated": current.uninst = true; continue;

        // var decl
        case "mutable": current.mutable = true; continue;
        case "tls": current.tls = true; continue;
        case "tls_dynamic": current.tlsDynamic = true; continue;
        case "nrvo": current.nrvo = true; continue;
        case "constexpr": current.constexpr = true; continue;
        case "cinit": current.cinit = true; continue;
        case "callinit": current.callinit = true; continue;
        case "listinit": current.listinit = true; continue;

        // OpenMP
        case "combiner":
        case "initializer":
        case "omp_priv":
          writeln("TODO:", arg, " >> ", line);
          return;

        // C++
        // case "typename":
        // case "target": ParmVarDecl
        case "nominated":
        case "constructed":
          writeln("TODO:", arg, " >> ", line);
          return;

        // linkage spec decl
        // case "C":
        // case "C++":
        //   // TODO
        //   continue;

        // objc ivar decl
        case "none":
          writeln("TODO:", arg, " >> ", line);
          continue;
        case "private":   current.declProt = DeclProt.Private;   continue;
        case "protected": current.declProt = DeclProt.Protected; continue;
        case "public":    continue;
        case "package":   current.declProt = DeclProt.Package;   continue;

        // objc type param decl
        case "covariant":
        case "contravariant":
        case "bounded":
          // writeln("TODO:", arg, " >> ", line);
          continue;

        // objc protocol decl
        case "-": continue;
        case "+": current.isStatic = true; continue;

        // objc property decl
        // case "required":
        // case "optional":
        // case "readonly":
        // case "assign":
        // case "readwrite":
        // case "retain":
        // case "copy":
        // case "nonatomic":
        // case "atomic":
        // case "weak":
        // case "strong":
        // case "unsafe_unretained":
          // writeln("TODO:", arg, " >> ", line);
          // continue;

        // objc property impl decl
        // case "synthesize":
        // case "dynamic":
          // writeln("TODO:", arg, " >> ", line);
          // continue;

        // objc block decl
        // case "capture":
        case "byref":
        case "nested":
          writeln("TODO:", arg, " >> ", line);
          continue;

        // objc message
        // TODO

        // objc bool literal
        case "__objc_yes":
        case "__objc_no":
          writeln("TODO:", arg, " >> ", line);
          continue;

        // rvalue
        // case "written":
          // writeln("TODO:", arg, " >> ", line);
          // "at lvalue reference"
          // continue;

        // Array

        // Vector
        case "altivec":
          writeln("TODO:", arg, " >> ", line);
          // "pixel"
          // "bool"
          assert(0);
        case "neon":
          writeln("TODO:", arg, " >> ", line);
          // "poly"
          assert(0);

        // Function
        case "noreturn":
        case "produces_result":
          writeln("TODO:", arg, " >> ", line);
          continue;
        case "regparm":
          writeln("TODO:", arg, " >> ", line);
          return;

        // Proto
        case "const": current.isConst = true; continue;
        case "trailing_return":
        case "volatile":
        case "restrict":
          writeln("TODO:", arg, " >> ", line);
          continue;

        // Decl
        case "implicit":   current.implicit = true; continue;
        case "referenced": current.referenced = true; continue;
        case "used":       current.used = true; continue;
        case "imported":
          // continue;
          // case "in":
          // continue;
          // case "hidden":
          // continue;
          // continue;
        // case "invalid":
          // continue;
          // Template
        // case "null":
          // continue;
          // case "type": ParmVarDecl
          // return;
        case "decl":
          // assert(0);
        case "nullptr":
          // continue;
        case "integral":
          // assert(0);
        // case "template":
          // assert(0);
          // TODO: " expansion"
        // case "expr":
          // assert(0);
        case "pack":
          // assert(0);
          // Attrs
        case "Inherited":
        case "Implicit":
          // assert(0);
          // Lookups
        case "primary":
          // assert(0);
          writeln("TODO:", arg, " >> ", line);
          continue;
          // Exprs
        // case "prefix":
          // continue;
          // Comments
        case "RenderNormal":
          continue;
          // Ptrs
          // case "parent":
          // case "prev":
          // case "first":
          //   if (line[lineOffset] == '0') goto skipPtr;
          //   current.params[currentArg++] = arg;
          //   continue;
        default:
        }
        if (current.kind == Node.Kind.CXXStaticCastExpr) return; // TODO
        debug if (currentArg >= current.params.length)
          writeln("PARAMS OVERFLOW: ", line, " ", current.params);
        current.params[currentArg++] = arg;
      }
    }
  }
}


// Networking Utilities
// -----------------------------------------------------------------------------

/// Simple TCP Server allowing only one connection at once (TODO: for now.)
abstract class TcpServer : Socket {
  private {
    net.Address address;
    TcpConn     conn;
    Thread      listener;
    shared bool running;
  }

  this(net.Address address) {
    assert(address);
    this.address = address;

    super(net.AddressFamily.INET, net.SocketType.STREAM); // TODO support ipv6
  }

  ~this() { stop(); }

  string name() { return this.classinfo.name; }

  void start() {
    // writeln("Starting ", name, " on ", address); // TODO: log
    assert(address);
    bind(address);
    listen(0); // TODO configure backlog

    running.atomicStore = true;
    listener = new Thread(&run);
    listener.start();
    assert(0);
  }

  void stop() {
    // writeln("Stopping ", name, " on ", address); // TODO: log
    address = null;
    running.atomicStore = false;

    if (conn) {
      conn.stop();
      conn = null;
    }

    if (listener) {
      shutdown(net.SocketShutdown.RECEIVE);
      close();

      try listener.join();
      catch (Exception e) printThrowable(e, true);

      listener = null;
    }
  }

  protected void run() {
    try while (running.atomicLoad) {
      try conn = cast(TcpConn) accept();
      catch (net.SocketAcceptException e) break;

      // writeln("client connected");
      conn.server = this;
      guard!({ conn.run(); });
      conn.stop();
      conn = null;
      // writeln("client disconnected");
    }
    catch (Throwable e) {
      printThrowable(e, true);
    }
  }

  override TcpConn accepting() @trusted {
    try return newConnection();
    catch (Exception e) assert(0); // TODO: appropriate behaviour?
  }

  protected abstract TcpConn newConnection() pure nothrow;
}

/// Simple buffered connection with a reference to the TCP Server accepting it.
abstract class TcpConn : Socket {
  protected {
    TcpServer server; /// Owner of this connection.

    // Receiving
    char[1024] rxBuf; /// Receive buffer, move leftover to head after consuming.
    ushort     rxEnd; /// How much of the receive buffer has been accumulated.
    ushort     rxPos; /// Dumb optimization for calls to indexOf over new data.

    // Transmitting
    uint       txSeq; /// Request sequence number, used to lookup responses.
  }

  void stop() {
    shutdown(net.SocketShutdown.BOTH);
    close();
  }

  protected void run() {
    assert(server);
    while (server.running.atomicLoad) {
      auto len = receive(rxBuf);
      if (len <= 0) break; // Closed/error condition

      rxEnd += len;
      assert(rxEnd <= rxBuf.length); // TODO: why hit? when client disc?
      while (rxPos < rxEnd) onData();
    }
  }

  protected void consumeRemaining(ushort end) {
    auto prevEnd = rxEnd;
    rxPos  = 0;
    rxEnd -= end;

    if (end == prevEnd) return;

    assert(end < prevEnd);
    memmove(rxBuf.ptr, rxBuf.ptr + end, prevEnd - end);
  }

  protected abstract void onData();
}


// Language Server Protocol
// -----------------------------------------------------------------------------

/// A client connected to the Language Server Protocol server.
/// Handles the protocol framing and dispatches messages and errors.
class LangConn : TcpConn {
  private {
    Appender!(char[]) content; /// Used to receive bodies larger than rxBuf.
    int    contentLength = -1; /// Number of bytes in the next message body.
    bool   inContent;          /// Whether receiving content or headers.
    State  state;              /// Current initialization state of the client.
    string rootUri;            /// Path of the current workspace.

    PendingRequest[] pending;  /// Storage for in-flight requests.
    TextDocument.Set docs;     /// The set of opened documents on this client.
    Capabilities     caps;     /// What the client told us it can do.
  }

  enum State : ubyte {
    Disconnected, /// Has disconnected from the server.
    Connected,    /// Is connected, waiting on 'initialize'.
    Initialized,  /// Has handled 'initialize'.
    Ready         /// Has received 'initialized'.
  }

  alias ResponseDg = void delegate(Response* res);

  struct PendingRequest {
    ResponseDg dg;
    MonoTime   time;
    uint       id;
  }

  struct Response {
    uint      id;
    bool      hasError; /// True if "error" was present in the response.
    JSONValue result;
    Error     error;

    static struct Error {
      uint      code;
      string    msg;
      JSONValue data;
    }
  }

  this() pure nothrow {
    pending = new PendingRequest[16]; // TODO: config limit?
    docs    = new TextDocument.Set();
  }

  override void onData() { inContent ? receiveContent() : receiveHeaders(); }

  PendingRequest* findRequest(uint id = 0) {
    foreach (ref req; pending)
      if (req.id == id)
        return &req;
    return null;
  }

private:
  void receiveHeaders() {
    // Wait until a full header line is received.
    auto idx = rxBuf[rxPos..rxEnd].indexOf(LSP.separator);
    if (idx == -1) {
      enforce(rxEnd < rxBuf.length, "LangConn rxBuf overflow");
      rxPos = rxEnd;
      return;
    }
    idx += rxPos;

    // An empty header line separates the content.
    if (idx == 0) {
      enforce(contentLength > 0, "Expecting content length");
      inContent = true;
      consumeRemaining(LSP.separator.length);
      return;
    }

    // Header line: <Header> ': ' <Value> '\r\n'
    auto sep = rxBuf[0..idx].indexOf(LSP.headerSep);
    enforce(sep > 0, "Expecting header separator");

    auto k = rxBuf[0..sep];
    auto v = rxBuf[sep + LSP.headerSep.length..idx];

    // Header dispatch
    switch (k) {
    case "Content-Length":
      contentLength = v.to!ushort;
      break;
    case "Content-Type":
      enforce(v == LSP.contentType, "Unexpected type: " ~ v);
      break;
    default:
      enforce(0, "Unexpected header: " ~ k);
    }

    consumeRemaining((idx + LSP.separator.length).to!ushort);
  }

  void receiveContent() {
    ushort end;
    char[] v;

    // Content is larger than the receive buffer
    if (contentLength > rxBuf.length) {
      end = min(rxEnd, contentLength - content.data.length).to!ushort;
      content.put(rxBuf[0..end]);
      consumeRemaining(end);

      if (content.data.length < contentLength) return;

      v = content.data;
      content.clear();
    }
    // Content fits in the receive buffer
    else {
      if (rxEnd < contentLength) return;

      v = rxBuf[0..contentLength];
      consumeRemaining(contentLength.to!ushort);
    }

    inContent = false;
    onMsg(v);
  }

  void onMsg(in char[] data) nothrow {
    JSONValue id;
    try {
      auto start = MonoTime.currTime;
      auto msg   = data.parseJSON;
      enforce(msg["jsonrpc"].str == "2.0");

      if (auto p = "id" in msg) {
        auto t = p.type;
        enforce(t == JSONType.integer || t == JSONType.string || t == JSONType.null_);
        id = *p;
      }

      // writeln("Msg ", msg);
      onMsg(id, msg);
      // writeln("Msg handled in ", MonoTime.currTime - start);
    }
    catch (Exception e) {
      printThrowable(e, true); // TODO: config
      try onMsgError(e, id);
      catch (Exception e2) printThrowable(e2, true);
    }
  }

  void onMsg(JSONValue id, JSONValue msg) {
    // Notification or Request
    if (auto msgMethod = "method" in msg) {
      auto method  = msgMethod.str;
      auto handler = method in LangServer.handlers;
      if (!handler) {
        onMsgError(id, LSP.ErrorCode.MethodNotFound, method);
        return;
      }

      JSONValue params;
      if (auto msgParams = "params" in msg) {
        if (!msgParams.isNull) {
          auto t = msgParams.type;
          enforce(t == JSONType.array || t == JSONType.object);
          params = *msgParams;
        }
      }

      // writeln("DISPATCH ", method);
      auto r = LangServer.Req(id, params, method, this);
      (*handler)(r);
    }
    // Response
    else {
      auto r = Response(id.integer.to!uint);
      auto c = findRequest(r.id);
      if (!c) return; // No request matching response?

      auto dg = c.dg;
      c.id = 0;
      c.dg = null;

      if (auto p = "result" in msg) r.result = *p;
      if (auto p = "error"  in msg) {
        auto err = *p;
        r.error.code = err["code"].integer.to!uint;
        r.error.msg  = err["message"].str;

        if (auto data = "data" in err) r.error.data = *data;
      }

      c.dg(&r);
    }
  }

  void onMsgError(JSONValue reqId, LSP.ErrorCode code, string msg) {
    if (!reqId.isNull)
      langServer.failure(reqId, code, msg, JSONValue(null));
  }
  void onMsgError(Throwable e, JSONValue reqId) {
    auto msg = e.toString;
    if (!reqId.isNull) onMsgError(reqId, LSP.ErrorCode.InternalError, msg);
    else langServer.showMessage(LangServer.MessageType.Error, msg);
  }

  private unittest {
    scope auto c = new LangConn();
    assert(!c.inContent);

    void buf(string s) {
      assert(s.length);
      auto end = c.rxEnd + s.length;
      if (end <= c.rxBuf.length) {
        c.rxBuf[c.rxEnd..end] = s;
        c.rxEnd += s.length.to!ushort;
        c.onData();
      }
      else {
        while (s.length) {
          auto x = min(c.rxBuf.length, end);
          auto e = x - c.rxEnd;
          c.rxBuf[c.rxEnd..x] = s[0..e];
          c.rxEnd += e;
          c.onData();
          s = s[e..$];
          end -= e;
        }
      }
    }

    auto content = q{{"jsonrpc": "2.0", "id": 1, "stuff": "hello world"}};

    // Not enough data, header
    buf = "Content-Length:";
    assert(c.rxPos == c.rxEnd);

    // Parse header, no data left
    buf = text(' ', content.length, LSP.separator);
    assert(c.rxPos == 0);
    assert(c.rxEnd == 0);
    assert(c.contentLength == content.length);

    // Parse header, remaining data
    buf = "Content-Type: " ~ LSP.contentType ~ LSP.separator ~ LSP.separator;
    assert(c.rxPos == 0);
    assert(c.rxEnd == LSP.separator.length);

    // Remaining data, end of header
    c.onData();
    assert(c.inContent);

    // Parse content
    buf = content;
    assert(c.rxPos == 0);
    assert(c.rxEnd == 0);
    assert(!c.inContent);
  }

  static struct Capabilities {
    LSP.SymbolKind[] symbolKind_valueSet;
    // MarkupKind[] completionItem_documentationFormat;
    // CompletionItemKind[] completionItemKind_valueSet;
    // MarkupKind[] hover_contentFormat;
    // MarkupKind[] signatureInformation_documentationFormat;
    LSP.SymbolKind[] documentSymbol_valueSet;
    // CodeActionKind[] codeActionKind_valueSet;

    bool applyEdit;
    bool workspaceEdit_documentChanges;
    bool didChangeConfiguration;
    bool didChangeWatchedFiles;
    bool symbol;
    bool executeCommand;
    bool workspaceFolders;
    bool configuration;

    bool synchronization;
    bool synchronization_willSave;
    bool synchronization_willSaveWaitUntil;
    bool synchronization_didSave;
    bool completion;
    bool completion_contextSupport;
    bool completionItem_snippetSupport;
    bool completionItem_commitCharacterSupport;
    bool completionItem_deprecatedSupport;
    bool completionItem_preselectSupport;
    bool hover;
    bool signatureHelp;
    bool references;
    bool documentHighlight;
    bool documentSymbol;
    bool formatting;
    bool rangeFormatting;
    bool onTypeFormatting;
    bool definition;
    bool typeDefinition;
    bool implementation;
    bool codeAction;
    bool codeLens;
    bool documentLink;
    bool colorProvider;
    bool rename;
    bool publishDiagnostics;

    void opAssign(JSONValue json) {
      auto workCaps = "workspace"    in json;
      auto textCaps = "textDocument" in json;
      auto alpha    = "experimental" in json;

      if (workCaps) {
        json = *workCaps;

        applyEdit        = json.isTrue1("applyEdit");
        workspaceFolders = json.isTrue1("workspaceFolders");
        configuration    = json.isTrue1("configuration");

        workspaceEdit_documentChanges =
          json.isTrue2("workspaceEdit", "documentChanges");

        didChangeConfiguration = json.isTrue2("didChangeConfiguration");
        didChangeWatchedFiles  = json.isTrue2("didChangeWatchedFiles");
        executeCommand         = json.isTrue2("executeCommand");

        if (auto p = "symbol" in json) {
          symbol = (*p).isTrue1();

          if (auto symbolKind = "symbolKind" in *p) {
            if (auto valueSet = "valueSet" in *symbolKind) {
              // TODO
            }
          }
        }
      }

      if (textCaps) {
        json = *textCaps;
        references         = json.isTrue2("references");
        documentHighlight  = json.isTrue2("documentHighlight");
        formatting         = json.isTrue2("formatting");
        rangeFormatting    = json.isTrue2("rangeFormatting");
        onTypeFormatting   = json.isTrue2("onTypeFormatting");
        definition         = json.isTrue2("definition");
        typeDefinition     = json.isTrue2("typeDefinition");
        implementation     = json.isTrue2("implementation");
        codeLens           = json.isTrue2("codeLens");
        documentLink       = json.isTrue2("documentLink");
        colorProvider      = json.isTrue2("colorProvider");
        rename             = json.isTrue2("rename");
        publishDiagnostics = json.isTrue2("publishDiagnostics");

        if (auto p = "synchronization" in json) {
          synchronization          = (*p).isTrue1;
          synchronization_didSave  = (*p).isTrue1("didSave");
          synchronization_willSave = (*p).isTrue1("willSave");
          synchronization_willSaveWaitUntil =
            (*p).isTrue1("willSaveWaitUntil");
        }

        if (auto p = "completion" in json) {
          completion                = (*p).isTrue1;
          completion_contextSupport = (*p).isTrue1("contextSupport");

          if (auto pitem = "completionItem" in *p) {
            auto item = *pitem;
            completionItem_snippetSupport =
              item.isTrue1("snippetSupport");
            completionItem_commitCharacterSupport =
              item.isTrue1("commitCharacterSupport");
            completionItem_deprecatedSupport =
              item.isTrue1("deprecatedSupport");
            completionItem_preselectSupport =
              item.isTrue1("preselectSupport");

            if (auto kind = "documentationFormat" in item) {
              // TODO MarkupKind[]
            }
          }

          if (auto pkind = "completionItemKind" in *p) {
            if (auto set = "valueSet" in *pkind) {
              // TODO CompletionItemKind[]
            }
          }
        }

        if (auto p = "hover" in json) {
          hover = (*p).isTrue1;

          if (auto kind = "contentFormat" in *p) {
            // TODO: MarkupKind[]
          }
        }

        if (auto p = "signatureHelp" in json) {
          signatureHelp = (*p).isTrue1;

          if (auto info = "signatureInformation" in *p) {
            if (auto kind = "documentationFormat" in *info) {
              // TODO: MarkupKind
            }
          }
        }

        auto sym = "documentSymbol" in json;
        if (!sym) sym = "symbol" in json; // TODO: older? sent by lsp-mode
        if (sym) {
          documentSymbol = (*sym).isTrue1;

          if (auto kind = "symbolKind" in *sym) {
            if (auto set = "valueSet" in *kind) {
              // TODO: SymbolKind[]
            }
          }
        }

        if (auto p = "codeAction" in json) {
          codeAction = (*p).isTrue1;

          if (auto lit = "codeActionLiteralSupport" in *p) {
            if (auto kind = "codeActionKind" in *lit) {
              if (auto set = "valueSet" in *kind) {
                // TODO: CodeActionKind[]
              }
            }
          }
        }
      }

      if (alpha) {
        // TODO
      }
    }
  }
}

/// Language Server for a single connection. (TODO: for now)
class LangServer : TcpServer {
  private {
    TextDocument.Map docs; /// All documents currently opened.
  }

  this() { super(new net.InternetAddress("127.0.0.1", 10420)); }

  override string name() { return "Language Server"; }

  override TcpConn newConnection() {
    auto c = new LangConn;
    c.state = LangConn.State.Connected;
    return c;
  }

  /// A request from a client, lives on the stack.
  static struct Req {
    JSONValue id;     /// Request identifier, null for notifications
    JSONValue params; /// Call parameters; null, array or object
    string    method; /// Call function in 'handlers' with this name
    LangConn  conn;   /// Client sending the message, must respond to

    /// The language server associated with the request.
    LangServer server() { return cast(LangServer) conn.server; }

    void success(JSONValue result) { langServer.success(id, result); }

    // void failure()
  }

  // TODO: decouple msg-building (conn-independent) from sending (conn)
  void send(JSONValue msg) {
    with (LSP) {
      msg["jsonrpc"] = "2.0";
      auto t = msg.toString;
      auto s = text("Content-Length", headerSep, t.length,    separator,
                    "Content-Type",   headerSep, contentType, separator,
                    separator,
                    t);
      // writeln("SEND ", t);
      conn.send(s);
    }
  }

  void notify(string method, JSONValue params) {
    JSONValue msg;
    if (!params.isNull) msg["params"] = params;
    notify(msg, method);
  }
  void notify(JSONValue msg, string method) {
    msg["method"] = method;
    send(msg);
  }

  void request(string method, JSONValue params, LangConn.ResponseDg dg) {
    JSONValue msg;
    if (!params.isNull) msg["params"] = params;
    request(msg, method, dg);
  }
  void request(JSONValue msg, string method, LangConn.ResponseDg dg) {
    auto slot = (cast(LangConn)conn).findRequest;
    assert(slot);
    assert(dg.ptr && dg.funcptr);
    slot.dg = dg;
    slot.id = ++conn.txSeq;

    msg["id"]     = slot.id;
    msg["method"] = method;
    send(msg);

    slot.time = MonoTime.currTime;
  }

  void respond(JSONValue msg, JSONValue reqId) {
    msg["id"] = reqId;
    send(msg);
  }
  void success(JSONValue reqId, JSONValue result) {
    JSONValue msg;
    msg["result"] = result;
    respond(msg, reqId);
  }
  void failure(JSONValue reqId, int code, string message, JSONValue data) {
    JSONValue error;
    error["code"]    = code;
    error["message"] = message;
    if (!data.isNull) error["data"] = data;

    JSONValue msg;
    msg["error"] = error;
    respond(msg, reqId);
  }

  private {
    __gshared JSONValue caps; /// Capabilities sent to initializing clients
  }

  shared static this() {
    JSONValue saveOptions;
    saveOptions["includeText"] = true;

    enum TextDocumentSyncKind {
      None = 0,
      Full = 1,
      Incremental = 2
    }

    JSONValue sync;
    sync["openClose"]         = true;
    sync["change"]            = TextDocumentSyncKind.Incremental;
    sync["willSave"]          = true;
    sync["willSaveWaitUntil"] = true;
    sync["save"]              = saveOptions;

    JSONValue completion;
    completion["resolveProvider"]   = true;
    completion["triggerCharacters"] = ["\t"];

    JSONValue help;
    help["triggerCharacters"] = ["."];

    JSONValue lens;
    lens["resolveProvider"] = true;

    JSONValue onType;
    onType["firstTriggerCharacter"] = ")";
    onType["moreTriggerCharacter"]  = ["]", "}"];

    JSONValue link;
    link["resolveProvider"] = true;

    JSONValue exec;
    exec["commands"] = ["test"];

    JSONValue color;
    color["id"]               = "$color"; // TODO
    color["documentSelector"] = null;

    JSONValue type;
    color["id"]               = "$type"; // TODO
    color["documentSelector"] = null;

    JSONValue impl;
    color["id"]               = "$impl"; // TODO
    color["documentSelector"] = null;

    JSONValue workFolders;
    workFolders["supported"]           = true;
    workFolders["changeNotifications"] = true; // TODO: id

    JSONValue work;
    work["workspaceFolders"] = workFolders;

    JSONValue repl;
    repl["eval"] = true;

    caps["textDocumentSync"]                 = sync;
    caps["hoverProperties"]                  = true;
    caps["completionProvider"]               = completion;
    caps["signatureHelpProvider"]            = help;
    caps["definitionProvider"]               = true;
    caps["typeDefinitionProvider"]           = true; //type;
    caps["implementationProvider"]           = true; //impl;
    caps["referencesProvider"]               = true;
    caps["documentHighlightProvider"]        = true;
    caps["documentSymbolProvider"]           = true;
    caps["workspaceSymbolProvider"]          = true;
    caps["codeActionProvider"]               = true;
    caps["codeLensProvider"]                 = lens;
    caps["documentFormattingProvider"]       = true;
    caps["documentRangeFormattingProvider"]  = true;
    caps["documentOnTypeFormattingProvider"] = onType;
    caps["renameProvider"]                   = true;
    caps["documentLinkProvider"]             = link;
    caps["colorProvider"]                    = true; //color;
    caps["executeCommandProvider"]           = exec;
    caps["workspace"]                        = work;
    caps["repl"]                             = repl;
    // caps["experimental"] =
  }

  // General
  // ---------------------------------------------------------------------------

  /**
   * Request: First message sent from a client to setup the connection.
   *
   * No other message can be sent or received until this handler responds!
   * The one exception is the 'exit' notification to allow disconnection.
   */
  static void onInitialize(ref Req r) {
    if (r.conn.state != LangConn.State.Connected) {
      JSONValue d;
      d["retry"] = false;
      langServer.failure(r.id, LSP.ErrorCode.InvalidRequest, "Initialized", d);
      return;
    }

    try doInitialize(r);
    catch (Exception e) {
      printThrowable(e, true);
      JSONValue d;
      d["retry"] = true;
      langServer.failure(r.id, LSP.ErrorCode.InternalError, e.toString, d);
    }
  }
  private static void doInitialize(ref Req r) {
    // Parent process ID. When non-null check if we belong to that process.
    auto json = r.params["processId"];
    auto pid  = json.isNull ? -1 : json.integer.to!int;
    // TODO: actually do something, want lsp-mode to send null (reboot in dev)

    // Workspace path.
    json = r.params["rootUri"];
    if (!json.isNull) r.conn.rootUri = json.str;
    else if (auto old = "rootPath" in r.params) {
      json = *old;
      if (!json.isNull) r.conn.rootUri = json.str;
      writeln("WARNING: deprecated rootPath, use rootUri instead");
    }

    // Vile specific options from the client. (?)
    if (auto opts = "initializationOptions" in r.params) {
      if (!opts.isNull) {
        // TODO
      }
    }

    // Client capabilities.
    r.conn.caps = r.params["capabilities"];

    if (auto ptrace = "trace" in r.params) {
      auto trace = ptrace.str;
      switch (trace) {
      case "off":
      case "messages":
      case "verbose":
        // TODO
        break;
      default:
        enforce(0, "Invalid trace value: " ~ trace);
      }
    }

    if (auto folders = "workspaceFolders" in r.params) {
      json = *folders;
      if (!json.isNull) {
        // TODO WorkspaceFolder[]
      }
    }

    // Success!
    JSONValue result;
    result["capabilities"] = caps;
    r.success(result);
    r.conn.state = LangConn.State.Initialized;

    // TODO: can we fail? what does it mean for our client to retry?
    // JSONValue data;
    // data["retry"] = false;
    // failure(reqId, 0, "Oh no!", data);

    // TODO: all v2 LSP messages are mandatory! adjust caps?
  }

  /// Notify: Sent directly after receiving the 'initialize' result.
  static void onInitialized(ref Req r) {
    if (r.conn.state == LangConn.State.Initialized)
      r.conn.state = LangConn.State.Ready;
  }

  /// Request: Prepare for exit, returning null or errors.
  static void onShutdown(ref Req r) {
    // TODO: destroy resources associated with client

    r.success(JSONValue(null));
  }

  /// Notify: Terminate the calling client.
  static void onExit(ref Req r) {
    r.conn.state = LangConn.State.Disconnected;
    r.conn.stop();

    // TODO: exit process? (if daemon, exit on last conn closed)
    // exit(EXIT_SUCCESS);
  }

  /// Notify: Cancel a request made to the client.
  void cancelRequest(JSONValue reqId) {
    // TODO: find request

    JSONValue v;
    v["id"] = reqId;

    notify("$/cancelRequest", v);
    assert(0, "TODO");
  }

  /// Notify: Cancel a request from the client.
  static void onCancelRequest(ref Req r) {
    // TODO: find request
    auto reqId = r.params["id"];
    switch (reqId.type) with (JSONType) {
    case integer: break;
    case string:  break;
    default: enforce(0, "Unexpected id: " ~ reqId.toString);
    }
    assert(0, "TODO");
  }

  // Window
  // ---------------------------------------------------------------------------

  enum MessageType {
    Error   = 1,
    Warning = 2,
    Info    = 3,
    Log     = 4
  }

  static JSONValue jsonMessage(MessageType type, string message) {
    JSONValue msg;
    msg["type"]    = type;
    msg["message"] = message;
    return msg;
  }

  /// Notify: Ask the client to display a message in the user interface.
  void showMessage(MessageType type, string message) {
    notify("window/showMessage", jsonMessage(type, message));
  }
  /// Request: Ask the client to display a message in the user interface.
  /// Also allows to pass actions and to wait for an answer from the client.
  void showMessage(MessageType type, string message, string[] actions) {
    auto params = jsonMessage(type, message);
    if (actions.length) {
      auto values = new JSONValue[actions.length];
      foreach (i, v; actions) values[i]["title"] = v;
      params["actions"] = values;
    }
    request("window/showMessageRequest", params, (res) {
        // TODO
      });
  }

  /// Notify: Ask the client to log a message.
  void logMessage(MessageType type, string message) {
    notify("window/logMessage", jsonMessage(type, message));
  }

  // Telemetry
  // ---------------------------------------------------------------------------

  /// Notify: Ask the client to log a telemetry event.
  void telemetry(JSONValue params) { notify("telemetry/event", params); }

  // Client
  // ---------------------------------------------------------------------------

  void registerCapability(JSONValue registrations) {
    JSONValue params;
    params["registrations"] = registrations;
    request("client/registerCapability", params, (res) {
        // TODO
      });
  }

  void unregisterCapability(JSONValue unregistrations) {
    JSONValue params;
    params["unregistrations"] = unregistrations;
    request("client/unregisterCapability", params, (res) {
        // TODO
      });
  }

  // Workspace
  // ---------------------------------------------------------------------------

  void workspaceFolders() {
    request("workspace/workspaceFolders", JSONValue(null), (res) {
        // TODO
      });
  }

  static void onDidChangeWorkspaceFolders(ref Req r) {
    auto e = r.params["event"];

    foreach (j; e["added"].array) {
      auto f = LSP.WorkspaceFolder(j);
      // TODO
    }

    foreach (j; e["removed"].array) {
      auto f = LSP.WorkspaceFolder(j);
      // TODO
    }
    writeln("didChangeWorkspaceFolders");
  }

  static void onDidChangeConfiguration(ref Req r) {
    auto s = r.params["settings"];
    // TODO
    writeln("didChangeConfiguration");
  }

  void configuration(JSONValue items) {
    JSONValue params;
    params["items"] = items;
    request("workspace/configuration", params, (res) {
        // TODO
      });
  }

  enum FileChangeType {
    Created = 1,
    Changed = 2,
    Deleted = 3
  }

  static void onDidChangeWatchedFiles(ref Req r) {
    auto changes = r.params["changes"];
    foreach (j; changes.array) {
      auto uri  = j["uri"] .str;
      auto type = j["type"].uinteger.to!FileChangeType;
      // TODO
    }
    writeln("didChangeWatchedFiles");
  }

  static void onSymbol(ref Req r) {
    auto q = r.params["query"];
    // TODO
    writeln("symbol");
    r.success(JSONValue(null)); // or SymbolInformation[]
  }

  static void onExecuteCommand(ref Req r) {
    auto cmd = r.params["command"];
    if (auto args = "arguments" in r.params) {

    }

    // TODO
    writeln("executeCommand");

    JSONValue result;
    r.success(result);
  }

  void applyEdit(string label, ref LSP.WorkspaceEdit edit) {
    JSONValue params;
    if (label.length) params["label"] = label;
    // params["edit"] = edit; TODO
    request("workspace/applyEdit", params, (res) {

      });
  }
  void onApplyEditResponse() {
    //v["applied"].isTrue TODO
  }

  /// Looks up a document using the TextDocumentIdentifier request parameter.
  static TextDocument findDoc(ref Req r) {
    return findDoc(r.params["textDocument"]);
  }
  /// ditto
  static TextDocument findDoc(JSONValue doc) {
    auto uri  = doc["uri"].str;
    auto hash = murmurHash3(uri);
    if (auto p = hash in langServer.docs) return *p;
    enforce(0, "TextDocument not found: " ~ uri);
    return null;
  }

  /// Notify: Client just opened a text document.
  static void onDidOpen(ref Req r) {
    auto json = r.params["textDocument"];
    auto uri  = json["uri"].str;
    auto hash = murmurHash3(uri);

    TextDocument doc;
    if (auto p = hash in langServer.docs) {
      doc = *p;
      enforce(0, "TODO: doc sharing");
    }
    else {
      writeln("open ", uri);
      doc = new TextDocument(uri, hash, json);
      langServer.docs[hash] = doc;
    }

    r.conn.docs.insert(doc);
  }

  /// Notify:
  static void onDidClose(ref Req r) {
    auto doc = findDoc(r);
    r.conn.docs.removeKey(doc);
    // TODO: remove client from doc
    langServer.docs.remove(doc.hash);
    doc.destroy();
    writeln("close ", doc.uri);
  }

  /// Notify:
  static void onDidChange(ref Req r) {
    auto obj = r.params["textDocument"];
    auto doc = findDoc(obj);
    auto ver = obj["version"].integer;

    auto changes = r.params["contentChanges"];
    if (changes.type == JSONType.null_) return;
    enforce(changes.type == JSONType.array, changes.type.to!string);
    foreach (change; changes.array) {
      auto range = LSP.Range(change["range"]);
      auto len   = change["rangeLength"].integer;
      auto text  = change["text"].str;
      // TODO: handle change !!
      writeln("change ", text);
    }
  }

  enum TextDocumentSaveReason {
    Manual     = 1,
    AfterDelay = 2,
    FocusOut   = 3
  }

  static void onWillSave         (ref Req r) { onWillSave(r, false); }
  static void onWillSaveWaitUntil(ref Req r) { onWillSave(r, true);  }
  static void onWillSave(ref Req r, bool isWaitUntil) {
    auto doc = findDoc(r);
    auto why = r.params["reason"].integer.to!TextDocumentSaveReason;
    // TODO: handle

    if (isWaitUntil) {
      r.success(JSONValue(null)); // TODO: TextEdit[]
    }
  }

  /// Notify: Document was saved in the client.
  static void onDidSave(ref Req r) {
    auto doc = findDoc(r);
    if (auto p = "text" in r.params) {
      // TODO: needed?
    }
    // TODO: what?
  }

  // Diagnostics
  // ---------------------------------------------------------------------------

  void publishDiagnostics(LSP.DocumentUri uri, LSP.Diagnostic[] diags) {
    auto diagnostics = new JSONValue[diags.length];
    foreach (i, ref d; diags) diagnostics[i] = d.json;

    JSONValue params;
    params["uri"] = uri;
    params["diagnostics"] = diagnostics;
    notify("textDocument/publishDiagnostics", params);
  }

  // Language Features
  // ---------------------------------------------------------------------------

  static void onCompletion(ref Req r) {
    auto doc = findDoc(r); // TODO: position
    // TODO get ident up to point
    //      filter a completion list
    // auto items = new JSONValue[0];
    // TODO: CompletionItem[] => CompletionList
    r.success(JSONValue(null)); // TODO
  }

  static void onCompletionResolve(ref Req r) {
    // TODO: CompletionItem -> CompletionItem
    r.success(JSONValue(null)); // TODO
  }

  static void onHover(ref Req r) {
    JSONValue hover;
    if (auto def = findDoc(r).findDefAtPoint(r)) {
      // TODO: MarkedString, MarkedString[] or MarkupContent
      hover["contents"] = text("Hello");
      hover["range"]    = LSP.Range().json; // TODO ?
    }
    hover["contents"] = text("Hello");
    r.success(hover);
  }

  static void onSignatureHelp(ref Req r) {
    JSONValue help;
    if (auto def = findDoc(r).findDefAtPoint(r)) {
      auto info = new JSONValue[0];
      help["signatures"]      = info;
      help["activeSignature"] = 0; // TODO if overloaded
      help["activeParameter"] = 0; // TODO if function?
    }
    r.success(help);
  }

  static void onDefinition(ref Req r) {
    JSONValue jump;
    if (auto def = findDoc(r).findDefAtPoint(r)) {
      // TODO Location | Location[]
      jump["uri"]   = ""; // TODO uri from def
      jump["range"] = LSP.Range().json;
    }
    r.success(jump);
  }

  static void onTypeDefinition(ref Req r) {
    JSONValue type;
    if (auto def = findDoc(r).findDefAtPoint(r)) {
      // TODO Location | Location[]
    }
    r.success(type);
  }

  static void onImplementation(ref Req r) {
    JSONValue impl;
    if (auto def = findDoc(r).findDefAtPoint(r)) {
      // TODO Location | Location[]
    }
    r.success(impl);
  }

  static void onReferences(ref Req r) {
    JSONValue refs;
    if (auto def = findDoc(r).findDefAtPoint(r)) {
      auto includeDecl = r.params["context"]["includeDeclaration"].isTrue0;
      // TODO: Location[]
    }
    r.success(refs);
  }

  static void onDocumentHighlight(ref Req r) {
    auto doc = findDoc(r);
    auto pos = LSP.Position(r.params["position"]);
    // TODO ident at point, find all other references
    JSONValue highlights;
    r.success(highlights);
  }

  static void onDocumentSymbol(ref Req r) {
    auto doc = findDoc(r);
    JSONValue symbols;
    // TODO: list of symbols
    r.success(symbols);
  }

  static void onCodeAction(ref Req r) {
    auto doc = findDoc(r);
    auto ctx = r.params["context"];
    auto diags = ctx["diagnostics"];
    auto range = LSP.Range(r.params["range"]);

    if (auto p = "only" in ctx) {
      // TODO: CodeActionKind[]
    }

    JSONValue actions;
    // TODO: handle

    r.success(actions);
  }

  static void onCodeLens(ref Req r) {
    auto doc = findDoc(r);
    // TODO handle
    JSONValue lenses;
    r.success(lenses);
  }

  static void onCodeLensResolve(ref Req r) {
    writeln("code lens resolve ", r.params);
    // TODO: handle

    JSONValue lens;
    r.success(lens);
  }

  static void onDocumentLink(ref Req r) {
    auto doc = findDoc(r);
    // TODO: handle
    JSONValue links;
    r.success(links);
  }

  static void onDocumentLinkResolve(ref Req r) {
    writeln("doc link resolve ", r.params);
    // TODO: handle
    JSONValue link;
    r.success(link);
  }

  static void onDocumentColor(ref Req r) {
    auto doc = findDoc(r);
    // TODO: handle
    JSONValue colors;
    r.success(colors);
  }

  static void onColorPresentation(ref Req r) {
    auto doc   = findDoc(r);
    auto color = r.params["color"];
    auto range = LSP.Range(r.params["range"]);
    // TODO: handle
    auto colors = new JSONValue[0];
    r.success(JSONValue(colors));
  }

  static void onFormatting     (ref Req r) { onFormattingImpl(r, false, false); }
  static void onRangeFormatting(ref Req r) { onFormattingImpl(r, true, false); }
  static void onTypeFormatting (ref Req r) { onFormattingImpl(r, true, true); }

  static void onFormattingImpl(ref Req r, bool isRange, bool onType) {
    auto doc = findDoc(r);
    // auto range = isRange ? LSP.Range(r.params["range"]) : doc.fullRange;

    dchar ch;
    if (onType) {
      size_t dummy;
      ch = r.params["ch"].str.decode(dummy);
    }

    ubyte tabSize;
    bool  insertSpaces;

    foreach (string k, v; r.params["options"]) {
      switch (k) {
      case "tabSize":      tabSize      = v.integer.to!ubyte; break;
      case "insertSpaces": insertSpaces = v.isTrue0;          break;
      default: enforce(0, "Unknown formatting option: " ~ k);
      }
    }

    JSONValue result;
    r.success(result); // TODO TextEdit[]
  }

  static void onRename(ref Req r) {
    JSONValue result;
    auto def = findDoc(r).findDefAtPoint(r);
    if (def) {
      auto to = r.params["newName"].str;
      // TODO: handle
      r.success(result); // TODO WorkspaceEdit[]
    }
    else {
      langServer.failure(r.id, LSP.ErrorCode.InvalidParams,
                         "No such symbol", result);
    }
  }

  static void onEval(ref Req r) {
    auto arg = r.params;
    auto env = Env(rt.rootEnv);
    auto rdr = Reader(arg["body"].str, arg["file"].str,
                      cast(uint)arg["line"].integer,
                      cast(uint)arg["column"].integer);
    env.ns = vc.currentNS;
    r.success(JSONValue(rdr.read().eval(env).toString));
  }

  alias Handler = void function(ref Req);

  __gshared Handler[string] handlers;

  shared static this() {
    handlers =
      ["initialize":                          &onInitialize,
       "initialized":                         &onInitialized,
       "shutdown":                            &onShutdown,
       "exit":                                &onExit,
       "$/cancelRequest":                     &onCancelRequest,

       "workspace/didChangeWorkspaceFolders": &onDidChangeWorkspaceFolders,
       "workspace/didChangeConfiguration":    &onDidChangeConfiguration,
       "workspace/didChangeWatchedFiles":     &onDidChangeWatchedFiles,
       "workspace/symbol":                    &onSymbol,
       "workspace/executeCommand":            &onExecuteCommand,

       "textDocument/didOpen":                &onDidOpen,
       "textDocument/didChange":              &onDidChange,
       "textDocument/willSave":               &onWillSave,
       "textDocument/willSaveWaitUntil":      &onWillSaveWaitUntil,
       "textDocument/didSave":                &onDidSave,
       "textDocument/didClose":               &onDidClose,

       "textDocument/completion":             &onCompletion,
       "completionItem/resolve":              &onCompletionResolve,
       "textDocument/hover":                  &onHover,
       "textDocument/signatureHelp":          &onSignatureHelp,
       "textDocument/definition":             &onDefinition,
       "textDocument/typeDefinition":         &onTypeDefinition,
       "textDocument/implementation":         &onImplementation,
       "textDocument/references":             &onReferences,
       "textDocument/documentHighlight":      &onDocumentHighlight,
       "textDocument/documentSymbol":         &onDocumentSymbol,
       "textDocument/codeAction":             &onCodeAction,
       "textDocument/codeLens":               &onCodeLens,
       "codeLens/resolve":                    &onCodeLensResolve,
       "textDocument/documentLink":           &onDocumentLink,
       "documentLink/resolve":                &onDocumentLinkResolve,
       "textDocument/documentColor":          &onDocumentColor,
       "textDocument/colorPresentation":      &onColorPresentation,
       "textDocument/formatting":             &onFormatting,
       "textDocument/rangeFormatting":        &onRangeFormatting,
       "textDocument/onTypeFormatting":       &onTypeFormatting,
       "textDocument/rename":                 &onRename,

       "repl/eval":                           &onEval];
  }
}

/// A "namespace" for Language Server Protocol definitions.
final class LSP {
  @disable this();
static:

  immutable
    contentType = "application/vscode-jsonrpc; charset=utf-8",
    separator   = "\r\n",
    headerSep   = ": ";

  enum ErrorCode {
    ParseError           = -32700,
    InvalidRequest       = -32600,
    MethodNotFound       = -32601,
    InvalidParams        = -32602,
    InternalError        = -32603,
    serverErrorStart     = -32099,
    serverErrorEnd       = -32000,
    ServerNotInitialized = -32002,
    UnknownErrorCode     = -32001,

    // Defined by the protocol.
    RequestCancelled     = -32800
  }

  alias DocumentUri = string; /// The URI of a Document.

  /// Position in a text document expressed as zero-based line and zero-based
  /// character offset.
  struct Position {
    uint line;
    uint character;

    this(JSONValue v) {
      line      = v["line"]     .integer.to!uint;
      character = v["character"].integer.to!uint;
    }

    JSONValue json() {
      JSONValue v;
      v["line"]      = line;
      v["character"] = character;
      return v;
    }
  }

  ///
  struct Range {
    Position start;
    Position end;

    this(JSONValue v) {
      start = Position(v["start"]);
      end   = Position(v["end"]);
    }
    this(Position start, Position end) {
      this.start = start;
      this.end   = end;
    }

    JSONValue json() {
      JSONValue v;
      v["start"] = start.json;
      v["end"]   = end  .json;
      return v;
    }
  }

  ///
  struct Location {
    DocumentUri uri;
    Range       range;

    JSONValue json() {
      JSONValue v;
      v["uri"]   = uri;
      v["range"] = range.json;
      return v;
    }
  }

  ///
  struct Diagnostic {
    Range    range;
    Severity severity;
    int      code; // TODO: can be string
    string   source;
    string   message;
    RelatedInformation[] relatedInformation;

    enum Severity {
      None        = 0,
      Error       = 1,
      Warning     = 2,
      Information = 3,
      Hint        = 4
    }

    JSONValue json() {
      JSONValue v;
      v["range"]   = range.json;
      v["message"] = message;

      if (severity != Severity.None) v["severity"] = severity;
      if (source.length)             v["source"]   = source;

      // TODO: code

      if (relatedInformation.length) {
        auto info = new JSONValue[relatedInformation.length];
        foreach (i, ref r; relatedInformation) info[i] = r.json;
        v["relatedInformation"] = info;
      }
      return v;
    }

    static struct RelatedInformation {
      Location location;
      string   message;

      JSONValue json() {
        JSONValue v;
        v["location"] = location.json;
        v["message"]  = message;
        return v;
      }
    }
  }

  ///
  struct Command {
    string title;
    string command;
    Any[]  arguments;
  }

  ///
  struct TextEdit {
    string range;
    string newText;
  }

  alias TextDocumentIdentifier = DocumentUri;

  struct VersionedTextDocumentIdentifier {
    TextDocumentIdentifier uri;
    int version_;
  }

  struct TextDocumentEdit {
    VersionedTextDocumentIdentifier textDocument;
    TextEdit[] edits;
  }

  struct WorkspaceFolder {
    string uri;
    string name;

    this(JSONValue v) {
      uri  = v["uri"] .str;
      name = v["name"].str;
    }
  }

  struct WorkspaceEdit {
    TextEdit[][DocumentUri] changes;
    TextDocumentEdit[] documentChanges;
  }

  struct DocumentFilter {
    string language;
    string scheme;
    string pattern;
  }

  alias DocumentSelector = DocumentFilter[];

  enum MarkupKind { PlainText, Markdown }

  struct MarkupContent {
    MarkupKind kind;
    string     value;
  }

  enum SymbolKind {
	  File          = 1,
    Module        = 2,
    Namespace     = 3,
    Package       = 4,
    Class         = 5,
    Method        = 6,
    Property      = 7,
    Field         = 8,
    Constructor   = 9,
    Enum          = 10,
    Interface     = 11,
    Function      = 12,
    Variable      = 13,
    Constant      = 14,
    String        = 15,
    Number        = 16,
    Boolean       = 17,
    Array         = 18,
    Object        = 19,
    Key           = 20,
    Null          = 21,
    EnumMember    = 22,
    Struct        = 23,
    Event         = 24,
    Operator      = 25,
    TypeParameter = 26,
  }

  enum InsertTextFormat : ubyte {
    PlainText = 1,
    Snippet   = 2
  }

  struct CompletionItem {
    string label;
    string detail;
    string documentation;
    string sortText;
    string filterText;
    Kind   kind;
    bool   isDeprecated;
    bool   preselect;
    InsertTextFormat insertTextFormat;
    TextEdit   textEdit;
    TextEdit[] additionalTextEdits;
    string[]   commitCharacters;
    Command    command;
    JSONValue  data;

    JSONValue json() const {
      JSONValue v;
      v["label"] = label;
      if (detail.length)        v["detail"]        = detail;
      if (documentation.length) v["documentation"] = documentation;
      if (sortText.length)      v["sortText"]      = sortText;
      // TODO: ??
      return v;
    }

    enum Kind : ubyte {
      Text          = 1,
      Method        = 2,
      Function      = 3,
      Constructor   = 4,
      Field         = 5,
      Variable      = 6,
      Class         = 7,
      Interface     = 8,
      Module        = 9,
      Property      = 10,
      Unit          = 11,
      Value         = 12,
      Enum          = 13,
      Keyword       = 14,
      Snippet       = 15,
      Color         = 16,
      File          = 17,
      Reference     = 18,
      Folder        = 19,
      EnumMember    = 20,
      Constant      = 21,
      Struct        = 22,
      Event         = 23,
      Operator      = 24,
      TypeParameter = 25
    }
  }
}

/// A source code file being edited.
class TextDocument {
  alias Map  = TextDocument[uint];        /// A mapping from hash to document.
  alias Set  = RedBlackTree!TextDocument; /// A set of text documents.
  alias Tree = RedBlackTree!Segment;      /// Text segments of a document.

  private {
    LSP.DocumentUri uri;     /// Unique location, serves as an identifier.
    Tree            tree;    /// The text being edited.
    NS*             ns;      /// Vile namespace for live information.
    string          langId;  /// Name of the language being edited.
    LangConn[]      clients; /// List of clients having this file opened.
    uint            verNum;  /// Version number, incremented with changes.
    uint            hash;    /// Use in comparisons instead of uri.
    // TODO: ^ make 64bit with first 32 used to sort alphabetically

    Appender!(Edit[]) edits;
  }

  /// Text change to commit on the document. Each change increments the version.
  /// These are accumulated from 'didChange' notifications, committed as needed.
  static struct Edit {
    string    newText;
    LSP.Range range;
  }

  this(LSP.DocumentUri uri, uint hash, JSONValue v) {
    this(uri, hash, v["text"].str.dup);
    this.langId = v["languageId"].str;
    this.verNum = v["version"].integer.to!uint;
  }
  this(LSP.DocumentUri uri, uint hash, char[] contents) {
    assert(uri.length);
    assert(hash != 0);
    this.uri  = uri;
    this.hash = hash;
    this.tree = new Tree(new Segment(contents));
  }

  bool opEquals(in TextDocument rhs) const { return rhs.hash == hash; }

  int opCmp(in TextDocument rhs) const {
    if (rhs.hash == hash) return 0;
    return hash < rhs.hash ? -1 : 1;
  }

  /// Locates the definition under point (fn, macro, var, local, etc)
  Def* findDefAtPoint(ref LangServer.Req r) {
    auto pos = LSP.Position(r.params["position"]);
    return findDefAtPoint(pos);
  }
  /// ditto
  Def* findDefAtPoint(scope ref LSP.Position pos) {
    return null; // TODO: resolve symbol at point
  }

  /**
   * TODO: really?
   */
  static class Segment {
    uint   line;
    uint   col;
    char[] text;

    this(char[] text) {
      this.text = text;
    }

    int opCmp(in Segment rhs) const {
      if (rhs.line != line) return line < rhs.line ? -1 : 1;
      if (rhs.col  != col)  return col  < rhs.col  ? -1 : 1;
      return 0;
    }
  }
}


// REPL Server
// -----------------------------------------------------------------------------

class ReplServer : TcpServer {
  this() {
    super(new net.InternetAddress("127.0.0.1", 11420));
  }

  override string name() { return "nREPL"; }

  override TcpConn newConnection() { return new ReplConn; }
}

class ReplConn : TcpConn {
  override void onData() {

  }
}


// Vile environment
// -----------------------------------------------------------------------------

__gshared Type*
  typeType, typeVoid, typeAny, typeNil,
  typeBool, typeChar, typeChar8, typeChar16, typeBigInt,
  typeSInt8, typeSInt16, typeSInt32, typeSInt64,
  typeUInt8, typeUInt16, typeUInt32, typeUInt64,
  typeFloat16, typeFloat32, typeFloat64, typeFloat80,
  typeStr, typeSym, typeKey, typePtr, typeObj, //typeObjC,
  typeMeta, typeArray, typeList, typeVec, typeMap, typeSet,
  typeId, typeReg, typeNS, typeVar, typeDef, typeFn, typeProp, typeTrait,
  typeUUID, typeVariadic, typeVoidPtr;

__gshared NS*  nsVileCore, nsVileLang, nsVileTool, nsVileRepl, nsVileBase;
__gshared Sym* sym_, symDeref, symQuote, symRef, symVar, symAnon, symCast;
__gshared Sym* symC, symCPlusPlus, symObjC, symVile, symFn;
__gshared Key* keyCPP, keyWrap, keyFlags, keyFull,
  keyLib, keyInclude, keyImport, keyExtends, keyStrong, keyNonAtom;

__gshared Key* keyType, keyCDecl;

__gshared Sym* symAdd, symSub, symMul, symDiv, symMod, symInc, symDec,
  symEq, symNeq, symGreater, symLess, symGreaterEq, symLessEq,
  symNot, symAnd, symOr,
  symBitNot, symBitAnd, symBitOr, symBitXor, symBitShl, symBitShr;

__gshared Sym* symArrayLength, symArrayPtr;

__gshared Any readFinished, readSkip;

shared static this() {
  // Bootstrapping the Vile types and values; a chicken and egg problem.

  // Step 0: types need symbols (from Def), symbols need types (from Val)
  typeType = new Type(typeid(Type), typeType); typeType.type = typeType;
  typeSym  = new Type(typeid(Sym),  typeType);
  typeType.sym  = Sym.intern("Type");
  typeSym .sym  = Sym.intern("Sym");

  // Step 1: types need namespaces (from Def), namespaces need types (from Val)
  typeNS     = new Type(typeid(NS),  "NS");
  typeDef    = new Type(typeid(Def), "Def");
  typeVar    = new Type(typeid(Var), "Var");
  nsVileBase = NS.intern("vile.base");
  nsVileCore = NS.intern("vile.core");

  nsVileCore.define(typeType.sym, Any(&typeType.val));
  nsVileCore.define(typeSym .sym, Any(&typeSym .val));
  nsVileCore.define(typeNS  .sym, Any(&typeNS .val));
  nsVileCore.define(typeDef .sym, Any(&typeDef.val));
  nsVileCore.define(typeVar .sym, Any(&typeVar.val));

  Type* deftypeN(Type.Native n) {
    auto t = new Type(n);
    nsVileCore.define(t.sym, Any(&t.val));
    return t;
  }
  Type* deftypeS(TypeInfo_Struct ti, string n) {
    auto t = new Type(ti, n);
    nsVileCore.define(t.sym, Any(&t.val));
    return t;
  }

  // Step 2: remaining types and language values
  typeVoid = deftypeN(Type.Native.Void);
  typeAny  = deftypeN(Type.Native.Any);
  typeNil  = deftypeN(Type.Native.Nil);
  typeBool = deftypeN(Type.Native.Bool);
  typeChar = deftypeN(Type.Native.Char);
  typeChar8 = deftypeN(Type.Native.Char);
  typeChar16 = deftypeN(Type.Native.Char);

  typeSInt8   = deftypeN(Type.Native.SInt8);
  typeSInt16  = deftypeN(Type.Native.SInt16);
  typeSInt32  = deftypeN(Type.Native.SInt32);
  typeSInt64  = deftypeN(Type.Native.SInt64);
  typeUInt8   = deftypeN(Type.Native.UInt8);
  typeUInt16  = deftypeN(Type.Native.UInt16);
  typeUInt32  = deftypeN(Type.Native.UInt32);
  typeUInt64  = deftypeN(Type.Native.UInt64);
  typeFloat16 = deftypeN(Type.Native.Float16);
  typeFloat32 = deftypeN(Type.Native.Float32);
  typeFloat64 = deftypeN(Type.Native.Float64);
  typeFloat80 = deftypeN(Type.Native.Float80);
  typeBigInt  = deftypeN(Type.Native.BigInt);

  typePtr  = deftypeN(Type.Native.Pointer);
  typeObj  = deftypeN(Type.Native.DObject);
  // typeObjC = deftypeN(Type.Native.NSObject);
  typeVariadic = new Type(Type.Native.Void, Sym.intern("...")); // TODO

  typeStr   = deftypeS(typeid(Str),   "Str");
  typeKey   = deftypeS(typeid(Key),   "Key");
  typeMeta  = deftypeS(typeid(Array), "Meta");
  typeArray = deftypeS(typeid(Array), "Array");
  typeList  = deftypeS(typeid(List),  "List");
  typeVec   = deftypeS(typeid(Vec),   "Vec");
  typeMap   = deftypeS(typeid(Map),   "Map");
  // typeSet   = deftypeS(typeid(Set),   "Set"); // https://issues.dlang.org/show_bug.cgi?id=19877

  typeId    = deftypeS(typeid(Identity), "Identity");
  typeReg   = deftypeS(typeid(Registry), "Registry");
  typeFn    = deftypeS(typeid(Fn),       "Fn");
  typeProp  = deftypeS(typeid(Prop),     "Prop");
  typeTrait = deftypeS(typeid(Trait),    "Trait");

  typeUUID  = deftypeS(typeid(UUID), "UUID");

  typeVoidPtr = Type.pointer(typeVoid);

  typeMeta .native = Type.Native.Meta;
  typeArray.native = Type.Native.Array;
  // typeFn   .native = Type.Native.Fn;

  sym_     = Sym.intern("_");
  symDeref = Sym.intern("deref");
  symQuote = Sym.intern("quote");
  symRef   = Sym.intern("ref");
  symVar   = Sym.intern("var");
  symAnon  = Sym.intern("#<anonymous>");

  symC         = Sym.intern("C");
  symCPlusPlus = Sym.intern("C++");
  symObjC      = Sym.intern("ObjC");
  symVile      = Sym.intern("Vile");

  symArrayLength = Sym.intern("length");
  symArrayPtr    = Sym.intern("ptr");

  keyType    = Key.intern(":type");
  keyCDecl   = Key.intern(":cdecl");

  keyCPP     = Key.intern(":cpp");
  keyWrap    = Key.intern(":wrap");
  keyFlags   = Key.intern(":flags");
  keyFull    = Key.intern(":full");
  keyLib     = Key.intern(":lib");
  keyInclude = Key.intern(":include");
  keyImport  = Key.intern(":import");
  keyExtends = Key.intern(":extends");
  keyStrong  = Key.intern(":strong");
  keyNonAtom = Key.intern(":nonatomic");

  readFinished = Any(new InternalMarker());
  readSkip = Any(new InternalMarker());
}


// Vile application
// -----------------------------------------------------------------------------

__gshared {
  Runtime    rt;
  Compiler   vc;
  Linker     ld;
  LangServer langServer;
  // ReplServer replServer;
}

void cmd(string[] args) {
  int  logLevel;
  bool showVersion;
  bool lsp;

  auto r = getopt(args,
                  opt.config.caseSensitive,
                  opt.config.bundling,
                  opt.config.passThrough,
                  opt.config.stopOnFirstNonOption,
                  "V|version", &showVersion,
                  "q|quiet",   { logLevel++; },
                  "v|verbose", { logLevel--; },
                  "t|task", &rt.runTask,
                  "lsp", &lsp,
                  "stderr",             &rt.stderr,
                  "showStatusTrace",    &rt.showStatusTrace,
                  "showExceptionTrace", &rt.showExceptionTrace,
                  "showErrorTrace",     &rt.showErrorTrace);

  if (r.helpWanted) {
    enum helpHeader = "VILE: Vile Integrated Lisp Environment";
    opt.defaultGetoptPrinter(helpHeader, r.options);
    rt.running = false;
  }
  else if (showVersion) {
    printVersion();
    rt.running = false;
  }
  else {
    // TODO: logLevel

    // rt.daemon = true;
    if (lsp) rt.daemon = true; // TODO: lsp always start?
  }
}

void printVersion() {
  writefln("Vile 0.1.0-bootstrap-dev [%s %s.%s]",
           d_compiler.name,
           d_compiler.version_major,
           d_compiler.version_minor);
}

void printThrowable(Throwable t, bool trace) nothrow {
  try {
    auto s = rt.stderr ? stderr : stdout;
    auto m = trace ? t.toString : t.msg;
    if (m.length > 0) s.writeln(m);
  }
  catch (Throwable e) {
    fprintf(rt.stderr ? cstdio.stderr : cstdio.stdout,
            "Error printing exception: %.*s\n\nOriginal error was: %.*s\n",
            cast(uint)e.msg.length, e.msg.ptr,
            cast(uint)t.msg.length, t.msg.ptr);
  }
}
void printThrowable(Throwable t) nothrow @nogc {
  fprintf(rt.stderr ? cstdio.stderr : cstdio.stdout, "%.*s\n",
          cast(uint)t.msg.length, t.msg.ptr);
}

void guardFail(Throwable t, bool trace, int exitCode) nothrow {
  rt.exitCode = exitCode;
  rt.running  = false;
  t.printThrowable(trace);
}

void guard(alias f, bool force = false)() nothrow {
  try if (force || rt.running) f();
  catch (ExitCode  e) guardFail(e, rt.showStatusTrace,    e.exitCode);
  catch (Error     e) guardFail(e, rt.showErrorTrace,     -1);
  catch (Exception e) guardFail(e, rt.showExceptionTrace, -2);
  catch (Throwable e) guardFail(e, true,                  -3);
}

version (Java) extern (C) export {
  void Java_Vile_init(void*, void*) {
    rt.init();
  }

  void Java_Vile_term(void*, void*) {
    rt.term();
  }

  void Java_Vile_run(void*, void*) {
    rt.run();
  }
}
else int main(string[] args) nothrow {
  version(Windows) {
    wincon.SetConsoleOutputCP(winnls.CP_UTF8);
  }
  guard!({ cmd(args); });
  guard!({ rt.init(); });
  guard!({ rt.exec(); });
  guard!({ rt.term(); }, true);
  return rt.exitCode;
}


version(none)
debug {
  void _(T)() if (is(T == struct)) {
    pragma(msg, format("%s size=%x align=%x", T.stringof, T.sizeof, T.alignof));
    foreach (immutable m; __traits(allMembers, T)) {
      pragma(msg, format("  %04x %s :: %s",
                         __traits(getMember, T, m).offsetof, m,
                         typeof(__traits(getMember, T, m)).stringof));
    }
  }

  void testAlign() {
    import directx.d3d12;
    _!D3D12_GRAPHICS_PIPELINE_STATE_DESC;
    _!D3D12_BLEND_DESC;
    _!D3D12_RASTERIZER_DESC;
    _!D3D12_DEPTH_STENCIL_DESC;
    _!D3D12_INPUT_LAYOUT_DESC;
  }
}
