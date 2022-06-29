



















// Today we'll be learning F#.




















// By writing a Lisp interpreter.




















// And then using it to implement microKanren.




















// And then... well that one is a surprise!



















// Nothing to Lisp in Four Simple Steps.
//
//   1. Read  • Syntax, operator precedence table, AST, ... 🤔
//   2. Print • ToStrings, serialization, handlers, ...     😧
//   3. Eval  • Code generation, toolchain, libraries, ...  🤯
//   4. Loop  • Review, test, build, stage, publish, ...    😬
//
// How hard can it be?
//
// In the case of a Lisp, it turns out to be easy.
//
// We just skip all of it (for this toy interpreter!)





















/// A simple Scheme/Clojure inspired interpreter.
/// 
/// Original implementation taken from
/// https://github.com/AshleyF/FScheme/blob/master/FScheme.fs
/// 
/// With minor fixes and few additions.
module Lisp
open System





















// Step -1 • Requirements
// -----------------------------------------------------------------
// This is what we'd like to provide as input to the interpreter:

/// Data.
/// 
/// Think of it as JSON; it's just scalars, sequences and maps.
/// 
/// A few notable additions enable us to represent programs:
/// - Lists, for function application
/// - Symbols, for identifiers and variables
/// - Keywords, for enum values, map keys and more (fast cmp)
/// - Quotation, to generate list expressions, like templates
/// 
/// This is the entirety of the language's syntax.
let source = """
    ;; Comments, because comments are good.

    #! #nil #t #f ; Short-form literals (*)

                  ; Top value -> type of all our values; think 'object' in C#
    undefined     ; Bottom -> type has no values; raises an error if evaluated
    nil           ; Unit -> type Nil has only one value, nil
    true false    ; Booleans
    +1 2 -3 3.14  ; Numbers -> just doubles, this is a toy language, like JavaScript
    "Hello World" ; Strings -> utf-16, because these are already available

    bare words !  ; Symbols -> syntax? where we're going, we don't need syntax
    :key :words   ; Keywords -> like symbols, but as self-evaluating values
    'a `b ~x ~@xs ; Quotation (quote, quasiquote, unquote, splice-unquote)

    () (1 2 "hi") ; Lists
    [] [1 2 "hi"] ; Vectors
    {} {1 2  3 4} ; Maps
    #{} #{1 2 3}  ; Sets
    """

/// Code.
/// 
/// Evaluating data interprets the data as code.
let forms = """
    ;; Symbols for variables and identifiers
    print ;=> #<print> (the print function)

    ;; Lists for function application.
    (print "Hello")
    (head [1 2]) ;=> 1

    ;; Blocks evaluate expressions in sequence
    ;; The value of the block is from the last expression
    (do (print 1) "Hello") ;=> "Hello"

    ;; Def names a definition in the current namespace
    (def answer 42)

    ;; Conditionals introduce a choice between multiple expressions
    (if true
      answer
      (print "oh no!"))

    ;; Lambda creates a new function
    (def print-name
      (fn [name]
        (print "Hello, " name)))

    ;; Let bindings introduce local variables
    (let [user "You"]
      (print-name user)


    ;; Let Over Lambda: Capture the current lexical environment
    (let [x 42]
      (fn add-x [y]
        (+ x y)))
    """




















// Step 0 • Types Definitions
// -----------------------------------------------------------------
// Only a handful of data types are required.
//
// Most of the types will be function signatures,
// and inferred automatically from the compiler.

/// Tokens provide a lexical view of the source text.
type Token =
    | Ignore | Dot
    | OpenParen  | CloseParen
    | OpenSquare | CloseSquare
    | OpenCurly  | CloseCurly
    | OpenHashCurly
    | Quote | QuasiQuote | Unquote | UnquoteSplice
    | Number  of string
    | String  of string
    | Symbol  of string
    | Keyword of string

/// Expressions represent both data as immutable values and code as functions.
/// 
/// The 'read' function creates an Expr from a string. (ie JSON.parse)
/// The 'eval' function applies an Expr as code.       (this variant unique to Lisp)
/// The 'print' function converts an Expr to a string. (ie JSON.stringify)
/// 
/// A function evaluates its arguments before applying them.
/// A macro does not, allowing this language to be extended.
[<CustomComparison>]
[<CustomEquality>]
type Expr =
    | Undefined | Nil 
    | Bool of bool
    | Num  of double
    | Str  of string
    | Sym  of string
    | Key  of string
    | Cons of Expr list * Expr
    | List of Expr list
    | Vec  of Expr []
    | Map  of Map<Expr, Expr>
    | Set  of Set<Expr>
    | Fn   of string * (Cont -> Expr list -> Expr)
    | Form of string * (Cont -> Env -> Expr list -> Expr)
    | Curr of Cont
    | Rest of Expr list
    | Marker of string
    | LVar of int
    interface IComparable with
        member x.CompareTo y =
            match y with
            | :? Expr as y' ->
                match x, y' with
                | Nil, Nil -> 0
                | Bool a, Bool b -> a.CompareTo b
                | Num a, Num b -> a.CompareTo b
                | Str a, Str b | Sym a, Sym b | Key a, Key b -> a.CompareTo b
                | LVar a, LVar b -> a.CompareTo b
                | _ -> -1
            | _ -> failwith "not an Expr"
    override x.Equals y =
        match y with
        | :? Expr as y' ->
            match x, y' with
            | Nil, Nil -> true
            | Bool a, Bool b -> a = b
            | Num a, Num b -> a = b
            | Str a, Str b | Sym a, Sym b | Key a, Key b -> a = b
            | List a, List b | List a, Rest b | Rest a, List b -> a = b
            | Vec a, Vec b -> a = b
            | Map a, Map b -> a = b
            | Set a, Set b -> a = b
            | Fn (_, f), Fn (_, g) -> Object.ReferenceEquals (f, g)
            | Form (_, f), Form (_, g) -> Object.ReferenceEquals (f, g)
            | Curr f, Curr g -> Object.ReferenceEquals (f, g)
            | LVar a, LVar b -> a = b
            | _ -> false
        | _ -> false
    override _.GetHashCode () = failwith "TODO"

/// Continuations represent the next step in a computation.
/// Called with the result of the expression being evaluated.
/// They can be called more than once. Or never.
/// 
/// The current continuation is captured using 'call/cc',
/// allowing powerful manipulations of control flow, for example
/// implementing async/await or exception handling from a library.
and Cont = Expr -> Expr

/// Scopes are sets of definitions and local variables indexed by name.
/// As a ref cell so 'def' can add new definitions to the current scope.
/// Each definition and variable is also a ref cell to support 'set!'.
and Scope = Map<string, Expr ref> ref

/// Our evaluation environment.
/// 
/// Passed to Form expressions only. Fn doesn't use it, having its arguments already evaluated.
/// 
/// The top of the list is the current scope. Popping this list returns to the previous scope.
/// 
/// Better than prototype inheritance -> same features, no chain lookups; backtracking is free.
and Env = Scope list

/// Utilities, mostly allowing vectors, sets & maps to be transformed using list functions.
/// A more serious impl would abstract over a seq.
module Expr =
    module List =
        let rec toPair = function
            | [] -> []
            | [_] -> failwith "Syntax error (toPair)."
            | a :: b :: t -> (a, b) :: toPair t
        let ofPair xs =
            let rec expand' acc = function
                | [] -> List.rev acc
                | (a, b) :: t -> expand' (b :: a :: acc) t
            expand' [] xs
    module Map =
        let ofList = List.toPair >> Map.ofSeq >> Map
        let toList: Map<_,_> -> Expr list = Map.toList >> List.ofPair
    module Set =
        let ofList = Set.ofList >> Set
        let toList = Set.toList
    module Vec =
        let ofList = Array.ofList >> Vec
        let toList = Array.toList





















// Step 0.1 • Constant Definitions
// -----------------------------------------------------------------
// Values used by the interpreter.

let undefined = Undefined
let nil = Nil 
let boolTrue = Bool true
let boolFalse = Bool false
let restArg = Sym "&"
let fn = Sym "fn"
let macro = Sym "macro"
let quote = Sym "quote"
let quasiQuote = Sym "quasiquote"
let unquote = Sym "unquote"
let splice = Sym "unquote-splicing"
let unbound = Marker "unbound"
let doMark = Marker "do"
let letrec = Marker "letrec"





















// Step 1 • Read :: string -> Expr
// -----------------------------------------------------------------
// The first step is converting source text into data.

let tokenize src =
    let rec string acc = function
        | '\\' :: '"' :: t -> string (acc + "\"") t
        | '\\' :: 'b' :: t -> string (acc + "\b") t
        | '\\' :: 'f' :: t -> string (acc + "\f") t
        | '\\' :: 'n' :: t -> string (acc + "\n") t
        | '\\' :: 'r' :: t -> string (acc + "\r") t
        | '\\' :: 't' :: t -> string (acc + "\t") t
        | '\\' :: '\\' :: t -> string (acc + "\\") t
        | '"'  :: t -> acc, t
        | c :: t -> string (acc + (c.ToString ())) t
        | _ -> failwith "Malformed string."
    let rec comment = function
        | '\r' :: t | '\n' :: t -> t
        | _ :: t -> comment t
        | [] -> []
    let endsToken = function
        | '(' | ')' | '[' | ']' | '{' | '}' | ';' | '"' -> true
        | w -> Char.IsWhiteSpace w
    let rec token acc = function
        | c :: _ as t when endsToken c -> acc, t
        | w :: t when Char.IsWhiteSpace w -> acc, t
        | c :: t -> token (acc + (c.ToString ())) t
        | [] -> acc, []
    and token' ty acc x t =
        let x', t' = token (x.ToString ()) t
        tokenize' (ty x' :: acc) t'
    and tokenize' acc = function
        | w :: t when Char.IsWhiteSpace w -> tokenize' acc t
        | ';' :: t -> tokenize' acc <| comment t
        | '.' :: t -> tokenize' (Dot :: acc) t
        | '(' :: t -> tokenize' (OpenParen :: acc) t
        | ')' :: t -> tokenize' (CloseParen :: acc) t
        | '[' :: t -> tokenize' (OpenSquare :: acc) t
        | ']' :: t -> tokenize' (CloseSquare :: acc) t
        | '{' :: t -> tokenize' (OpenCurly :: acc) t
        | '}' :: t -> tokenize' (CloseCurly :: acc) t
        | '#' :: '<' :: _ -> failwith "Unreadable."
        | '#' :: '_' :: t -> tokenize' (Ignore :: acc) t
        | '#' :: '{' :: t -> tokenize' (OpenHashCurly :: acc) t
        | '~' :: '@' :: t -> tokenize' (UnquoteSplice :: acc) t
        | '~' :: t -> tokenize' (Unquote :: acc) t
        | '`' :: t -> tokenize' (QuasiQuote :: acc) t
        | '\'' :: t -> tokenize' (Quote :: acc) t
        | '"' :: t ->
            let s, t' = string "" t
            tokenize' (String s :: acc) t'
        | '-' :: (d :: _ as t) when Char.IsDigit d -> token' Number acc '-' t
        | '+' :: (d :: _ as t) when Char.IsDigit d -> token' Number acc '+' t
        | d :: t when Char.IsDigit d -> token' Number acc d t
        | ':' :: s :: t when not <| endsToken s -> token' Keyword acc s t
        | s :: t -> token' Symbol acc s t
        | [] -> List.rev acc
    tokenize' [] <| Seq.toList src

let tokens = tokenize source

type private ReadState = TopLevel | InList | InVec | InMapOrSet
let read src =
    let rec endList ch expected st acc t =
        if st = expected then List.rev acc, t
        else failwith <| sprintf "Unexpected '%c'" ch
    and list st st' ty f t acc =
        let e, t' = read' st' [] t
        read' st (ty (f e) :: acc) t'
    and concat acc acc' t = List.rev (List.rev acc' @ acc), t
    and ignore st acc t =
        let e, t' = read' st [] t
        match e with
            | [] -> failwith "Syntax error (ignore #_)."
            | [_] -> acc, t'
            | _ :: e' -> concat acc e' t'
    and wrap st s acc t =
        let e, t' = read' st [] t
        let wrap' h e' = concat (List [s; h] :: acc) e' t'
        match e with
            | [] -> failwith "Syntax error (quote ')."
            | [h] -> wrap' h []
            | h :: e' -> wrap' h e'
    and read' st acc = function
        | Dot :: _ -> failwith "TODO cons reader"
        | OpenParen :: t -> list st InList List id t acc
        | OpenSquare :: t -> list st InVec Expr.Vec.ofList id t acc
        | OpenCurly :: t -> list st InMapOrSet Expr.Map.ofList id t acc
        | OpenHashCurly :: t -> list st InMapOrSet Expr.Set.ofList id t acc
        | CloseParen :: t -> endList ')' InList st acc t
        | CloseSquare :: t -> endList ']' InVec st acc t
        | CloseCurly :: t -> endList '}' InMapOrSet st acc t
        | Ignore :: t -> ignore st acc t
        | Quote :: (_ :: _ as t) -> wrap st quote acc t
        | QuasiQuote :: (_ :: _ as t) -> wrap st quasiQuote acc t
        | Unquote :: (_ :: _ as t) -> wrap st unquote acc t
        | UnquoteSplice :: (_ :: _ as t) -> wrap st splice acc t
        | x :: t ->
            let map = function
                | Symbol "#!" | Symbol "undefined" | Symbol "⊥" -> undefined
                | Symbol "#nil" | Symbol "nil" -> nil
                | Symbol "#t" | Symbol "true" -> boolTrue
                | Symbol "#f" | Symbol "false" -> boolFalse
                | Symbol s -> Sym s
                | Keyword s -> Key s
                | String s -> Str s
                | Number n -> Num (Double.Parse n)
                | _ -> failwith "Syntax error."
            read' st (map x :: acc) t
        | [] -> List.rev acc, []
    tokenize src |> read' TopLevel [] |> fst

let testMore = read "(1 (2 '(3 ~4 (5 ~6 #_7 8))))"
let fullExpr = read "(1 2 3 + () [] #{:hello \"world\" {:map 12}})"
let codeExpr = read "(if (do something) (run something-else) :failed)"
let expr = read source




















// Step 2 • Print :: Expr -> string
// -----------------------------------------------------------------
// Being able to display data is important. Literals can be read back.

let rec printList (xs: seq<Expr>) = String.Join (" ", Seq.map print xs)
and print = function
    | Undefined -> "⊥"
    | Nil -> "nil"
    | Bool b -> if b then "true" else "false"
    | Num n -> n.ToString ()
    | Sym s -> s
    | Key s -> ":" + s
    | Str s -> "\"" + s + "\""
    | Cons (h, t) -> "(" + (printList h) + " . " + (print t) + ")"
    | List [Sym "quote"; e] -> "'" + print e
    | List [Sym "quasiquote"; e] -> "`" + print e
    | List [Sym "unquote"; e] -> "~" + print e
    | List [Sym "unquote-splice"; e] -> "~@" + print e
    | List l | Rest l -> "(" + printList l + ")"
    | Vec v -> "[" + printList (Seq.ofArray v) + "]"
    | Map m -> "{" + printList (Map.toSeq m |> Seq.collect (fun (a, b) -> [a; b])) + "}"
    | Set s -> "#{" + printList (Set.toSeq s) + "}"
    | Fn (s, _) | Form (s, _) -> "#<" + s + ">"
    | Curr _ -> "#<cont>"
    | Marker s -> "#:" + s
    | LVar n -> "?" + (n.ToString ())

printfn "%s" <| printList fullExpr
printfn "%s" <| printList expr



















// Step a • Interpreter Utilities
// -----------------------------------------------------------------

let malformed n e =
    failwith <| sprintf "Malformed '%s': %s" n (print (List [e]))

let math name identity unary op =
    let math' cont = function
        | [] -> Num identity |> cont
        | [Num n] -> Num (unary * n) |> cont
        | Num n :: ns ->
            let op' a = function
                | Num b -> op a b
                | m -> malformed (sprintf "%s arg" name) m
            Num (List.fold op' n ns) |> cont
        | m -> malformed name (List m)
    ref <| Fn (name, math')

let compare name pred =
    let compare' cont = function
        | [Num a; Num b] -> (if pred a b then boolTrue else boolFalse) |> cont
        | m -> malformed name (List m) 
    ref <| Fn (name, compare')

let extend env bindings = ref (Map.ofList bindings) :: env
let lookup env symbol =
    match List.tryPick (fun (s: Scope) -> Map.tryFind symbol s.Value) env with
    | Some e -> e
    | None -> failwith <| sprintf "No binding for '%s'." symbol

type ParamList =
    | Params of Expr list
    | VarArg of Expr list * Expr

let mkParams xs =
    let len = Array.length xs
    if len < 2 || xs.[len - 2] <> restArg
    then Params <| List.ofArray xs
    else VarArg (List.ofArray <| Array.sub xs 0 (len - 2), xs.[len - 1])

let zipParams args =
    let len = List.length args
    function
        | Params xs ->
            if len = List.length xs then List.zip xs args
            else failwith "Wrong number of arguments."
        | VarArg (xs, rest) ->
            let xlen = List.length xs
            if len < xlen then failwith "Not enough arguments." else
                (List.zip xs <| List.take xlen args) @ [rest, Rest <| List.skip xlen args]

let undefinedTy = Sym "Undefined"
let nilTy  = Sym "Nil"
let boolTy = Sym "Bool"
let numTy  = Sym "Num"
let strTy  = Sym "Str"
let symTy  = Sym "Sym"
let keyTy  = Sym "Key"
let consTy = Sym "Cons"
let listTy = Sym "List"
let vecTy  = Sym "Vec"
let mapTy  = Sym "Map"
let setTy  = Sym "Set"
let fnTy   = Sym "Fn"
let lvarTy = Sym "LVar"

let refStar1 = ref Nil
let refStar2 = ref Nil
let refStar3 = ref Nil
let refStarE = ref Nil

let mutable backtrack: list<(unit -> Expr)> = []

type Either<'a, 'b> =
    | Left of 'a
    | Right of 'b




















// Step 3 • Eval :: Expr -> Expr
// -----------------------------------------------------------------
// Evaluating an expression yields another expression.
//
// Evaluation Models:
// - Register (LLVM -> infinity of immutable registers -> explicit registers specified for results and operands)
// - Stack    (MSIL -> infinity of immutable locals -> results pushed on stack, operands taken from stack) 
// - CPS      (Lisp -> environment + function composition -> code being eval is a value passed back to eval) 
// (only the Scheme dialect of Lisps -> CPS is not the eval model of Clojure, Common-Lisp, Emacs-Lisp and others)
// (not to be confused with CSP -> building processes using async operations on data channels -> Go language, Clojure core.async lib)
//
// Rich Code for Tiny Computers: A Simple Commodore 64 Game in C++17 - Jason Turner
// https://www.youtube.com/watch?v=zBkNBP00wJE

let rec eval cont env = function
    // Environment lookup -> Variables
    | Sym s -> (lookup env s).Value |> cont
    // Self Evaluating Forms -> Literals
    | Nil | Bool _ | Num _ | Str _ | Key _ | Curr _ | List [] | LVar _ as lit -> cont lit
    | Rest r -> mapEval (List >> cont) env r
    | Vec v -> Expr.Vec.toList v |> mapEval (Expr.Vec.ofList >> cont) env
    | Set s -> Expr.Set.toList s |> mapEval (Expr.Set.ofList >> cont) env
    | Map m -> Expr.Map.toList m |> mapEval (Expr.Map.ofList >> cont) env
    // Application -> Function calls
    | List (h :: t) ->
        eval (function
            | Fn (_, f) -> apply cont env f t // User-defined function -> Program
            | Form (_, f) -> f cont env t // Special form or macro -> Compiler
            | Curr f ->
                match t with
                    | [] -> malformed "call/cc continuation param" (List t)
                    | [x] -> eval f env x
                    | xs -> mapEval (List >> f) env xs
            | m -> malformed "call" m) env h
    // Runtime errors (or compile-time inside macros)
    | Marker s -> failwith <| sprintf "Cannot evaluate marker value %s" s
    | Undefined -> failwith "Undefined behavior!"
    | m -> malformed "expr" m

and apply cont env fn args = mapEval (fn cont) env args

and mapEval cont env forms =
    let rec mapEval' acc = function
        | h :: t -> eval (fun a -> mapEval' (a :: acc) t) env h
        | [] -> List.rev acc |> cont
    mapEval' [] forms




















// Step 3.1 • Special Forms -> 
// -----------------------------------------------------------------
// A Turing Complete and Extendable Language in 9 Simple Forms
//
// On the Expressive Power of Programming Languages - Shriram Krishnamurthi
// https://www.youtube.com/watch?v=43XaZEn2aLc
//
// Into the Core - Squeezing Haskell into Nine Constructors - Simon Peyton Jones
// https://www.youtube.com/watch?v=uR_VzYxvbxg

// (Do form ...) -> blocks, eval given forms and returns the last result
and Do cont env =
    let rec foldEval last = function
        | h :: t -> eval (fun x -> foldEval x t) env h
        | [] -> cont last
    foldEval nil

// (If cond then else) -> eval cond determines the branch to eval next
and If cont env = function
    | [cond; t; f] ->
        eval (function
            | Nil | Bool false -> eval cont env f
            | _ -> eval cont env t) env cond
    | m -> malformed "if" (List m)

// (Let [bindings] block) -> eval block in a new scope with local variable bindings
and Let cont env = function
    | Vec bindings :: body ->
        let rec mapBind acc = function
            | [Sym s; e] :: t -> eval (fun x -> mapBind ((s, ref x) :: acc) t) env e
            | [] ->
                let frame = List.rev acc
                let env' = extend env frame
                Do cont env' body
            | _ -> failwith "Malformed 'let' binding."
        mapBind [] <| letBind bindings
    | m -> malformed "let" (List m)

and LetStar cont env = function
    | Vec bindings :: body ->
        let rec foldBind env' = function
            | [Sym s; e] :: t -> eval (fun x ->
                foldBind (extend env' <| [s, ref x]) t) env' e
            | [] -> Do cont env' body
            | _ -> failwith "Malformed 'let*' binding."
        foldBind env <| letBind bindings
    | m -> malformed "let*" (List m)

and LetRec cont env = function
    | Vec bindings :: body ->
        let bind = function
            | [Sym s; _] -> s, ref letrec
            | m -> malformed "letrec binding" (List m)
        let bindings' = letBind bindings
        let env' = List.map bind bindings' |> extend env
        let frame = env'.Head.Value
        let rec mapUpdate = function
            | [Sym s; e] :: t -> eval (fun x -> (frame.Item s).Value <- x; mapUpdate t) env' e
            | [] -> Do cont env' body
            | _ -> failwith "Malformed 'letrec' binding."
        mapUpdate bindings'
    | m -> malformed "letrec" (List m)

and letBind = Array.toList >> List.chunkBySize 2

// (Def var value) -> eval value and add it to the environment as var
and DefVar cont (env : Env) = function
    | [Sym s; e] ->
        let def = ref unbound
        env.Head.Value <- Map.add s def env.Head.Value
        eval (fun x -> def.Value <- x; x |> cont) env e
    | m -> malformed "def" (List m)

// (Set! var value) -> eval value and assign it to var
and SetVar cont env = function
    | [Sym s; e] -> eval (fun x -> (lookup env s).Value <- x; x |> cont) env e
    | m -> malformed "set!" (List m)

// (Fn [arg-list] block)      -> lambda constructor. Evaluates arguments when called,
// (Fn name [arg-list] block)    binds them to arg-list and then evaluates block.
and Lambda cont env = function
    | Vec _ as paramList :: body -> Lambda cont env <| fn :: paramList :: body
    | Sym name :: Vec xs :: body ->
        let ps = mkParams xs
        let closure cont' env' args =
            let rec mapBind acc = function
                | (Sym p, a) :: t -> eval (fun x -> mapBind ((p, ref x) :: acc) t) env' a
                | [] ->
                    let env'' = List.rev acc |> extend (env @ env')
                    Do cont' env'' body
                | _ -> failwith "Malformed fn param."
            zipParams args ps |> mapBind []
        Form (name, closure) |> cont
    | m -> malformed "fn" (List m)

// (Macro [arg-list] block)      -> macro constructor. Same as a lambda, but
// (Macro name [arg-list] block)    evaluates the result instead of the arguments (!)
and Macro cont env = function
    | Vec _ as paramList :: body -> Macro cont env <| macro :: paramList :: body
    | Sym name :: Vec xs :: body ->
        let ps = mkParams xs
        let closure cont' env' args =
            let bind = function Sym p, a -> p, ref a | _, m -> malformed "macro param" m
            let env'' = zipParams args ps |> List.map bind |> extend env
            Do (eval cont' env') env'' body
        Form (name, closure) |> cont
    | m -> malformed "macro" (List m)

// (Quote form) -> prevents the evaluation of a form. QuasiQuote allows evaluating subexpressions.
and Quote cont _ = function
    | [x] -> cont x
    | m -> malformed "quote" (List m)

and QuasiQuote cont env = 
    let rec cat acc = function
        | Left x -> x :: acc
        | Right x -> List.rev x @ acc
    and map acc ty = function
        | [] -> ty <| List.rev acc
        | h' :: t' -> unquote (fun x -> map (cat acc x) ty t') h'
    and unquote cont' = function
        | List [Sym "unquote"; e] -> eval (Left >> cont') env e
        | List [Sym "unquote-splicing"; e] ->
            let splice cont'' = function
                | List l | Rest l -> l |> cont''
                | Vec v -> Expr.Vec.toList v |> cont''
                | Set s -> Expr.Set.toList s |> cont''
                | Map m -> Expr.Map.toList m |> cont''
                | m -> malformed "unquote-splicing" m
            eval (splice <| Right >> cont') env e
        | List (Sym "unquote" :: _) as m -> malformed "unquote" m
        | List (Sym "unquote-splicing" :: _) as m -> malformed "unquote-splicing" m
        | List l -> map [] List l |> Left |> cont'
        | Vec v -> Expr.Vec.toList v |> map [] Expr.Vec.ofList |> Left |> cont'
        | Set s -> Expr.Set.toList s |> map [] Expr.Set.ofList |> Left |> cont'
        | Map m -> Expr.Map.toList m |> map [] Expr.Map.ofList |> Left |> cont'
        | e -> cont' (Left e)
    function
        | [e] -> unquote (function Left e -> cont e | Right e -> cont (List e)) e
        | m -> malformed "quasiquote" (List m)

// (Call/CC f) -> Call w/ Current Continuation. Swiss-army knife of control flow.
and CallCC cont env = function
    | [callee] -> eval (function
        | Form (_, f) -> f cont env [Curr cont]
        | Curr f -> Curr cont |> f |> cont
        | m -> malformed "call/cc" m) env callee
    | m -> malformed "call/cc" (List m)




















// Step 3.2 • Library
// -----------------------------------------------------------------

and Eq cont = function
    | [a; b] -> a.Equals b |> Bool |> cont
    | m -> malformed "=" (List m)

and Read cont = function
    | [Str s] -> read s |> List.head |> cont
    | m -> malformed "read (not a string)" (List m)

and Eval cont env = function
    | [form] -> eval (eval cont env) env form
    | m -> malformed "eval" (List m)

and Print cont args = printList args |> printfn "%s"; cont nil

and Type cont = function
    | [e] -> match e with
             | Undefined | Marker _ -> cont undefinedTy
             | Nil -> cont nilTy
             | Bool _ -> cont boolTy
             | Num _ -> cont numTy
             | Str _ -> cont strTy
             | Sym _ -> cont symTy
             | Key _ -> cont keyTy
             | Cons _ -> cont consTy
             | List _ | Rest _ -> cont listTy
             | Vec _ -> cont vecTy
             | Map _ -> cont mapTy
             | Set _ -> cont setTy
             | Fn _ | Form _ | Curr _ -> cont fnTy
             | LVar _ -> cont lvarTy
    | m -> malformed "type" (List m)

and Name cont = function 
    | [Sym s] | [Key s] -> cont (Str s)
    | [(Str _) as s] -> cont s
    | m -> malformed "name" (List m)

and Count cont =
    let cont' = double >> Num >> cont
    function
        | [Cons (h, Nil)] -> List.length h |> cont'
        | [Cons (h, _)] -> List.length h + 1 |> cont'
        | [List l] | [Rest l] -> List.length l |> cont'
        | [Vec v] -> Array.length v |> cont'
        | [Set s] -> Set.count s |> cont'
        | [Map m] -> Map.count m |> cont'
        | [Str s] -> String.length s |> cont'
        | m -> malformed "count" (List m)

and Append cont = function
    | [List a; List b] | [List a; Rest b]
    | [Rest a; List b] | [Rest a; Rest b] -> List (a @ b) |> cont
    | [Str a; Str b] -> Str (a + b) |> cont
    | m -> malformed "append" (List m)

and Cons' cont = function
    | [h; Nil] -> List [h] |> cont
    | [h; List t] | [h; Rest t] -> List (h :: t) |> cont
    | [h; Cons (h', t)] -> Cons (h :: h', t) |> cont
    | [h; t] -> Cons ([h], t) |> cont
    | m -> malformed "cons" (List m)

and Head cont = function 
    | [List (h :: _)] | [Rest (h :: _)] -> h |> cont
    | [Vec v] -> Array.head v |> cont
    | [Cons (h, _)] -> List.head h |> cont
    | m -> malformed "head" (List m)

and Tail cont = function
    | [List (_ :: t)] | [Rest (_ :: t)] -> List t |> cont
    | [Vec v] -> Array.tail v |> Vec |> cont
    | [Cons ([_], t)] -> t |> cont
    | [Cons ((_ :: t), t')] -> Cons (t, t') |> cont
    | m -> malformed "tail" (List m)

and Assoc cont = function
    | [Map m; k; v] -> Map.add k v m |> Map |> cont
    | m -> malformed "assoc" (List m)

and Get cont = function
    | [Map m; k] ->
        match Map.tryFind k m with
        | Some v -> cont v
        | None -> cont nil
    | m -> malformed "get" (List m)

and Nth cont = function
    | [List l; Num n] | [Rest l; Num n] -> l.[int n] |> cont
    | [Vec v; Num n] -> v.[int n] |> cont
    | m -> malformed "nth" (List m)

and ToSeq cont = function
    | [Map m] -> Map.toList m |> Expr.List.ofPair |> List |> cont
    | m -> malformed "seq" (List m)

and MakeLVar cont = function
    | [Num n] -> int n |> LVar |> cont
    | m -> malformed "lvar" (List m)




















// Step 3.3 • Top-Level Environment
// ---------------------------------------------------------------------------
// Maps symbols to Lisp values (as an Expr cell)
// More advanced interpreter/compiler would have one per module.

and env = [ref <| Map.ofList
    [
        // Special Forms
        "do", ref (Form ("do", Do))
        "let", ref (Form ("let", Let))
        "let*", ref (Form ("let*", LetStar))
        "letrec", ref (Form ("letrec", LetRec))
        "if", ref (Form ("if", If))
        "fn", ref (Form ("fn", Lambda))
        "macro", ref (Form ("macro", Macro))
        "def", ref (Form ("def", DefVar))
        "set!", ref (Form ("set!", SetVar))
        "quote", ref (Form ("quote", Quote))
        "quasiquote", ref (Form ("quasiquote", QuasiQuote))
        "call/cc", ref (Form ("call/cc", CallCC))
        // Library
        "read", ref (Fn ("read", Read))
        "eval", ref (Form ("eval", Eval))
        "print", ref (Fn ("print", Print))
        "type", ref (Fn ("type", Type))
        "name", ref (Fn ("name", Name))
        "append", ref (Fn ("append", Append))
        "count", ref (Fn ("count", Count))
        "cons", ref (Fn ("cons", Cons'))
        "head", ref (Fn ("head", Head))
        "tail", ref (Fn ("tail", Tail))
        "assoc", ref (Fn ("assoc", Assoc))
        "get", ref (Fn ("get", Get))
        "nth", ref (Fn ("nth", Nth))
        "seq", ref (Fn ("seq", ToSeq))
        "lvar", ref (Fn ("lvar", MakeLVar))
        "=", ref (Fn ("=", Eq))
        "+", math "+" 0.0  1.0 (+)
        "-", math "-" 0.0 -1.0 (-)
        "*", math "*" 1.0  1.0 (*)
        "/", math "/" 1.0  1.0 (/)
        "%", math "%" 1.0  1.0 (%)
        "==", compare "==" (=)
        "<>", compare "<>" (<>)
        ">",  compare ">"  (>)
        ">=", compare ">=" (>=)
        "<",  compare "<"  (<)
        "<=", compare "<=" (<=)
        // REPL results / error
        "*1", refStar1
        "*2", refStar2
        "*3", refStar3
        "*e", refStarE
    ]]





















/// Step 4 • REPL
/// ----------------------------------------------------------------
/// Wire everything together!

let readLines prompt =
    let count x = Seq.filter ((=) x) >> Seq.length
    let rec readLine n acc prompt' =
        printf "%s" prompt'
        let s = Console.ReadLine ()
        let n' = n + (count '(' s - count ')' s)
        let acc' = acc + s
        if n' > 0 then readLine n' acc' "\\ "
        else if acc' = "" then readLine 0 "" prompt
        else acc'
    readLine 0 "" prompt

let rep env =
    let eval' = function
        | Sym "?" ->
            match backtrack with
            | h :: t -> backtrack <- t; h ()
            | [] -> printfn "No current problem."; Sym "ok"
        | e -> eval id env e
    let preEval = List.head
    let postEval = function
        | Key "q" | Key "quit" -> None
        | e ->
            refStar3.Value <- refStar2.Value
            refStar2.Value <- refStar1.Value
            refStar1.Value <- e
            Some e
    read >> preEval >> eval' >> postEval >> Option.map print

let repl () =
    let rec repl' = function
        | None -> printfn "Bye, cruel world!"; ()
        | Some o ->
            try sprintf "%s\nλ " o |> readLines |> rep env |> repl'
            with ex ->
                refStarE.Value <- Str ex.Message
                repl' <| Some ex.Message
    repl' <| Some "Hello, world!"





















// Step 5 • REPL Driven Development
// -----------------------------------------------------------------

repl () // Driven Development

let me = """ show you

repl () // F#
:quit ;; Lisp
:q    ;; Free tutorial: how to exit Vim :D










;; Literals
;; --------

undefined
nil
true
false
3.12
"Hello World"
:keyword
symbol
+
(+ 1 2 3)
'(1 2 3 hello world)
[1 2 3 'hello 'world]
{:a 1 :b 2 :hello "world"}
#{1 2 3 3 ['hello (+ 1 2)] {:world "Whee!"}}
{(name 'hello) (+ 1 2 3 4)}
*1
*2
*3
*e

;; Variables
(def my-var :this-is-a-long-keyword)
my-var
'my-var

;; Conditionals
(if my-var "Hi" "Oh no")

(if false "Nope!"
  (if my-var "Whoo!"
    (undefined "Oh no!")))

;; Local variables
(let [x 0
      y 3.14]
  (print x y)
  (if (= 0 x) y x))

(let* [x 12
       y (+ x 8)]
  (* y 2))

(letrec [x (fn x [a] (print "Hello from" (if a a (y))))
         y (fn y [] (x y))]
  (x nil))

;; Functions
(fn [a] (print a))
(*1 "hello")

(def show print)
(show "World\nWith newlines")










;; Compiler Access (!)
;; -------------------

(print hello)
(read "(def hello :world)")
(eval "(def hello :world)")
(show '(def hello :world))
(eval '(def hello :world))
(print hello)


(print (eval (read "(def my-fn (fn hello-lisp-world [x y] (+ x y 10)))")))
my-fn
(my-fn 1 2)










;; Quotation
;; ---------

;; Quote
(+ 1 2)
'(+ 1 2)

`(a ~(+ 1 2) ~{(> 1 2) (name :hi)})

`(1 2 ~@[1 2 3])
`~@'(1 2 3)

;; Macros

(fn [x] `(quote ~x))
(*1 (+ 1 2 3))

(macro [x] `(quote ~x))
(*1 (+ 1 2 3))
(eval *1)

(macro [a b] `(def ~a (quote ~b)))
(*1 hello-world (this will not be evaluated))
hello-world










;; Continuations
;; -------------

(call/cc (fn [k] (k 10)))

(/ 1 0)
(let [n 1
      d 0]
  (call/cc (fn [k]
             (/ n (if (== d 0)
                    (k "Oh no!")
                    d)))))

(call/cc (fn [k] (if (k 10) 20 30)))

(call/cc (fn [k] (do (print 1) (print k) (k 3) (print 4))))

(let [c 10
      r (call/cc (fn [k] k))]
  (print "Hello!" c)
  (if (== 0 c) 'done
    (do (set! c (- c 1))
        (r r))))










;; Growing a Language - Guy Steele
;; https://www.youtube.com/watch?v=lw6TaiXzHAE

(def var-name "value expression")
var-name

(def defmacro (macro defmacro [sym args & body]
                `(def ~sym (macro ~sym ~args ~@body))))

(defmacro defn [sym args & body]
  `(def ~sym (fn ~sym ~args ~@body)))

(defn foo [a] (print 'Hello, a))
foo
(foo 'Foo)
(foo 1 2)

(defn bar [a & x] (print a x))
(bar "Hello, here are more params:" bar 1 2 3 (+ 1 3))

(defn zero? [n] (== 0 n))
(zero? 0)
(zero? 1)

(defn fact [n] (if (zero? n) 1 (* (fact (- n 1)) n)))
(fact 5)

(defn not [x] (if x false true))
(defn != [x y] (not (= x y)))
(not true)
(= 1 1)
(= 1 2)
(!= 1 2)
(!= 1 1)

(defn id [x] x)

(defmacro comment [& _] nil)
(comment This will never be evaluated!)

(defmacro deftype? [sym type] `(defn ~sym [x] (= ~type (type x))))
(deftype? undefined? 'Undefined)
(deftype? nil? 'Nil)
(deftype? bool? 'Bool)
(deftype? num? 'Num)
(deftype? str? 'Str)
(deftype? sym? 'Sym)
(deftype? key? 'Key)
(deftype? cons? 'Cons)
(deftype? list? 'List)
(deftype? vec? 'Vec)
(deftype? map? 'Map)
(deftype? set? 'Set)
(deftype? fn? 'Fn)
(deftype? lvar? 'LVar)

(nil? nil)
(nil? 1)
(num? 1)
(str? "Hello")
(nil? "Hello")
(fn? +)
(num? +)
(sym? +)
(sym? '+)


(defmacro when [cond & then] `(if ~cond (do ~@then) nil))
(defmacro when-not [cond & else] `(if ~cond nil (do ~@else)))
(defmacro if-not [cond else then] `(if ~cond ~then ~else))

(when true (print 1) (print 2) :done)
(when-not false 1)
(if-not true :wait :done)

(def first head)
(defn second [xs] (head (tail xs)))
(second '(1 2))
(second [1 2])

(defmacro if-let [binding then else]
  (let [var-sym (first binding)]
    `(let [~var-sym ~(second binding)]
       (if ~var-sym ~then ~else))))

(defmacro when-let [binding & then]
  (let [var-sym (first binding)]
    `(let [~var-sym ~(second binding)]
       (when ~var-sym ~@then))))

(if-let [a nil] (print a) (print "oh no"))
(when-let [a (= 'Bool (type 1))] (print a))

(defn empty? [x] (if (= nil x) true (= 0 (count x))))
(defn not-empty? [x] (not (empty? x)))
(empty? nil)
(empty? "")
(empty? [])

(defn map [f xs]
  (when (not-empty? xs)
    (cons (f (head xs))
          (map f (tail xs)))))

(map id '(1 2 3))
(map (fn [x] (* 10 x)) '(1 2 3))

(defn fold [f a xs]
  (if (empty? xs)
    a
    (fold f (f (head xs) a) (tail xs))))

(defn sum [& xs] (fold + 0 xs))
(sum 1 2 3 4)

(defmacro list [& xs] `(map eval (quote ~xs)))
(list 'hello "World" 11)

(defn rev [xs] (fold cons nil xs))
(rev [1 2 3])

(defn inc [x] (+ x 1))
(defn dec [x] (- x 1))

(defmacro while [test & body]
  `(letrec [loop (fn []
                   (when ~test (do ~@body (loop))))]
     (loop)))

(let [d 10]
  (while (>= d 0)
    (print d)
    (set! d (dec d))))

(defmacro for [binding expr]
  (let [v (head binding)]
    `(map (fn [~v] ~expr) ~(second binding))))

(for [x [1 2 3 4]]
  [:div x])

(defmacro cond [& clauses]
  (if (empty? clauses)
    'undefined
    (let [h (head clauses)
          t (tail clauses)]
      (if (empty? t)
        'undefined
        (let [h' (head t)
              t' (tail t)]
          `(if ~h ~h' (cond ~@t')))))))

(cond
  nil undefined
  false (+ 1 2)
  false "oh no"
  :else "No match")

(defmacro and [x & xs]
  `(when-let [x' ~x]
     ~(if (empty? xs)
        'x'
        `(and ~(head xs) ~@(tail xs)))))

(and true 12 "hi" 1 2 3 4)

(defmacro or [x & xs]
  `(if-let [x' ~x]
     x'
     ~(if (empty? xs)
        nil
        `(or ~(head xs) ~@(tail xs)))))

(or nil 1 false :hello nil)

(defn next [xs]
  (when (not-empty? xs)
    (when-let [xs' (tail xs)]
      (when (or (not (list? xs')) (not-empty? xs'))
        xs'))))
(next nil)
(next [])
(next [1])
(next [1 2 3])
(next (cons 1 2))

(def *trace* true)

(defmacro trace [what & xs]
  `(when *trace*
     (print (quote ~what) ~@xs)))

(trace hello :world)
(let [*trace* false] (trace hello :world))

(defmacro defnt [sym args & body]
  `(defn ~sym ~args
     (trace ~sym ~@args)
     ~@body))

(defnt hi [a b] (+ a b))
(hi 1 2)








;; Quines! -> Programs that evaluate to themselves
;; http://community.schemewiki.org/?quines

`(print 1 2 ~(+ 1 2))
(eval *1)



((fn [x]
  `((fn [x] ~x) '~x))
 '`((fn [x] ~x) '~x))

'`((fn [x] ~x) '~x)

*1
(eval *1)
(= (eval *1) *1)
(= '(1 2 3) '(1 2))



((fn [q] ((fn [x] `((fn [q] ~((eval q) x)) '~q))
          '(fn [x] `((fn [q] ~((eval q) x)) '~q))))
 '(fn [x] `(~x '~x)))

(eval *1)



(call/cc
 (fn [c]
   (c ((fn [c] `(call/cc (fn [c] (c (~c '~c)))))
       '(fn [c] `(call/cc (fn [c] (c (~c '~c)))))))))

(eval *1)



(call/cc
 (fn [c]
   (call/cc
    (fn [cc]
      (c ((fn [c] `(call/cc (fn [c] (call/cc (fn [cc] (c (~c '~c)))))))
          '(fn [c] `(call/cc (fn [c] (call/cc (fn [cc] (c (~c '~c)))))))))))))

(eval *1)



((fn [x] `(~(rev x) '~x)) '(`(~(rev x) '~x) [x] fn))

(eval *1)



((fn [c]
   (if (fn? c)
     (c '`((fn [c] (if (fn? c) (c '~c) ~c)) (call/cc call/cc)))
     `((fn [c] (if (fn? c) (c '~c) ~c)) (call/cc call/cc))))
 (call/cc call/cc))

(eval *1)


((fn [c]
   (if (fn? c) (c 0)
     ((fn [c] `((fn [c] (if (fn? c) (c 0) (~c '~c)))
                (call/cc call/cc)))
      '(fn [c] `((fn [c] (if (fn? c) (c 0) (~c '~c)))
                 (call/cc call/cc))))))
 (call/cc call/cc))

(eval *1)

((fn [x] (list x (list 'quote x)))
 '(fn [x] (list x (list 'quote x))))

(eval *1)


;; Inter Twine

(defn check-twine [a b]
  (and (not (= a b))
       (= (eval a) b)
       (= (eval b) a)))

(def twine-a
  '((fn [c] (list 'quote (list c (list 'quote c))))
    '(fn [c] (list 'quote (list c (list 'quote c))))))

(def twine-b `(quote ~twine-a))

(eval *1)
(check-twine twine-a twine-b)

(def twine-2-a
  '(list
    '(fn [x]
       (list 'list x (list 'quote (list 'quote x))))
    '''(fn [x]
         (list 'list x (list 'quote (list 'quote x))))))

(def twine-2-b (eval twine-2-a))

(eval *1)










;; microKanren (surprise!)
;; -----------------------

(def empty-state [{} 0])
(defn empty-state* [g] (g empty-state))

(defn S [a] (nth a 0))
(defn C [a] (nth a 1))

(defn walk [u s]
  (if-let [t (and (lvar? u) (get s u))]
    (walk t s)
    u))

(def v0 (lvar 0))
(def v1 (lvar 1))
(def s0 {})
(def s1 (assoc s0 v0 v1))
(def s2 (assoc s1 v1 "Hello"))
(walk (lvar 0) s2)

(defn occurs? [x v s]
  (let [v (walk v s)]
    (cond
      (lvar? v) (= v x)
      (vec? v) (or (occurs? x (head v) s)
                   (occurs? x (next v) s))
      :else false)))

(defn ext-s [x v s]
  (if (occurs? x v s)
    false
    (assoc s x v)))

(defn seq? [x] (or (list? x) (vec? x) (cons? x)))

(defn unify [u v s]
  (let [u (walk u s)
        v (walk v s)]
    (cond (= u v) s
          (and (lvar? u) (lvar? v) (= u v)) s
          (lvar? u) (ext-s u v s)
          (lvar? v) (ext-s v u s)
          (and (seq? u) (seq? v))
          (let [s (unify (head u) (head v) s)]
            (and s (unify (next u) (next v) s)))
          :else false)))

(unify (lvar 0) 2 {})

(defn call/fresh [f]
  (fn [s'c]
    (let [c (C s'c)]
      ((f (lvar c)) [(S s'c) (inc c)]))))

(def mzero nil)
(defn unit [s'c] (cons s'c mzero))

(defn === [u v]
  (fn [s'c]
    (if-let [s (unify u v (S s'c))]
      (unit [s (C s'c)])
      mzero)))

(=== (lvar 0) "hello")
(*1 empty-state)

(defn mplus [$1 $2]
  (cond (nil? $1) $2
        (fn? $1) (fn [] (mplus $2 ($1)))
        :else (cons (head $1) (mplus $2 (next $1)))))

(defn bind [$ g]
  (cond (nil? $) mzero
        (fn? $) (fn [] (bind ($) g))
        :else (mplus (g (head $)) (bind (next $) g))))

(defn and* [g1 g2] (fn [a] (bind (g1 a) g2)))
(defn or* [g1 g2] (fn [a] (mplus (g1 a) (g2 a))))

(defmacro Zzz [g] `(fn [a] (fn [] (~g a))))

(defmacro or+ [g & gs]
  (if (empty? gs)
    g
    `(or* ~g (or+ ~@gs))))

(defmacro and+ [g & gs]
  (if (empty? gs)
    g
    `(and* ~g (and+ ~@gs))))

(defmacro conde [& clauses]
  `(or+ ~@(for [gs clauses] `(and+ ~@gs))))

(defmacro fresh [lvars & gs]
  (if (empty? lvars)
    `(and+ ~@gs)
    `(call/fresh
      (fn [~(head lvars)]
        (fresh ~(next lvars) ~@gs)))))

(empty-state* (fresh [x y z] (=== x 1) (=== y 2) (=== z 3)))

(defn pull [$]
  (if (fn? $)
    (pull ($))
    $))

(defn pull-all [$]
  (when-let [$ (pull $)]
    (cons (head $) (pull-all (next $)))))

(defn pull-n [n $]
  (when-let [$ (and (> n 0) (pull $))]
    (cons (head $) (pull-n (dec n) (next $)))))

(defn reify-s [v s]
  (let [v (walk v s)]
    (if-not (seq? v)
      s
      (reify-s (next v) (reify-s (head v) s)))))

(defn walk* [v s]
  (let [v (walk v s)]
    (cond (lvar? v) v
          (seq? v) (cons (walk* (head v) s)
                         (walk* (next v) s))
          :else v)))

(defn reify-state-1 [$]
  (let [v (walk* (lvar 0) (head $))]
    (walk* v (reify-s v {}))))

(defn mK-reify [a]
  (map reify-state-1 a))

(defmacro run [n xs & gs]
  `(mK-reify (pull-n ~n (empty-state* (fresh ~xs ~@gs)))))

(defmacro run* [xs & gs]
  `(mK-reify (pull-all (empty-state* (fresh ~xs ~@gs)))))

(run* [q] (=== q true))
(run* [q] (=== q true) (=== q 1))
(run* [q] (or+ (=== q 1) (=== q 2) (=== q 3)))
;; SQL WHERE q = 1 OR q = 2 OR q = 3

(defn fives [q] (or+ (=== q 5) (Zzz (fives q))))
(run 7 [q] (fives q))

(defn sixes [q] (or+ (=== q 6) (Zzz (sixes q))))
(run 3 [q] (sixes q))

(defn fives-or-sixes [q] (or+ (fives q) (sixes q)))
(run 6 [q] (fives-or-sixes q))

(defn hot-dogo [meal]
  (or+ (=== "dog" meal)
       (fresh [res]
         (=== (cons "hot" res) meal)
         (Zzz (hot-dogo res)))))

(run 4 [meal] (hot-dogo meal))

(pull-n 3 (empty-state* (call/fresh hot-dogo)))










;; HTML Templates
;; --------------

(defn ttail [xs] (tail (tail xs)))

(defmacro concat [& xs]
  (if (empty? xs)
    ""
    `(append ~(head xs) (concat ~@(tail xs)))))

(defn attr* [attr-list acc]
  (if (empty? attr-list)
    acc
    (attr* (ttail attr-list)
           (concat acc " "
                   (name (first attr-list)) "=\""
                   (second attr-list) "\""))))

(defn attr [attr-map]
  (attr* (seq attr-map) ""))

(defn html* [elem acc]
  (cond (str? elem) (append acc elem)
        (list? elem) (fold html* "" elem)
        (vec? elem)
        (let* [tag (name (head elem))
               t (tail elem)
               a (when-not (empty? t)
                  (let [x (head t)]
                    (when (map? x) x)))
               t (if a (tail t) t)]
          (concat acc "<" tag (if-not a "" (attr a)) ">"
                  (fold html* "" t)
                  "</" tag ">"))
        :else (do (print "Invalid HTML") undefined)))

(defn html [& elem] (html* elem ""))

(html
 [:head
  [:title "Hello HTML World"]
  [:script {:src "app.js"}]]
 [:body
  [:main
   [:header [:h1 "My App"]]
   [:content {:class "front-page"} "Hello World!"]
   [:footer [:a {:href "#"} "Back to top"]]]])

(let [user {:username "Mr. World"}]
  (html [:div {:class "user-button"}
         (if user
           (concat "Welcome, " (get user :username))
           [:a {:href "/login"} "Login"])]))

(defn html-list [& elements]
  (html [:ul (for [x elements]
               [:li x])]))
(html-list "Hello" "List" "World")
(html-list "a" "b" "c" (concat "hello " (name 'html-list)))


;; More advanced:
;; Convert :div.class-a.class-b into <div class="class-a class-b">
;; Convert :div#id into <div id="id">
;; Support both, :div#app.container






;; Y Not?

;; One of the most interesting concepts in computer science,
;; the implementation of recursivity:
(defn Y [f]
  ((fn [g] (f (fn [x] ((g g) x))))
   (fn [g] (f (fn [x] ((g g) x))))))

;; Another way to look at it, 'y is a function calling itself
(defn Y [f]
  (let [y (fn [g] (f (fn [x] ((g g) x))))]
    (y y)))


(def fact
  (Y (fn [fact]
       (fn [n]
         (if (zero? n) 1 (* n (fact (dec n))))))))

(def fib
  (Y (fn [fib]
       (fn [n]
         (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))))))

(fact 5)
(map fact [1 2 3 4 5 6 7 8])

(fib 6)
(map fib [2 3 4 5 6 7 8 9])

(((fn [f]
    ((fn [g] (f (fn [x] ((g g) x))))
     (fn [g] (f (fn [x] ((g g) x))))))
  (fn [fib]
    (fn [n]
      (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))))
 9)

"""

// Interactive F# is *REALLY* nice here:
// - Alt+Enter sends the current line, or the selection, to FSI. Nothing new.
// - Starting the REPL however, "hijacks" standard input until :quit is typed.
// - We effectively get a basic in-editor Lisp REPL, for free, which is nice.
// - The main downside is it only works from a *.fs file, so no highlighting.
