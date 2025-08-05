module Parser

open Lexer

type Expression =
    | Var of string
    | Not of Expression
    | And of Expression * Expression
    | Or of Expression * Expression
    | Implies of Expression * Expression
    | Biconditional of Expression * Expression
    | True
    | False

let mutable currentTokens: Token list = []

let consume (expected: Token) =
    match currentTokens with
    | head :: tail when head = expected -> currentTokens <- tail
    | head :: _ -> failwithf "Parser error: Expected %A but found %A" expected head
    | [] -> failwithf "Parser error: Expected %A but reached end of input" expected

let peek () : Token =
    match currentTokens with
    | head :: _ -> head
    | [] -> Eof

let rec parsePrimary () : Expression =
    match peek () with
    | Variable v ->
        consume (Variable v)
        Var v
    | LParen ->
        consume LParen
        let expr = parseBiconditional ()
        consume RParen
        expr
    | _ -> failwithf "Parser error: Expected variable or '(' but found %A" (peek ())

and parseNot () : Expression =
    if peek () = NotOp then
        consume NotOp
        Not(parseNot ())
    else
        parsePrimary ()

and parseAnd () : Expression =
    let mutable left = parseNot ()

    while peek () = AndOp do
        consume AndOp
        let right = parseNot ()
        left <- And(left, right)

    left

and parseOr () : Expression =
    let mutable left = parseAnd ()

    while peek () = OrOp do
        consume OrOp
        let right = parseAnd ()
        left <- Or(left, right)

    left

and parseImplies () : Expression =
    let left = parseOr ()

    if peek () = ImpliesOp then
        consume ImpliesOp
        let right = parseImplies ()
        Implies(left, right)
    else
        left

and parseBiconditional () : Expression =
    let mutable left = parseImplies ()

    while peek () = BiconditionalOp do
        consume BiconditionalOp
        let right = parseImplies ()
        left <- Biconditional(left, right)

    left

let parse (tokens: Token list) : Expression =
    currentTokens <- tokens
    let expr = parseBiconditional ()

    if peek () <> Eof then
        failwithf "Parser error: Unexpected tokens remaining after parsing: %A" currentTokens

    expr
