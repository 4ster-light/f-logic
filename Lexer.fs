module Lexer

type Token =
    | LParen
    | RParen
    | NotOp
    | AndOp
    | OrOp
    | ImpliesOp
    | BiconditionalOp
    | Variable of string
    | Eof

let lex (input: string) : Token list =
    let mutable i = 0
    let chars = input.ToCharArray()

    let peek (offset: int) : char option =
        if i + offset < chars.Length then
            Some chars[i + offset]
        else
            None

    let advance (count: int) = i <- i + count

    let rec loop (acc: Token list) : Token list =
        if i >= chars.Length then
            List.rev (Eof :: acc)
        else
            match chars[i] with
            | char when char |> System.Char.IsWhiteSpace ->
                advance 1
                loop acc
            | '(' ->
                advance 1
                loop (LParen :: acc)
            | ')' ->
                advance 1
                loop (RParen :: acc)
            | '!' ->
                advance 1
                loop (NotOp :: acc)
            | '&' ->
                advance 1
                loop (AndOp :: acc)
            | '|' ->
                advance 1
                loop (OrOp :: acc)
            | '-' ->
                if peek 1 = Some '>' then
                    advance 2
                    loop (ImpliesOp :: acc)
                else
                    failwithf $"Lexer error: Unexpected character '{chars[i]}' at position {i}. Expected '->'."
            | '<' ->
                if peek 1 = Some '-' && peek 2 = Some '>' then
                    advance 3
                    loop (BiconditionalOp :: acc)
                else
                    failwithf $"Lexer error: Unexpected character '{chars[i]}' at position {i}. Expected '<->'."
            | char when char |> System.Char.IsLetter && char |> System.Char.IsUpper ->
                advance 1
                loop (Variable(string char) :: acc)
            | char -> failwithf $"Lexer error: Unexpected character '{char}' at position {i}"

    loop []
