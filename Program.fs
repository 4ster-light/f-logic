let debug (formula: string) (tokens: Lexer.Token list) (expression: Parser.Expression) =
    printfn "- Formula:\n  - %s\n" formula
    printfn "- Tokens:\n%s\n" (tokens |> List.map (fun t -> "  - " + string t) |> String.concat "\n")
    printfn "- Expression:\n  - %s" (string expression)
    printfn ""

[<EntryPoint>]
let main argv =
    printfn "\nEnter a logical formula (e.g., P & Q -> R, !A | B):\n"
    let input = System.Console.ReadLine()
    printfn ""

    try
        let tokens = input |> Lexer.lex
        let expression = tokens |> Parser.parse

        if argv.Length > 0 && argv.[0] = "--debug" then
            (input, tokens, expression) |||> debug
        else
            (expression, input) ||> Evaluator.generateAndPrintTruthTable
            printfn ""
    with ex ->
        eprintfn "Error: %s" ex.Message

    0
