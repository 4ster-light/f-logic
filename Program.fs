let debug (formula : string) (tokens : Lexer.Token list) (expression : Parser.Expression) =
    printfn "- Formula:"
    printfn "  - %s" formula
    
    printfn "- Tokens:"
    for token in tokens do
        printfn "  - %A" token
    
    printfn "- Expression:"
    printfn "  - %A" expression

[<EntryPoint>]
let main argv =
    printfn "Enter a logical formula (e.g., P & Q -> R, !A | B):\n"
    let input = System.Console.ReadLine()
    printfn ""

    try
        let tokens = Lexer.lex input
        let expression = Parser.parse tokens

        if argv.Length > 0 && argv.[0] = "--debug" then
            debug input tokens expression
        else
            Evaluator.generateAndPrintTruthTable expression input
    with ex ->
        eprintfn "Error: %s" ex.Message

    0
