module Evaluator

open Parser

let rec evaluate (expr: Expression) (assignments: Map<string, bool>) : bool =
    match expr with
    | Var v ->
        match assignments.TryFind v with
        | Some value -> value
        | None -> failwithf $"Evaluation error: Variable '{v}' not found in assignments."
    | Not e -> not (evaluate e assignments)
    | And(e1, e2) -> evaluate e1 assignments && evaluate e2 assignments
    | Or(e1, e2) -> evaluate e1 assignments || evaluate e2 assignments
    | Implies(e1, e2) -> not (evaluate e1 assignments) || evaluate e2 assignments
    | Biconditional(e1, e2) -> evaluate e1 assignments = evaluate e2 assignments
    | True -> true
    | False -> false

let rec getVariables (expr: Expression) : Set<string> =
    match expr with
    | Var v -> Set.singleton v
    | Not e -> getVariables e
    | And(e1, e2)
    | Or(e1, e2)
    | Implies(e1, e2)
    | Biconditional(e1, e2) -> Set.union (getVariables e1) (getVariables e2)
    | True
    | False -> Set.empty

let generateAndPrintTruthTable (expr: Expression) (formulaString: string) =
    let vars = getVariables expr |> Set.toList |> List.sort
    let numVars = List.length vars
    let varHeaders = String.concat " | " vars

    printfn $"{varHeaders} | {formulaString}"
    let varSeparator = String.replicate varHeaders.Length "-"
    let formulaSeparator = String.replicate formulaString.Length "-"
    printfn $"{varSeparator}-+-{formulaSeparator}"

    let rec generateAssignments (index: int) (current: Map<string, bool>) : Map<string, bool> list =
        if index = numVars then
            [ current ]
        else
            let var = vars[index]
            let assignmentsWithTrue = generateAssignments (index + 1) (current.Add(var, true))
            let assignmentsWithFalse = generateAssignments (index + 1) (current.Add(var, false))
            List.append assignmentsWithTrue assignmentsWithFalse

    let allAssignments = generateAssignments 0 Map.empty

    for assignments in allAssignments do
        let varValues = vars |> List.map (fun v -> if assignments[v] then "T" else "F") |> String.concat " | "
        let result = if evaluate expr assignments then "T" else "F"
        printfn $"{varValues} | {result}"
