module cliquecolouring.CliqueColouring

open System
open cliquecolouring.SatSolver
open System.Collections.Generic
    
let formulaForClique colours cliqueSize =
    let n = cliqueSize - 1
    let formulas = new List<list<int>>()
    
    for i in 0..cliqueSize - 1 do
        let iInd = i * n * colours 
        for j in i+1..cliqueSize - 1 do
            let jInd = j * colours
            let colourEdge = new ResizeArray<int>()
            for c1 in 0..colours - 1 do
                colourEdge.Add(iInd + jInd + c1)
                for c2 in c1+1..colours - 1 do
                    formulas.Add([-(iInd + jInd + c1); -(iInd + jInd + c2)])
            formulas.Add(Array.toList (colourEdge.ToArray()))
    
    for i in 0..cliqueSize - 1 do
        let iInd = i * n * colours 
        for j in i+1..cliqueSize - 1 do
            let jInd = j * colours 
            for k in j+1..cliqueSize - 1 do
                    for c in 0..colours - 1 do
                        formulas.Add([-(iInd + jInd + c); -(iInd + k * colours + c); -(jInd * n + k * colours + c)])
                
        
    formulas.ToArray() |> Array.toList
    
let DPLLForClique colours =
    let mutable cliqueSize =  colours + 1
    let mutable satResult = SAT
    while satResult = SAT do
        printfn "cliqueSize %A with %A colours" cliqueSize colours
        let edgesNumber = cliqueSize * (cliqueSize - 1) / 2
        let nconjs =  formulaForClique colours cliqueSize 
//        printfn "%A" nconjs.Length
//        for conj in  nconjs do
//            List.iter (fun x -> printf "%d " x) conj
//            printf "\n"
        let (sat, model) = DPLL(nconjs, Set.empty)
        if sat = SAT
        then 
            printfn "DPLL result: SAT"
            printfn "Model:" //" %A" model        
            model |> Seq.iter (fun x -> printf "%d " x)
            printfn ""
        else printfn "DPLL result: UNSAT\n"
        cliqueSize <- cliqueSize + 1
        satResult <- sat
    printfn "maximum clique size %A \n\n" (cliqueSize - 2)