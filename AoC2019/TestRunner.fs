namespace AoC2019

open System.IO
open System.Diagnostics

module TestRunner =

    let getInput day =
        let filename = $"..\..\..\inputs\input_day{day}.txt"

        if File.Exists filename then
            filename
            |> File.ReadAllText
            |> String.filter (fun c -> c <> '\r')
        else
            failwith $"Input file {filename} not found"

    let makeComparison (expectedResults: string []) (results: string []) =
        Array.zip results expectedResults
        |> Array.map (fun (r, e) -> (r, e, r = e))

    let printStatus ((res, expectedRes, success): string * string * bool) =
        printfn "%s! Got %s, expected %s." (if success then "Success" else "Failure") res expectedRes

    let run (examples: string []) expectedResults realInput (func: string -> string) title =
        printfn title

        if examples.Length = 0 then
            printfn "No examples found, running the real input..."
        else
            printfn "Running and verifying examples before the real input..."

        let resultList =
            examples
            |> Array.map func
            |> makeComparison expectedResults

        resultList |> Array.map printStatus |> ignore

        let examplesSuccessful =
            resultList
            |> Array.fold (fun b1 (_, _, b2) -> b1 && b2) true

        if examplesSuccessful then
            printfn "All examples were successful, running the real input..."
            let timer = new Stopwatch()
            timer.Start()
            printfn "Result from real input: %s" (func realInput)
            timer.Stop()
            printfn "Time elapsed: %A" timer.Elapsed
        else
            printfn "Some examples were not successful. PLEASE DO BETTER"

        printfn ""

    // Day1
    let input1 = getInput 1

    let examples1_1 = [| "12"; "14"; "1969"; "100756" |]

    let exampleResults1_1 = [| "2"; "2"; "654"; "33583" |]

    let examples1_2 = [| "14"; "1969"; "100756" |]

    let exampleResults1_2 = [| "2"; "966"; "50346" |]

    // Day2
    let input2 = getInput 2

    let examples2_1 = [||]
    //[| "1,0,0,0,99"; "2,3,0,3,99"; "2,4,4,5,99,0"; "1,1,1,4,99,5,6,0,99" |]

    let exampleResults2_1 = [||]
    //[| "2,0,0,0,99"; "2,3,0,6,99"; "2,4,4,5,99,9801"; "30,1,1,4,2,5,6,0,99" |]

    let examples2_2 = [||]

    let exampleResults2_2 = [||]

    // Day3
    let input3 = getInput 3

    let examples3_1 =
        [| "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
           "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7" |]

    let exampleResults3_1 = [| "159"; "135" |]

    let examples3_2 = examples3_1

    let exampleResults3_2 = [| "610"; "410" |]

    // Day4
    let input4 = getInput 4

    let examples4_1 = [||]

    let exampleResults4_1 = [||]

    let examples4_2 = examples4_1

    let exampleResults4_2 = [||]
