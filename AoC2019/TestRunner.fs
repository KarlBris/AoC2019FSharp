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

    // Day5
    let input5 = getInput 5

    let examples5_1 = [||]

    let exampleResults5_1 = [||]

    let examples5_2 = [||]
    let exampleResults5_2 = [||]

    // Day6
    let input6 = getInput 6

    let examples6_1 = [| "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L" |]

    let exampleResults6_1 = [| "42" |]

    let examples6_2 =
        [| "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN" |]

    let exampleResults6_2 = [| "4" |]

    // Day7
    let input7 = getInput 7

    let examples7_1 = [|"3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"; "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0";"3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"|]

    let exampleResults7_1 = [|"43210";"54321";"65210"|]

    let examples7_2 = [|"3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5";"3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"|]
    let exampleResults7_2 = [|"139629729";"18216"|]

    // Day7
    let input8 = getInput 8

    let examples8_1 = [||]

    let exampleResults8_1 = [||]

    let examples8_2 = [||]
    let exampleResults8_2 = [||]
