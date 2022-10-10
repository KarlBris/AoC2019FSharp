namespace AoC2019

open Utils
open IntcodeComputer

module Day07 =

    let rec runWithSettings (acc: int) (programs : ProgramState list) (settings : int list) : int =
    
        let in1 = if acc = 0 then [settings[0]; acc] else [acc]
        let output1 = addInputsToState in1 programs[0] |> runProgramState  
        let in2 = if acc = 0 then [settings[1]; output1|> getOutputsFromState |> List.last] else [output1|> getOutputsFromState |> List.last]
        let output2 = addInputsToState in2 programs[1] |> runProgramState
        let in3 = if acc = 0 then [settings[2]; output2|> getOutputsFromState |> List.last] else [output2|> getOutputsFromState |> List.last]
        let output3 = addInputsToState in3 programs[2] |> runProgramState
        let in4 = if acc = 0 then [settings[3]; output3|> getOutputsFromState |> List.last] else [output3|> getOutputsFromState |> List.last]
        let output4 = addInputsToState in4 programs[3] |> runProgramState
        let in5 = if acc = 0 then [settings[4]; output4|> getOutputsFromState |> List.last] else [output4|> getOutputsFromState |> List.last]
        let output5 = addInputsToState in5 programs[4] |> runProgramState

        if getWaitingFromState output5 then
            runWithSettings (output5 |> getOutputsFromState |> List.last) [output1; output2; output3; output4; output5] settings
        else
            output5 |> getOutputsFromState |> List.last

    let part1 (input: string) : string =
        let program = input|> parseProgram
        let permutations = permutations [0..4]

        permutations |> Seq.map (runWithSettings 0 (List.replicate 5 (makeInitialState [] program))) |> Seq.max |> string

    let part2 (input: string) : string =
        let program = input|> parseProgram
        let permutations = permutations [5..9]
        
        permutations |> Seq.map (runWithSettings 0 (List.replicate 5 (makeInitialState [] program))) |> Seq.max |> string
