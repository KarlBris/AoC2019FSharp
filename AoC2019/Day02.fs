namespace AoC2019

open Utils
open IntcodeComputer

module Day02 =

    let modifyProgram (noun: int) (verb: int) (program: int list) : int list =
        let program' = List.updateAt 1 noun program
        List.updateAt 2 verb program'

    let part1 (input: string) : string =
        input
        |> parseProgram
        |> modifyProgram 12 2
        |> runProgram []
        |> getProgramFromState
        |> List.head
        |> string

    let part2helper (program: int list) ((noun, verb): (int * int)) : Option<int * int> =
        let result =
            program
            |> modifyProgram noun verb
            |> runProgram []
            |> getProgramFromState
            |> List.head

        if result = 19690720 then
            Some(noun, verb)
        else
            None

    let part2 (input: string) : string =
        let program = input |> parseProgram

        List.allPairs [ 0..99 ] [ 0..99 ]
        |> List.map (part2helper program)
        |> List.filter Option.isSome
        |> List.head
        |> Option.get
        |> (fun (noun, verb) -> (100 * noun) + verb)
        |> string
