namespace AoC2019

open Utils

module Day02 =
    let processInput (inputLine: string) : int list =
        inputLine
        |> commas
        |> Array.map int
        |> List.ofArray

    let operate (op: int -> int -> int) (pc: int) (program: int list) : int list =
        let op1 = program[program[pc + 1]]
        let op2 = program[program[pc + 2]]
        let pos = program[pc + 3]
        List.updateAt pos (op op1 op2) program

    let rec executeInstruction (pc: int) (program: int list) : int list =
        let currentInstruction = program[pc]

        match currentInstruction with
        | 1 -> executeInstruction (pc + 4) (operate (+) pc program)
        | 2 -> executeInstruction (pc + 4) (operate (*) pc program)
        | 99 -> program
        | i -> failwith $"{i} at position {pc} is not a valid instruction"

    let runProgram (program: int list) : int list = executeInstruction 0 program

    let modifyProgram (noun: int) (verb: int) (program: int list) : int list =
        let program' = List.updateAt 1 noun program
        List.updateAt 2 verb program'

    let part1 (input: string) : string =
        input
        |> processInput
        |> modifyProgram 12 2
        |> runProgram
        |> List.head
        |> string

    let part2helper (program: int list) ((noun, verb): (int * int)) : Option<int * int> =
        let result =
            program
            |> modifyProgram noun verb
            |> runProgram
            |> List.head

        if result = 19690720 then
            Some(noun, verb)
        else
            None

    let part2 (input: string) : string =
        let program = input |> processInput

        List.allPairs [ 0..99 ] [ 0..99 ]
        |> List.map (part2helper program)
        |> List.filter Option.isSome
        |> List.head
        |> Option.get
        |> (fun (noun, verb) -> (100 * noun) + verb)
        |> string
