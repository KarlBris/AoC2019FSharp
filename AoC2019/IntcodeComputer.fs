namespace AoC2019

open Utils

module IntcodeComputer =

    type Program = int list

    type ProgramState = Program * int //TODO: inputs and outputs as well

    let parseProgram (inputLine: string) : Program =
        inputLine
        |> commas
        |> Array.map int
        |> List.ofArray

    type ParameterMode =
        | Position
        | Immediate

    type Parameter =
        | First
        | Second
        | Third

    let intToParameterMode (i: int) : ParameterMode = if i = 0 then Position else Immediate

    type ParameterModes =
        { FirstParamMode: ParameterMode
          SecondParamMode: ParameterMode
          ThirdParamMode: ParameterMode }

    type Opcode = { Code: int; Modes: ParameterModes }

    let parseOpcode (opcode: int) : Opcode =
        { Code = eMod opcode 100
          Modes =
            { FirstParamMode = intToParameterMode (eMod (opcode / 100) 10)
              SecondParamMode = intToParameterMode (eMod (opcode / 1000) 10)
              ThirdParamMode = intToParameterMode (eMod (opcode / 10000) 10) } }

    let getValue (modes: ParameterModes) (parameter: Parameter) (pc: int) (program: Program) : int =
        let (mode, position) =
            match parameter with
            | First -> (modes.FirstParamMode, pc + 1)
            | Second -> (modes.SecondParamMode, pc + 2)
            | Third -> (modes.ThirdParamMode, pc + 3)

        match mode with
        | Position -> program[program[position]]
        | Immediate -> program[position]

    let operate (modes: ParameterModes) (op: int -> int -> int) (pc: int) (program: Program) : ProgramState =
        let op1 = getValue modes First pc program
        let op2 = getValue modes Second pc program
        let pos = program[pc + 3]
        (List.updateAt pos (op op1 op2) program, pc + 4)

    let input (pc: int) (program: Program) : ProgramState =
        let inputVar = System.Console.ReadLine() |> int
        let pos = program[pc + 1]
        (List.updateAt pos inputVar program, pc + 2)

    let output (modes: ParameterModes) (pc: int) (program: Program) : ProgramState =
        let data = getValue modes First pc program
        System.Console.WriteLine data
        (program, pc + 2)

    let jump (modes: ParameterModes) (checkWithZero: int -> int -> bool) (pc: int) (program: Program) : ProgramState =
        let checkVal = getValue modes First pc program
        let pos = getValue modes Second pc program

        let pc' =
            if checkWithZero checkVal 0 then
                pos
            else
                (pc + 3)

        (program, pc')

    let comp (modes: ParameterModes) (comparator: int -> int -> bool) (pc: int) (program: Program) : ProgramState =
        let val1 = getValue modes First pc program
        let val2 = getValue modes Second pc program
        let pos = program[pc + 3]

        let boolVal = if comparator val1 val2 then 1 else 0

        (List.updateAt pos boolVal program, pc + 4)

    let rec executeInstruction ((program, pc): ProgramState) : ProgramState =
        let currentInstruction = program[pc]
        let parsedInstruction = parseOpcode currentInstruction

        let (program', pc') =
            match parsedInstruction.Code with
            | 1 -> executeInstruction (operate parsedInstruction.Modes (+) pc program)
            | 2 -> executeInstruction (operate parsedInstruction.Modes (*) pc program)
            | 3 -> executeInstruction (input pc program)
            | 4 -> executeInstruction (output parsedInstruction.Modes pc program)
            | 5 -> executeInstruction (jump parsedInstruction.Modes (<>) pc program)
            | 6 -> executeInstruction (jump parsedInstruction.Modes (=) pc program)
            | 7 -> executeInstruction (comp parsedInstruction.Modes (<) pc program)
            | 8 -> executeInstruction (comp parsedInstruction.Modes (=) pc program)
            | 99 -> (program, pc)
            | i -> failwith $"{i} at position {pc} is not a valid instruction"

        (program', pc')

    let runProgram (program: Program) : Program = fst (executeInstruction (program, 0))
