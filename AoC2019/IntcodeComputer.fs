namespace AoC2019

open Utils

module IntcodeComputer =

    type Program = int list

    type ProgramState = Program * int * int list * int list * bool

    let getProgramFromState (p, _, _, _, _) = p
    
    let getOutputsFromState (_, _, _, os, _) = os

    let getWaitingFromState (_, _, _, _, w) = w

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

    let operate
        (modes: ParameterModes)
        (op: int -> int -> int)
        ((program, pc, inputs, outputs, waiting): ProgramState)
        : ProgramState =
        let op1 = getValue modes First pc program
        let op2 = getValue modes Second pc program
        let pos = program[pc + 3]
        (List.updateAt pos (op op1 op2) program, pc + 4, inputs, outputs, waiting)

    let input ((program, pc, inputs, outputs, waiting): ProgramState) : ProgramState =
        let inputVar = List.head inputs
        let pos = program[pc + 1]
        (List.updateAt pos inputVar program, pc + 2, List.tail inputs, outputs, waiting)

    let output (modes: ParameterModes) ((program, pc, inputs, outputs, waiting): ProgramState) : ProgramState =
        let data = getValue modes First pc program
        (program, pc + 2, inputs, List.append outputs [ data ], waiting)

    let jump
        (modes: ParameterModes)
        (checkWithZero: int -> int -> bool)
        ((program, pc, inputs, outputs, waiting): ProgramState)
        : ProgramState =
        let checkVal = getValue modes First pc program
        let pos = getValue modes Second pc program

        let pc' =
            if checkWithZero checkVal 0 then
                pos
            else
                (pc + 3)

        (program, pc', inputs, outputs, waiting)

    let comp
        (modes: ParameterModes)
        (comparator: int -> int -> bool)
        ((program, pc, inputs, outputs, waiting): ProgramState)
        : ProgramState =
        let val1 = getValue modes First pc program
        let val2 = getValue modes Second pc program
        let pos = program[pc + 3]

        let boolVal = if comparator val1 val2 then 1 else 0

        (List.updateAt pos boolVal program, pc + 4, inputs, outputs, waiting)

    let rec executeInstruction ((program, pc, inputs, outputs, _) as state: ProgramState) : ProgramState =
        let currentInstruction = program[pc]
        let parsedInstruction = parseOpcode currentInstruction

        match parsedInstruction.Code with
        | 1 -> executeInstruction (operate parsedInstruction.Modes (+) state)
        | 2 -> executeInstruction (operate parsedInstruction.Modes (*) state)
        | 3 -> match inputs with
               | [] -> (program, pc, inputs, outputs, true)
               | _ -> executeInstruction (input state)
        | 4 -> executeInstruction (output parsedInstruction.Modes state)
        | 5 -> executeInstruction (jump parsedInstruction.Modes (<>) state)
        | 6 -> executeInstruction (jump parsedInstruction.Modes (=) state)
        | 7 -> executeInstruction (comp parsedInstruction.Modes (<) state)
        | 8 -> executeInstruction (comp parsedInstruction.Modes (=) state)
        | 99 -> (program, pc, inputs, outputs, false)
        | i -> failwith $"{i} at position {pc} is not a valid instruction"

    let runProgramState (state: ProgramState) : ProgramState =
        executeInstruction state

    let addInputsToState (inputs: int list) ((program, pc, oldInputs, outputs, waiting): ProgramState) : ProgramState =
        (program, pc, List.append inputs oldInputs, outputs, waiting)

    let makeInitialState (inputs: int list) (program:Program) : ProgramState=
        (program, 0, inputs, [], false)

    let runProgram (inputs: int list) (program: Program) : ProgramState =
        runProgramState (makeInitialState inputs program)