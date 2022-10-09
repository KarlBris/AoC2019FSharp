namespace AoC2019

open Utils
open IntcodeComputer

module Day05 =

    let part1 (input: string) : string =
        input
        |> parseProgram
        |> runProgram [ 1 ]
        |> getOutputsFromState
        |> List.last
        |> string

    let part2 (input: string) : string =
        input
        |> parseProgram
        |> runProgram [ 5 ]
        |> getOutputsFromState
        |> List.head
        |> string
