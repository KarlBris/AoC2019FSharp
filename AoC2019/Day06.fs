namespace AoC2019

open Utils

module Day06 =

    type OrbitMap =
        { Name: string
          Children: OrbitMap list }

    let rec makeTree (orbits: OrbitMap) (pairs: (string * string) list) : OrbitMap =
        let name = orbits.Name
        let (matching, nonmatching) = List.partition (fun (l, r) -> l = name) pairs

        let newChildren =
            matching
            |> List.map (fun (_, r) -> makeTree ({ Name = r; Children = [] }) nonmatching)

        { Name = name; Children = newChildren }

    let processInputs (input: string) : OrbitMap =
        input
        |> lines
        |> Array.map (fun entry -> twoArrayToTuple (entry.Split ')'))
        |> List.ofArray
        |> makeTree ({ Name = "COM"; Children = [] })

    let rec calculateOrbits (level: int) (orbits: OrbitMap) : int =
        let value =
            orbits.Children
            |> List.map (calculateOrbits (level + 1))
            |> List.sum

        value + level

    let part1 (input: string) : string =
        input
        |> processInputs
        |> calculateOrbits 0
        |> string

    let rec distanceTo (acc: int) (name: string) (orbits: OrbitMap) : int option =
        if orbits.Name = name then
            Some(acc - 1)
        else
            match orbits.Children with
            | [] -> None
            | children ->
                children
                |> List.map (distanceTo (acc + 1) name)
                |> List.filter (Option.isSome)
                |> List.tryHead
                |> Option.flatten

    let rec canFindName (name: string) (orbits: OrbitMap) : bool =
        if orbits.Name = name then
            true
        else
            match orbits.Children with
            | [] -> false
            | children ->
                children
                |> List.map (canFindName name)
                |> List.exists (id)

    let rec findClosestRoot (names: string list) (orbits: OrbitMap) : OrbitMap =
        let childenThatCanFindAllNames =
            orbits.Children
            |> List.filter (fun child ->
                names
                |> List.map (fun name -> canFindName name child)
                |> List.forall id)

        match childenThatCanFindAllNames with
        | [] -> orbits
        | c :: _ -> findClosestRoot names c

    let rec calculateDistance (orbits: OrbitMap) : int =
        let closestRoot = orbits |> findClosestRoot [ "YOU"; "SAN" ]
        let distanceToYou = closestRoot |> distanceTo 0 "YOU" |> Option.get
        let distanceToSan = closestRoot |> distanceTo 0 "SAN" |> Option.get
        distanceToSan + distanceToYou

    let part2 (input: string) : string =
        input
        |> processInputs
        |> calculateDistance
        |> string
