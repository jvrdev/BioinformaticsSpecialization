
let splitS (separator : string) (s : string) : string[] =
    s.Split ([|separator|], System.StringSplitOptions.None)

let replaceS (u : string) (v : string) (s : string) : string =
    s.Replace (u, v)

let splitLines (s : string) : string[] =
    s |> replaceS "\r\n" "\n" |> splitS "\n"

let trim (s : string) =
    s.Trim ()

type Nucleobase = A | C | G | T

type DirectedGraph<'a when 'a : equality> =
    | AdjacencyList of list<'a * list<'a>>
with 
    static member print (printElem : 'a -> string) (AdjacencyList x) =
        x
        |> Seq.map (fun (a, b) -> sprintf "%s -> %s" (printElem a) (b |> Seq.map printElem |> String.concat ","))
        |> String.concat "\n"
    static member parse (parseNode : string -> 'a) (s : string) : DirectedGraph<'a> =
        let lines = splitLines s
        lines 
        |> Seq.filter (not << System.String.IsNullOrWhiteSpace)
        |> Seq.map (
            trim
            >> splitS " -> " 
            >> (function
                | [|a;  b|] -> parseNode a, splitS "," b |> Seq.map parseNode |> Seq.toList
                | other -> failwithf "Invalid line format, fields were %A" other))
        |> Seq.toList
        |> AdjacencyList
    static member takeFirst (AdjacencyList records) : option<'a> =
        match records with
        | (a, _)::_ -> Some a
        | [] -> None
    static member walk<'a when 'a : equality> (src : 'a) (AdjacencyList records : DirectedGraph<'a>) : option<('a * 'a)> * DirectedGraph<'a> =
        let zero = (None, [])
        let folder (edge, newRecords) (recordSrc, recordDsts as record) =
            match edge with
            | Some x -> Some x, record :: newRecords
            | None ->
                if src = recordSrc
                then 
                    match recordDsts with
                    | [] -> None, newRecords
                    | [recordDst] -> Some (src, recordDst), newRecords
                    | h :: t -> Some (src, h), (src, t) :: newRecords 
                else None, record :: newRecords
        List.fold folder zero records
        |> (fun (a, b) -> a , AdjacencyList b)
    static member isEmpty (AdjacencyList records : DirectedGraph<'a>) : bool =
        List.isEmpty records
    static member hasEdgesFor (node : 'a) (AdjacencyList records : DirectedGraph<'a>) : bool =
        records
        |> Seq.exists (fst >> (=)node)

type Walk<'a when 'a : equality> = Walk of list<'a>
with
    static member print (printElem : 'a -> string) (Walk x) =
        x
        |> Seq.rev
        |> Seq.map printElem
        |> String.concat "->"
    static member parse (parseElem : string -> 'a) (s : string) : Walk<'a> =
        s
        |> splitS " -> "
        |> Seq.map parseElem
        |> Seq.toList
        |> Walk
    //static member has (node : 'a) (Walk xs) =
    //    List.contains node xs
    static member append (node : 'a) (Walk xs) =
        Walk (node :: xs)
    static member rotate (Walk xs : Walk<'a>) (n : int) : Walk<'a> =
        let length = List.length xs
        let nPrime = n % length
        Seq.concat [
            (xs |> Seq.skip nPrime)
            (xs |> Seq.take nPrime)
        ]
        |> Seq.toList
        |> Walk
    static member dropFirstIfCycle (x : Walk<'a>) : Walk<'a> =
        match x with
        | (Walk (h::t)) when List.last t = h -> Walk t
        | _ -> x
    static member appendLast (Walk ws : Walk<'a> as x) : Walk<'a> =
        let last = List.last ws
        Walk.append last x

let rec walkUntilCycle (graph : DirectedGraph<'a>) (walk : Walk<'a>) : Walk<'a> * DirectedGraph<'a> =
    match walk with
    | Walk [] -> invalidArg "walk" "Walk cannot be empty"
    | Walk (last::_) ->
        match DirectedGraph.walk last graph with
        | Some (_, next), graphPrime -> 
            walkUntilCycle graphPrime (Walk.append next walk)
        | None, grpahPrime -> walk, grpahPrime

let rec eulerStep (remainingGraph : DirectedGraph<'a>) (progress : Walk<'a>) : Walk<'a> =
    let walkPrime, graphPrime = walkUntilCycle remainingGraph progress
    if DirectedGraph.isEmpty graphPrime
    then walkPrime
    else 
        let (Walk ws as nocycle) = Walk.dropFirstIfCycle walkPrime
        match ws |> Seq.tryFindIndex (fun n -> DirectedGraph.hasEdgesFor n graphPrime) with
        | Some i ->
            let walkPrimePrime = Walk.rotate nocycle i 
            eulerStep graphPrime walkPrimePrime
        | None -> invalidArg "graph" "Graph is not Eulerian"

let euler (graph : DirectedGraph<'a>) : Walk<'a> =
    match DirectedGraph.takeFirst graph with
    | Some first -> Walk.appendLast <| eulerStep graph (Walk [first])
    | None -> invalidArg "graph" "Graph is empty"

let runOnFile f path =
    let input = System.IO.File.ReadAllText path
    let result = f input
    System.IO.File.WriteAllText ("result.txt", result, System.Text.Encoding.ASCII)

let runEuler (x : string) : string =
    let graph = DirectedGraph.parse int x
    let walk = euler graph
    Walk.print string walk

[<EntryPoint>]
let main argv =
    runOnFile runEuler """C:\src\BioinformaticsSpecialization\Course2\dataset_203_2.txt"""
    0