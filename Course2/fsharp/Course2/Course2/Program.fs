
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
        |> Seq.map (
            trim
            >> splitS " -> " 
            >> (function
                | [|a;  b|] -> parseNode a, splitS "," b |> Seq.map parseNode |> Seq.toList
                | other -> failwith "Invalid line format, fields were %A" other))
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
        |> Seq.map printElem
        |> String.concat " -> "
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
    static member rotate (Walk xs) (n : int) =
        let length = List.length xs
        let n' = n % length
        Seq.concat [
            (xs |> Seq.skip n')
            (xs |> Seq.take n')
        ]
        |> Seq.toList
        |> Walk

let rec walkUntilCycle (graph : DirectedGraph<'a>) (walk : Walk<'a>) : Walk<'a> * DirectedGraph<'a> =
    match walk with
    | Walk [] -> invalidArg "walk" "Walk cannot be empty"
    | Walk (last::_) ->
        match DirectedGraph.walk last graph with
        | Some (_, next), graph' -> 
            walkUntilCycle graph' (Walk.append next walk)
        | None, graph' -> walk, graph'

let euler (graph : DirectedGraph<'a>) : option<Walk<'a>> =
    DirectedGraph.takeFirst graph 
    |> Option.map (
        fun first ->
            let walk', graph' = walkUntilCycle graph (Walk [first])
            if DirectedGraph.isEmpty graph'
            then walk'
            else 
                let (Walk ws) = walk'
                match ws |> Seq.tryFindIndex (fun n -> DirectedGraph.hasEdgesFor n graph') with
                | Some i ->
                    let walk'' = Walk.rotate
                    ()
                | None -> failwith
    )
    


//let walkCycle (x : DirectedGraph<'a>) : option<Walk<'a>> * DirectedGraph<'a> =

[<EntryPoint>]
let main argv =
    0