namespace Course2

type AdjacencyListEntry<'t> = 't * list<'t>
module AdjacencyListEntry =
    let edges (src, dsts) : seq<Edge<'t>> = 
        dsts |> Seq.map (fun dst -> (src, dst))

type DirectedGraph<'t> =
    | AdjacencyList of list<AdjacencyListEntry<'t>>
module DirectedGraph =
    let startWalk ((edgeSrc, edgeDst) : Edge<'u> as edge) (AdjacencyList entries) : DirectedGraph<'u> * Walk<'u> =
        let walk = Walk.ofStartEdge edge
        let graphMinusEdge =
            entries
            |> List.choose (
                fun (src, dsts) ->
                    if src <> edgeSrc then
                        match dsts with
                        | [] -> None
                        | other -> Some (src, dsts)
                    else
                        match dsts with
                        | [] -> None
                        | [singleDst] when singleDst = edgeDst -> None
                        | other -> Some (src, other |> List.filter ((<>)edgeDst))
            )
            |> AdjacencyList
        graphMinusEdge, walk
    let map (f : 'u -> 'v) (g : DirectedGraph<'u>) : DirectedGraph<'v> =
        let (AdjacencyList entries) = g
        let entriesPrime = 
            entries
            |> List.map (fun (src, dsts) -> f src, dsts |> List.map f)
        AdjacencyList entriesPrime
    let sort (AdjacencyList entries : DirectedGraph<'a> when 'a : comparison) =
        let entriesPrime = 
            entries
            |> Seq.map (fun (src, dsts) -> (src, dsts |> List.sort))
            |> Seq.sortBy fst
            |> Seq.toList
        AdjacencyList entriesPrime
    let print (printElem : 'a -> string) (AdjacencyList x) =
        x
        |> Seq.map (fun (a, b) -> sprintf "%s -> %s" (printElem a) (b |> Seq.map printElem |> String.concat ","))
        |> String.concat "\r\n"
    let parse (parseNode : string -> 'a) (s : string) : DirectedGraph<'a> =
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
    let takeFirst (AdjacencyList entries) : option<'a> =
        match entries with
        | (a, _)::_ -> Some a
        | [] -> None
    let walkFirstEdge<'a when 'a : equality> 
        (src : 'a) 
        (AdjacencyList entries : DirectedGraph<'a>)
        : option<Edge<'a>> * DirectedGraph<'a> =
        let zero = (None, [])
        let folder (edge, newRecords) (entrySrc, entryDsts as entry) =
            match edge with
            | Some x -> Some x, entry :: newRecords
            | None ->
                if src = entrySrc
                then 
                    match entryDsts with
                    | [] -> None, newRecords
                    | [entryDst] -> Some (src, entryDst), newRecords
                    | h :: t -> Some (src, h), (src, t) :: newRecords 
                else None, entry :: newRecords
        List.fold folder zero entries
        |> (fun (a, b) -> a , AdjacencyList b)
    let isEmpty (AdjacencyList entries : DirectedGraph<'a>) : bool =
        List.isEmpty entries
    let hasEdgesFor<'a when 'a : equality> (vertex : 'a) (AdjacencyList entries : DirectedGraph<'a>) : bool =
        entries
        |> Seq.exists (fst >> (=)vertex)
    let allEdgesFrom (vertex : 'a) (AdjacencyList entries : DirectedGraph<'a>) : seq<Edge<'a>> =
        entries 
        |> Seq.collect (fun ((src, _) as ale) -> 
            if src = vertex
            then AdjacencyListEntry.edges ale
            else Seq.empty
        )
    let allEdges (AdjacencyList entries : DirectedGraph<'a>) : seq<Edge<'a>> =
        entries |> Seq.collect AdjacencyListEntry.edges
    let allEdgesTo (vertex : 'a) : DirectedGraph<'a> -> seq<Edge<'a>> =
        allEdges >> Seq.filter (fun e -> Edge.destination e = vertex)
    // not required or finished
    let isConnected _ = true
    // not required or finished
    let bypass (u : 't, v : 't, w : 't) (g : DirectedGraph<'t>) : DirectedGraph<'t> =
        g
    let grades<'a when 'a : comparison> (AdjacencyList entries : DirectedGraph<'a> as graph) : Map<'a, Grade> =
        let grades = 
            entries
            |> Seq.map (fun (src, dsts) -> src, { InGrade = 0; OutGrade = dsts.Length })
            |> Map.ofSeq
        allEdges graph 
        |> Seq.fold
            (fun gradesPrime (_, dst) ->
                match Map.tryFind dst gradesPrime with
                | Some x -> Map.add dst { x with InGrade = x.InGrade + 1 } gradesPrime
                | None -> Map.add dst { InGrade = 1; OutGrade = 0 } gradesPrime
            )
            grades
    let add (src : 'a, dst : 'a) (AdjacencyList entries : DirectedGraph<'a>) : DirectedGraph<'a> =
        let zero = (false, [])
        let folder ((added : bool), (entriesPrime : list<'a * list<'a>>)) (srci : 'a, dstsi : list<'a>) : bool * list<'a * list<'a>> =
            if src = srci 
            then true, (srci, dst::dstsi) :: entriesPrime
            else added, (srci, dstsi) :: entriesPrime
        let added, entriesPrime = Seq.fold folder zero entries
        AdjacencyList <| if added then entriesPrime else (src, [dst]) :: entries
    let vertices (AdjacencyList entries : DirectedGraph<'a>) : list<'a> =
        entries
        |> Seq.collect (fun (src, dsts) -> src::dsts)
        |> Seq.distinct
        |> Seq.toList
