let sequenceEquals<'a when 'a : equality> (us : seq<'a>) (vs : seq<'a>) : bool =
    (Seq.length us) = (Seq.length vs) &&
        Seq.zip us vs |> Seq.forall (fun (a, b) -> a = b)

let splitS (separator : string) (s : string) : string[] =
    s.Split ([|separator|], System.StringSplitOptions.None)

let replaceS (u : string) (v : string) (s : string) : string =
    s.Replace (u, v)

let splitLines (s : string) : string[] =
    s |> replaceS "\r\n" "\n" |> splitS "\n"

let trim (s : string) =
    s.Trim ()

type Nucleobase = A | C | G | T

type Grade = { InGrade : int; OutGrade : int }
with
    static member balance {InGrade = a; OutGrade = b} = a - b

type Edge<'a> = 'a * 'a

module Edge =
    let source (a, _) = a
    let destination (_, b) = b

type ReadPair<'t> = 't * 't
module ReadPair = 
    let map f (a, b) = f a, f b

type Walk<'t> = Walk of list<'t>

module Walk = 
    let empty : Walk<'t> = Walk []
    let singleton (x : 'u) : Walk<'u> = Walk [x]
    let append (vertex : 'a) (Walk xs) =
        Walk (vertex :: xs)
    let ofStartEdge ((src, dst) : Edge<'u>) : Walk<'u> =
        singleton src |> append dst
    let map (f : 'u -> 'v) (Walk x : Walk<'u>) : Walk<'v> = 
        x |> List.map f |> Walk
    // print with -> as separator
    let print (printElem : 'a -> string) (Walk x) =
        x
        |> Seq.rev
        |> Seq.map printElem
        |> String.concat "->"
    /// print without separator
    let print2 (printElem : 'a -> string) (Walk x) =
        x
        |> Seq.rev
        |> Seq.map printElem
        |> System.String.Concat
    let toSeq (Walk x) =
        x |> Seq.rev
    let toArray = toSeq >> Seq.toArray
    let parse (parseElem : string -> 'a) (s : string) : Walk<'a> =
        s
        |> splitS " -> "
        |> Seq.map parseElem
        |> Seq.toList
        |> Walk
    //let has (vertex : 'a) (Walk xs) =
    //    List.contains vertex xs
    let rotate (n : int) (Walk xs : Walk<'a>) : Walk<'a> =
        let length = List.length xs
        let nPrime = n % length
        Seq.concat [
            (xs |> Seq.skip nPrime)
            (xs |> Seq.take nPrime)
        ]
        |> Seq.toList
        |> Walk
    let dropFirstIfCycle (x : Walk<'a>) : Walk<'a> =
        match x with
        | (Walk (h::t)) when List.last t = h -> Walk t
        | _ -> x
    let appendLast (Walk ws : Walk<'a> as x) : Walk<'a> =
        let last = List.last ws
        append last x
    let lastNode (Walk ns : Walk<'a>) : option<'a> = 
        match ns with
        | [] -> None
        | h::t -> Some h

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

type DeBruijnable<'t, 'c when 'c : comparison> = {
    Comparer : 't -> 'c
    Prefix : 't -> 't
    Suffix : 't -> 't
}

module DeBruijnable =
    module Instances =
        let mkSeqChar (k : int) =
            {
                Comparer = Seq.toArray >> System.String
                Prefix = Seq.take (k - 1)
                Suffix = Seq.skip 1
            }
        
        let mkString (k : int) : DeBruijnable<string, string>=
            {
                Comparer = id
                Prefix = fun s -> s.Substring (0, k - 1)
                Suffix = fun s -> s.Substring (1, k - 1)
            }
        
        // easy to have it not be just for strings but it does make things easier
        let mkReadPairString (k : int) : DeBruijnable<ReadPair<string>, string> =
            {
                Comparer = fun (a, b) -> a + b
                Prefix = ReadPair.map (fun s -> s.Substring (0, k - 1))
                Suffix = ReadPair.map (fun s -> s.Substring (1, k - 1))
            }

let deBruijnGraphGeneric 
    (ops : DeBruijnable<'t, 'c>)
    (kmers : seq<'t>)
    : DirectedGraph<'t> =
    let entries = 
        kmers
        |> Seq.map (fun x -> ops.Prefix x, ops.Suffix x)
        |> Seq.groupBy (fst >> ops.Comparer)
        |> Seq.map (fun (_, dsts) -> dsts |> Seq.head |> fst, dsts |> Seq.map snd |> Seq.toList)
        |> Seq.toList
    AdjacencyList entries

let rec walkUntilCycle (graph : DirectedGraph<'a>) (walk : Walk<'a>) : Walk<'a> * DirectedGraph<'a> =
    match walk with
    | Walk [] -> invalidArg "walk" "Walk cannot be empty"
    | Walk (last::_) ->
        match DirectedGraph.walkFirstEdge last graph with
        | Some (_, next), graphPrime -> 
            walkUntilCycle graphPrime (Walk.append next walk)
        | None, graphPrime -> walk, graphPrime

let rec eulerStep (remainingGraph : DirectedGraph<'a>) (progress : Walk<'a>) : Walk<'a> =
    let walkPrime, graphPrime = walkUntilCycle remainingGraph progress
    if DirectedGraph.isEmpty graphPrime
    then walkPrime
    else 
        let (Walk ws as nocycle) = Walk.dropFirstIfCycle walkPrime
        match ws |> Seq.tryFindIndex (fun n -> DirectedGraph.hasEdgesFor n graphPrime) with
        | Some i ->
            let walkPrimePrime = Walk.rotate i nocycle 
            eulerStep graphPrime walkPrimePrime
        | None -> invalidArg "graph" "Graph is not Eulerian"

let eulerCycle (graph : DirectedGraph<'a>) : Walk<'a> =
    match DirectedGraph.takeFirst graph with
    | Some first -> Walk.appendLast <| eulerStep graph (Walk [first])
    | None -> invalidArg "graph" "Graph is empty"

let eulerPath (graph : DirectedGraph<'a>) : Walk<'a> =
    let grades = DirectedGraph.grades graph
    let src = 
        grades
        |> Seq.find (fun kvp -> Grade.balance kvp.Value < 0)
        |> (fun x -> x.Key)
    let dst =
        grades
        |> Seq.find (fun kvp -> Grade.balance kvp.Value > 0)
        |> (fun x -> x.Key)
    let graphPrime = DirectedGraph.add (dst, src) graph
    let (Walk ws as walk) = 
        eulerStep graphPrime (Walk [src])
        |> Walk.dropFirstIfCycle
    let i = ws |> Seq.findIndex ((=)dst)
    Walk.rotate i walk

// not required or finished
let allEulerianCycles (graph : DirectedGraph<'a>) : seq<Walk<'a>> = 
    let eulerize (g : DirectedGraph<'a>) =
        let grades = DirectedGraph.grades g
        match grades |> Map.tryFindKey (fun _ v -> v.InGrade > 1) with
        | Some v ->
            DirectedGraph.allEdgesTo v g
            |> Seq.collect (fun (u, _) ->
                DirectedGraph.allEdgesFrom v g
                |> Seq.collect (fun (_, w) ->
                    let bypass = DirectedGraph.bypass (u, v, w) g
                    if DirectedGraph.isConnected bypass
                    then [bypass]
                    else []
                )
            )
        | None -> Seq.singleton g
    let graphs = [graph] |> Seq.collect eulerize |> Seq.toList
    graphs |> Seq.map eulerCycle

let pathToGenome (walk : Walk<seq<'a>>) : seq<'a> =
    let kmers = Walk.toArray walk
    Seq.append (Seq.head kmers) (Seq.tail kmers |> Seq.collect (Seq.last >> Seq.singleton))

let stringReconstruction (k : int) (kmers : seq<string>) : string =
    let db = 
        deBruijnGraphGeneric (DeBruijnable.Instances.mkString k) kmers 
        |> DirectedGraph.map (Seq.toArray >> System.String)
    let path = eulerPath db |> Walk.map seq
    pathToGenome path |> Seq.toArray |> System.String

let cycleReconstruction (k : int) (kmers : seq<string>) : string =
    let db = 
        deBruijnGraphGeneric (DeBruijnable.Instances.mkString k) kmers 
        |> DirectedGraph.map (Seq.toArray >> System.String)
    let path = eulerCycle db |> Walk.map seq
    pathToGenome path |> Seq.toArray |> System.String |> (fun s -> s.Substring(0, s.Length - k + 1))

let mkCombinations (xs : list<'a>) (length : int) : seq<'a[]> =
    let rec step (acc : list<list<'a>>) n : list<list<'a>> = 
        if n = 1 
        then acc
        else 
            step 
                (acc |> List.collect (fun u -> xs |> List.map (fun v -> v :: u)))
                (n - 1)
    if length < 1
    then Seq.empty
    else
        step (List.map List.singleton xs) length
        |> Seq.map List.toArray

let kUniversalCircularString (k : int) : string =
    let kmers = mkCombinations ['0'; '1'] k |> Seq.map System.String
    cycleReconstruction k kmers

type Reconstructable<'container, 'element> = {
    ToSeq : 'container -> seq<'element>
    Last : 'container -> 'element
    OfSeq : seq<'element> -> 'container
}

module Reconstructable =
    module Instances =
        let array = {
            ToSeq = Array.toSeq
            Last = Array.last
            OfSeq = Array.ofSeq
        }
        let string : Reconstructable<string, char> = {
            ToSeq = seq
            Last = fun s -> s.[s.Length - 1]
            OfSeq = Seq.toArray >> System.String
        }

let stringSpelledByReadPairs 
    (ops : Reconstructable<'container, 'element>) 
    (k : int, d : int) 
    (xs : seq<ReadPair<'container>>)
    : option<'container> =
    let l = Seq.length xs + k - 1
    let first = 
        xs 
        |> Seq.mapi (fun i (x, _) -> if i = 0 then ops.ToSeq x else Seq.singleton (ops.Last x))
        |> Seq.collect id
    let second =
        xs 
        |> Seq.mapi (fun i (_, x) -> if i = 0 then ops.ToSeq x else Seq.singleton (ops.Last x))
        |> Seq.collect id
    let overlapFirst = first |> Seq.skip (k + d)
    let overlapSecond = second |> Seq.take (l - k - d)

    if sequenceEquals overlapFirst overlapSecond then
        let prefix = first
        let suffix = second |> Seq.skip (l - k - d)
        Seq.append prefix suffix |> ops.OfSeq |> Some
    else 
        None

let walkNonBranchingPaths (grades : Map<'a, Grade>) (graph : DirectedGraph<'a>) (vertex : 'a) :
    DirectedGraph<'a> * list<Walk<'a>> = 
    let dstEdges = DirectedGraph.allEdgesFrom vertex graph |> Seq.toList
    let rec stepWithinDstEdge (graph : DirectedGraph<'a>, acc : Walk<'a>) : DirectedGraph<'a> * Walk<'a> =
        match Walk.lastNode acc with
        | Some src -> 
            let lastGrade = Map.find src grades
            if lastGrade = { InGrade = 1; OutGrade = 1}
            then
                let maybeEdge, graph2 = DirectedGraph.walkFirstEdge src graph
                match maybeEdge with
                | Some (_, dst) -> stepWithinDstEdge (graph2, Walk.append dst acc)
                | None -> failwithf "Edge starting on %A not found" src
            else graph, acc            
        | None -> failwithf "Walk %A is empty" acc

    let rec step (dstEdgesLeft : list<Edge<'a>>) (graph : DirectedGraph<'a>, acc : list<Walk<'a>>) =
        match dstEdgesLeft with
        | [] -> graph, acc
        | edge::t -> 
            let graph1, walk1 = DirectedGraph.startWalk edge graph
            let graph2, walk2 = stepWithinDstEdge (graph1, walk1)
            step t (graph2, walk2::acc)
    step dstEdges (graph, [])

let maximalNonBranchingPaths (graph : DirectedGraph<'a>) : Walk<'a>[] =
    let grades = DirectedGraph.grades graph
    let rec stepNonCycles (vertices : list<'a>) (edges : DirectedGraph<'a>) (acc : list<Walk<'a>>)
        : DirectedGraph<'a> * list<Walk<'a>> =
        match vertices with
        | [] -> edges, acc
        | h::t -> 
            let grade = Map.find h grades
            if grade <> { InGrade = 1; OutGrade = 1} && grade.OutGrade > 0
            then 
                let edgesNext, walks = walkNonBranchingPaths grades edges h
                stepNonCycles t edgesNext (walks @ acc)
            else
                stepNonCycles t edges acc
    let nonCyclesGraph, nonCycles = stepNonCycles (DirectedGraph.vertices graph) graph []
    let rec stepCycles (edges : DirectedGraph<'a>) (acc : list<Walk<'a>>)
        : list<Walk<'a>> =
        let rec step (g : DirectedGraph<'a>, w : Walk<'a>) 
            : DirectedGraph<'a> * Walk<'a> =
            match Walk.lastNode w with
            | Some src -> 
                let maybeEdge, g2 = DirectedGraph.walkFirstEdge src g
                match maybeEdge with
                | Some (_, dst) -> step (g2, Walk.append dst w)
                | None -> g2, w           
            | None -> failwithf "Walk %A is empty" acc
        match DirectedGraph.takeFirst edges with
        | Some vertex ->
            let maybeEdge, edges2 = DirectedGraph.walkFirstEdge vertex edges
            match maybeEdge with
            | Some edge ->
                let walk = Walk.ofStartEdge edge
                let edges3, walk2 = step (edges2, walk)
                stepCycles edges3 (walk2 :: acc)
            | None -> failwithf "Edge for vertex %A was not found" vertex
        | None -> acc
    let cycles = stepCycles nonCyclesGraph []
    nonCycles @ cycles |> List.toArray

let contigGeneration (kmers : seq<string>) : seq<string> =
    match Seq.tryHead kmers with
    | None -> Seq.empty
    | Some kmer -> 
        let k = kmer.Length
        let graph = deBruijnGraphGeneric (DeBruijnable.Instances.mkString k) kmers 
        let walks = maximalNonBranchingPaths (graph |> DirectedGraph.map (Seq.toArray >> System.String))
        walks
        |> Seq.map (Walk.map seq >> pathToGenome >> Seq.toArray >> System.String)
        |> Seq.sort
   
let stringReconstructionFromReadPairs 
    (debruijnable : DeBruijnable<ReadPair<'t>, 'c>)
    (reconstructable : Reconstructable<'t, 'a>)
    (k : int, d : int)
    (readPairs : seq<ReadPair<'t>>) 
    : 't =
    let debruijn = deBruijnGraphGeneric debruijnable readPairs
    // the book suggests all eulerian cycles are required in this step but just euerian walk seems to work fine
    let cycle = eulerPath debruijn
    let reconstructed = 
        stringSpelledByReadPairs
            reconstructable
            (k - 1, d + 1)
            (Walk.toSeq cycle)
    match reconstructed with
    | Some x -> x
    | None -> failwithf "No string could be reconstructed from %A" cycle

let printSeq (print : 'a -> string) (xs : seq<'a>) : string =
    xs |> Seq.map print |> System.String.Concat

let printComposition (print : 'a -> string) (xs : seq<'a[]*'a[]>) : string =
    xs 
    |> Seq.map (fun (a, b) -> sprintf "(%s|%s)" (printSeq print a) (printSeq print b) )
    |> String.concat " "

let pairedComposition (k : int) (d : int) (xs : 'a[]) : seq<'a[]*'a[]> =
    let segment = System.ArraySegment xs
    let compositionAt i = segment.Slice(i, k).ToArray (), segment.Slice(i + k + d, k).ToArray ()
    [0 .. xs.Length - (k + k + d)]
    |> Seq.map compositionAt
    |> Seq.sort
    |> Seq.toArray
    |> seq

let readStringReconstruction =
    splitLines
    >> Seq.map trim
    >> Seq.filter (not << System.String.IsNullOrWhiteSpace)
    >> Seq.toArray
    >> (fun x -> Array.head x |> int, Array.tail x)

let readGappedPattern (s : string) = 
    match s.Split '|' with
    | [|first; second|] -> first.ToCharArray(), second.ToCharArray()
    | _ -> invalidArg "s" "unexpected number of | on input"

let readGappedPatterns = 
    splitLines
    >> Seq.map trim
    >> Seq.filter (not << System.String.IsNullOrWhiteSpace)
    >> Seq.toList
    >> function
       | numbers::rest -> 
            let k, d = 
                match numbers.Split ' ' with
                | [|a; b|] -> int a, int b
                | _ -> invalidArg "x" "unexpected number of ' ' on input"
            let pairs = List.map readGappedPattern rest 
            k, d, pairs
       | _ -> invalidArg "x" "unexpected number of lines received"

let runOnFile f path =
    let input = System.IO.File.ReadAllText path
    let result = f input
    System.IO.File.WriteAllText ("result.txt", result, System.Text.Encoding.ASCII)

let runStringSpelledByGappedPatterns (s : string) : string =
    let k, d, pairs = readGappedPatterns s

    let result = stringSpelledByReadPairs Reconstructable.Instances.array (k, d) pairs

    result |> Option.map System.String |> Option.defaultValue ""

let runEuler (x : string) : string =
    let graph = DirectedGraph.parse int x
    let walk = eulerPath graph
    Walk.print string walk

let runStringReconstruction (x : string) : string =
    let k, dnas = readStringReconstruction x
    stringReconstruction k dnas

let runMaximalNonBranchingPaths (x : string) : string =
    let graph = DirectedGraph.parse int x
    let actual = maximalNonBranchingPaths graph
    let formatted = 
        actual
        |> Seq.map (Walk.print string)
        |> Seq.sort
        |> String.concat "\r\n"
    formatted

let runContigGeneration (x : string) : string =
    let dnas = 
        x
        |> splitLines
        |> Seq.map trim
        |> Seq.filter (not << System.String.IsNullOrWhiteSpace)
        |> Seq.toArray
    let output = contigGeneration dnas 
    String.concat " " output

let runStringReconstructionFromReadPairs (s : string) : string =
    let k, d, pairs = readGappedPatterns s
    let result =
        stringReconstructionFromReadPairs
            (DeBruijnable.Instances.mkReadPairString k)
            (Reconstructable.Instances.string)
            (k, d) 
            (pairs |> Seq.map (ReadPair.map System.String))
    result

[<EntryPoint>] 
let main argv =
    runOnFile runStringReconstructionFromReadPairs """C:\src\BioinformaticsSpecialization\Course2\dataset_204_16.txt"""
    0