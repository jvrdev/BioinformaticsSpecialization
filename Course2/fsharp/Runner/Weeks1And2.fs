namespace Course2

module Weeks1And2 =
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
        Reconstruction.cycleReconstruction k kmers

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
            let graph = Debruijn.run (Debruijn.Instances.mkString k) kmers 
            let walks = maximalNonBranchingPaths (graph |> DirectedGraph.map (Seq.toArray >> System.String))
            walks
            |> Seq.map (Walk.map seq >> Reconstruction.pathToGenome >> Seq.toArray >> System.String)
            |> Seq.sort
   
    let stringReconstructionFromReadPairs 
        (debruijnable : DeBruijnable<ReadPair<'t>, 'c>)
        (reconstructable : Reconstructable<'t, 'a>)
        (k : int, d : int)
        (readPairs : seq<ReadPair<'t>>) 
        : 't =
        let debruijn = Debruijn.run debruijnable readPairs
        // the book suggests all eulerian cycles are required in this step but just euerian walk seems to work fine
        let cycle = Euler.eulerPath debruijn
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

