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

type DirectedGraph<'t> =
    | AdjacencyList of list<'t * list<'t>>
with
    static member map (f : 'u -> 'v) (g : DirectedGraph<'u>) : DirectedGraph<'v> =
        let (AdjacencyList records) = g
        let recordsPrime = 
            records
            |> List.map (fun (src, dsts) -> f src, dsts |> List.map f)
        AdjacencyList recordsPrime
    static member sort (AdjacencyList records : DirectedGraph<'a> when 'a : comparison) =
        let recordsPrime = 
            records
            |> Seq.map (fun (src, dsts) -> (src, dsts |> List.sort))
            |> Seq.sortBy fst
            |> Seq.toList
        AdjacencyList recordsPrime
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
    static member hasEdgesFor<'a when 'a : equality> (node : 'a) (AdjacencyList records : DirectedGraph<'a>) : bool =
        records
        |> Seq.exists (fst >> (=)node)
    static member edges (AdjacencyList records : DirectedGraph<'a>) : seq<'a * 'a> =
        records |> Seq.collect (fun (src, dsts) -> dsts |> Seq.map (fun dst -> (src, dst)))
    static member grades<'a when 'a : comparison> (AdjacencyList records : DirectedGraph<'a> as graph) : Map<'a, Grade> =
        let grades = 
            records
            |> Seq.map (fun (src, dsts) -> src, { InGrade = 0; OutGrade = dsts.Length })
            |> Map.ofSeq
        DirectedGraph.edges graph 
        |> Seq.fold
            (fun gradesPrime (_, dst) ->
                match Map.tryFind dst gradesPrime with
                | Some x -> Map.add dst { x with InGrade = x.InGrade + 1 } gradesPrime
                | None -> Map.add dst { InGrade = 1; OutGrade = 0 } gradesPrime
            )
            grades
    static member add (src : 'a, dst : 'a) (AdjacencyList records : DirectedGraph<'a>) : DirectedGraph<'a> =
        let zero = (false, [])
        let folder ((added : bool), (recordsPrime : list<'a * list<'a>>)) (srci : 'a, dstsi : list<'a>) : bool * list<'a * list<'a>> =
            if src = srci 
            then true, (srci, dst::dstsi) :: recordsPrime
            else added, (srci, dstsi) :: recordsPrime
        let added, recordsPrime = Seq.fold folder zero records
        AdjacencyList <| if added then recordsPrime else (src, [dst]) :: records
            
type Walk<'t> = Walk of list<'t>
with
    static member map (f : 'u -> 'v) (Walk x : Walk<'u>) : Walk<'v> = 
        x |> List.map f |> Walk
    // print with -> as separator
    static member print (printElem : 'a -> string) (Walk x) =
        x
        |> Seq.rev
        |> Seq.map printElem
        |> String.concat "->"
    /// print without separator
    static member print2 (printElem : 'a -> string) (Walk x) =
        x
        |> Seq.rev
        |> Seq.map printElem
        |> System.String.Concat
    static member toArray (Walk x) =
        x |> Seq.rev |> Seq.toArray
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
    static member rotate (n : int) (Walk xs : Walk<'a>) : Walk<'a> =
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

let deBruijnGraph<'t, 'a, 'c when 't :> seq<'a> and 'c : comparison > (comparer : seq<'a> -> 'c) (k : int) (kmers : seq<'t>) : DirectedGraph<seq<'a>> =
    let toEdge (x : seq<'a>) = (x |> Seq.take (k - 1), x |> Seq.skip 1)
    let records = 
        kmers
        |> Seq.map toEdge
        |> Seq.groupBy (fst >> comparer)
        |> Seq.map (fun (_, dsts) -> dsts |> Seq.head |> fst, dsts |> Seq.map snd |> Seq.toList)
        |> Seq.toList
    AdjacencyList records

let rec walkUntilCycle (graph : DirectedGraph<'a>) (walk : Walk<'a>) : Walk<'a> * DirectedGraph<'a> =
    match walk with
    | Walk [] -> invalidArg "walk" "Walk cannot be empty"
    | Walk (last::_) ->
        match DirectedGraph.walk last graph with
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

let pathToGenome (walk : Walk<seq<'a>>) : seq<'a> =
    let kmers = Walk.toArray walk
    Seq.append (Seq.head kmers) (Seq.tail kmers |> Seq.collect (Seq.last >> Seq.singleton))

let stringReconstruction (k : int) (kmers : seq<string>) : string =
    let db = 
        deBruijnGraph (Seq.toArray >> System.String) k kmers 
        |> DirectedGraph.map (Seq.toArray >> System.String)
    let path = eulerPath db |> Walk.map seq
    pathToGenome path |> Seq.toArray |> System.String

let cycleReconstruction (k : int) (kmers : seq<string>) : string =
    let db = 
        deBruijnGraph (Seq.toArray >> System.String) k kmers 
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

let stringSpelledByGappedPatterns (k : int) (d : int) (xs : seq<'a[]*'a[]>) : option<'a[]> =
    let l = Seq.length xs + k - 1
    let first = 
        xs 
        |> Seq.mapi (fun i (x, _) -> if i = 0 then seq x else Seq.singleton (Array.last x))
        |> Seq.collect id
    let second =
        xs 
        |> Seq.mapi (fun i (_, x) -> if i = 0 then seq x else Seq.singleton (Array.last x))
        |> Seq.collect id
    let overlapFirst = first |> Seq.skip (k + d)
    let overlapSecond = second |> Seq.take (l - k - d)

    if sequenceEquals overlapFirst overlapSecond then
        let prefix = first
        let suffix = second |> Seq.skip (l - k - d)
        Seq.append prefix suffix |> Seq.toArray |> Some
    else 
        None

let maximalNonBranchingPaths (AdjacencyList entries : DirectedGraph<'a> as graph) : Walk<'a>[] =
    let grades = DirectedGraph.grades graph
    let rec step (AdjacencyList g) (acc : list<Walk<'a>>) : list<Walk<'a>> =
        match g with
        | [] -> acc
        | (src, _)::t -> 
            let grades = Map.find src grades

            let gPrime, walk = walkNonBranchingPath g

    [||]

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

    let result = stringSpelledByGappedPatterns k d pairs

    result |> Option.map System.String |> Option.defaultValue ""

let runEuler (x : string) : string =
    let graph = DirectedGraph.parse int x
    let walk = eulerPath graph
    Walk.print string walk

let runStringReconstruction (x : string) : string =
    let k, dnas = readStringReconstruction x
    stringReconstruction k dnas

[<EntryPoint>] 
let main argv =
    runOnFile runStringSpelledByGappedPatterns """C:\src\BioinformaticsSpecialization\Course2\dataset_6206_4.txt"""
    0