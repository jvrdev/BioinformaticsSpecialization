module Weeks1And2

open Course2
open NUnit.Framework
open Weeks1And2

let areEqual (expected : 't) (actual : 't) =
    Assert.AreEqual (expected, actual)

[<SetUp>]
let Setup () =
    ()

let sample : DirectedGraph<int> = 
    AdjacencyList [
        0, [3]
        1, [0]
        2, [1; 6]
        3, [2]
        4, [2]
        5, [4]
        6, [5; 8]
        7, [9]
        8, [7]
        9, [6]
    ]
    
let sampleText = """0 -> 3
1 -> 0
2 -> 1,6
3 -> 2
4 -> 2
5 -> 4
6 -> 5,8
7 -> 9
8 -> 7
9 -> 6"""
    
[<Test>]
let ``parse adjacency list`` () =
    let actual = DirectedGraph.parse int sampleText
    areEqual sample actual

[<Test>]
let ``print adjacency list`` () =
    let actual = DirectedGraph.print string sample
    areEqual sampleText actual
    
[<Test>]
let ``loop adjacency list`` () =
    let graph = AdjacencyList [
        0, [1]
        1, [2]
        2, [3]
        3, [4]
        4, [0]
    ]
    let (w, _) = Euler.walkUntilCycle graph (Walk [0])
    let printedWalk = Walk.print string w
    printfn "%s" printedWalk

[<Test>]
let ``euler cycle`` () =
    let graph = DirectedGraph.parse int sampleText
    let cycle = Euler.eulerCycle graph
    let printedWalk = Walk.print string cycle
    printfn "%s" printedWalk

[<Test>]
let ``graph grade calculation`` () =
    let graph = AdjacencyList [
        0, [1]
        1, [2]
        2, [3]
        3, [4]
        4, [0]
    ]
    let grades = DirectedGraph.grades graph

    let output = Map.ofList [
        0, { InGrade = 1; OutGrade = 1 }
        1, { InGrade = 1; OutGrade = 1 }
        2, { InGrade = 1; OutGrade = 1 }
        3, { InGrade = 1; OutGrade = 1 }
        4, { InGrade = 1; OutGrade = 1 }
    ]

    areEqual output grades


[<Test>]
let ``eulerian walk`` () =
    let text = """0 -> 2
     1 -> 3
     2 -> 1
     3 -> 0,4
     6 -> 3,7
     7 -> 8
     8 -> 9
     9 -> 6"""
    let graph = DirectedGraph.parse int text

    let walk = Euler.eulerPath graph

    let expectedWalk = "6->7->8->9->6->3->0->2->1->3->4"

    areEqual expectedWalk (Walk.print string walk)

[<Test>]
let ``debruijn graph`` () =
    let text = """GAGG
    CAGG
    GGGG
    GGGA
    CAGG
    AGGG
    GGAG"""
    
    let k = 4
    let dnas = 
        text
        |> String.splitLines
        |> Seq.map String.trim
        |> Seq.filter (not << System.String.IsNullOrWhiteSpace)
        |> Seq.toArray
    let graph : DirectedGraph<string> = 
        Debruijn.run (Debruijn.Instances.mkString k) dnas
        |> DirectedGraph.sort
    let expectedGraphText = """AGG -> GGG
    CAG -> AGG,AGG
    GAG -> AGG
    GGA -> GAG
    GGG -> GGA,GGG"""
    let expectedGraph = DirectedGraph.parse id expectedGraphText
    areEqual expectedGraph graph


[<Test>]
let ``string reconstruction`` () =
    let text = """4
     CTTA
     ACCA
     TACC
     GGCT
     GCTT
     TTAC"""

    let k, dnas = readStringReconstruction text
    let reconstructed = Reconstruction.stringReconstruction k dnas

    areEqual "GGCTTACCA" reconstructed

[<Test>]
let ``generate combinations`` () =
    let combinations = mkCombinations ["0"; "1"] 3

    areEqual 8 (Seq.length combinations)
    areEqual 3 (Seq.length <| Seq.head combinations)

[<Test>]
let ``cycle reconstruction`` () =
    let k = 3

    let actual = kUniversalCircularString k

    areEqual "10001011" actual

[<Test>]
let ``paired composition`` () =
    let actual = pairedComposition 3 2 ("TAATGCCATGGGATGTT".ToCharArray()) |> printComposition string

    areEqual "(AAT|CAT) (ATG|ATG) (ATG|ATG) (CAT|GAT) (CCA|GGA) (GCC|GGG) (GGG|GTT) (TAA|CCA) (TGC|TGG) (TGG|TGT)" actual

[<Test>]
let ``string spelled by gapped patterns`` () =
    let input = """4 2
    GACC|GCGC
    ACCG|CGCC
    CCGA|GCCG
    CGAG|CCGG
    GAGC|CGGA"""

    let k, d, pairs = readGappedPatterns input

    let actual = stringSpelledByReadPairs Reconstruction.Instances.array (k, d) pairs

    areEqual (Some "GACCGAGCGCCGGA") (actual |> Option.map System.String)


[<Test>]
let ``maximal non-braching paths`` () =
    let input = """1 -> 2
    2 -> 3
    3 -> 4,5
    6 -> 7
    7 -> 6"""

    let graph = DirectedGraph.parse int input
    let actual = maximalNonBranchingPaths graph
    let formatted = 
        actual
        |> Seq.map (Walk.print string)
        |> Seq.sort
        |> String.concat "\r\n"

    let expected = """1->2->3
3->4
3->5
7->6->7"""
    areEqual expected formatted

    
[<Test>]
let ``contig generation`` () =
    let text = """ATG
ATG
TGT
TGG
CAT
GGA
GAT
AGA"""
        
    let dnas = 
        text
        |> String.splitLines
        |> Seq.map String.trim
        |> Seq.filter (not << System.String.IsNullOrWhiteSpace)
        |> Seq.toArray
    let output = contigGeneration dnas 
    let expectedOutput = """AGA ATG ATG CAT GAT TGGA TGT"""
    let formattedOutput = String.concat " " output
    areEqual expectedOutput formattedOutput


[<Test>]
let ``string Reconstruction from Read-Pairs Problem`` () =
    let input = """4 2
GAGA|TTGA
TCGT|GATG
CGTG|ATGT
TGGT|TGAG
GTGA|TGTT
GTGG|GTGA
TGAG|GTTG
GGTC|GAGA
GTCG|AGAT"""
    
    let k, d, pairs = readGappedPatterns input
    
    let actual =
        stringReconstructionFromReadPairs
            (Debruijn.Instances.mkReadPairString k)
            (Reconstruction.Instances.string)
            (k, d) 
            (pairs |> Seq.map (ReadPair.map System.String))
    
    areEqual "GTGGTCGTGAGATGTTGA" actual