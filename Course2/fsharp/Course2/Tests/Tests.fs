module Tests

open NUnit.Framework
open Program

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
    let (w, _) = walkUntilCycle graph (Walk [0])
    let printedWalk = Walk.print string w
    printfn "%s" printedWalk

[<Test>]
let ``euler cycle`` () =
    let graph = DirectedGraph.parse int sampleText
    let cycle = eulerCycle graph
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

    let walk = eulerPath graph

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
        |> splitLines
        |> Seq.map trim
        |> Seq.filter (not << System.String.IsNullOrWhiteSpace)
        |> Seq.toArray
    let graph : DirectedGraph<string> = 
        deBruijnGraph (Seq.toArray >> System.String) k dnas
        |> DirectedGraph.map (Seq.toArray >> System.String)
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
    let reconstructed = stringReconstruction k dnas

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