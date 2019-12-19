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
    let cycle = euler graph
    let printedWalk = Walk.print string cycle
    printfn "%s" printedWalk