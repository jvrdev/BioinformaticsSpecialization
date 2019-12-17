module Tests

open NUnit.Framework
open Program

let areEqual (expected : 't) (actual : 't) =
    Assert.AreEqual (expected, actual)


[<SetUp>]
let Setup () =
    ()

let sample : AdjacencyList<int> = 
    [
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
    let actual = parse int sampleText
    areEqual sample actual

[<Test>]
let ``print adjacency list`` () =
    let actual = print string sample
    areEqual sampleText actual
    