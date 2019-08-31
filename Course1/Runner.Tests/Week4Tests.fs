namespace Tests

open NUnit.Framework
open Bio
open Bio.Week4

[<TestFixture>]
type Week4Tests () =
    [<Test>]
    member this.GreedyMotifSearchWithPseudoCount () =
        let expectedOutput =
            [|
                "TCTCGGGG"
                "CCAAGGTG"
                "TACAGGCG"
                "TTCAGGTG"
                "TCCACGTG"
            |] 
            |> Array.map Genome.OfString
        let dnaLines = 
            [|
                "CGCCCCTCTCGGGGGTGTTCAGTAAACGGCCA"
                "GGGCGAGGTATGTGTAAGTGCCAAGGTGCCAG"
                "TAGTACCGAGACCGAAAGAAGTATACAGGCGT"
                "TAGATCAAGTTTCAGGTGCACGTCGGTGAACC"
                "AATCCACCAGCTCCACGTGCAATGTTGGCCTA"
            |]
            |> Array.map Genome.OfString
        let output = randomizedMotifSearch 8 5 dnaLines
        Assert.SequenceEquals expectedOutput output

    [<Test>]
    member this.RandomSample0 () =
        let actual = randomSample (List.replicate 10 0.1) 0.25
        Assert.AreEqual (3, actual)

    [<Test>]
    member this.RandomSample1 () =
        let actual = randomSample (List.replicate 10 0.1) 0.0
        Assert.AreEqual (1, actual)

    [<Test>]
    member this.RandomSample2 () =
        let actual = randomSample (List.replicate 10 0.1) 1.0
        Assert.AreEqual (10, actual)

    [<Test>]
    member this.GibbsSampler () =
        let expectedOutput =
            [|
                "TCTCGGGG"
                "CCAAGGTG"
                "TACAGGCG"
                "TTCAGGTG"
                "TCCACGTG"
            |] 
            |> Array.map Genome.OfString
        let dnaLines = 
            [|
                "CGCCCCTCTCGGGGGTGTTCAGTAAACGGCCA"
                "GGGCGAGGTATGTGTAAGTGCCAAGGTGCCAG"
                "TAGTACCGAGACCGAAAGAAGTATACAGGCGT"
                "TAGATCAAGTTTCAGGTGCACGTCGGTGAACC"
                "AATCCACCAGCTCCACGTGCAATGTTGGCCTA"
            |]
            |> Array.map Genome.OfString
        let output = gibbsSampler 8 5 100 dnaLines
        Assert.SequenceEquals expectedOutput output