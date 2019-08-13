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