namespace Tests

open NUnit.Framework
open Bio
open Bio.Week3

[<TestFixture>]
type Week3Tests () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.MotifEnumeration () =
        let expectedOutput = [|"ATA";"ATT";"GTT";"TTT"|] |> Array.map Genome.ofString
        let dnaLines = [|"ATTTGGC";"TGCCTTA";"CGGTATC";"GAAAATT"|] |> Array.map Genome.ofString
        let output = motifEnumeration 3 1 dnaLines
        Assert.SequenceEquals expectedOutput output

    [<Test>]
    member this.GreedyMotifSearch () =
        let expectedOutput = [|"CAG";"CAG";"CAA";"CAA";"CAA"|] |> Array.map Genome.ofString
        let dnaLines = [|"GGCGTTCAGGCA";"CAAGGAGTTCGC";"CACGTCAATCAC";"CAATAATATTCG"|] |> Array.map Genome.ofString
        let output = greedyMotifSearch 3 5 dnaLines
        Assert.SequenceEquals expectedOutput output