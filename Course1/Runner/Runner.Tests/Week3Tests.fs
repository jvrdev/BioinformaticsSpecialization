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

