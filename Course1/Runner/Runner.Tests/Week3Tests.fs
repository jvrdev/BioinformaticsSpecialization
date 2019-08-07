namespace Tests

open NUnit.Framework
open Bio
open Bio.Week3

[<TestFixture>]
type Week3Tests () =
    let nMatrix = 
        [
            "TCGGGGGTTTTT"
            "CCGGTGACTTAC"
            "ACGGGGATTTTC"
            "TTGGGGACTTTT"
            "AAGGGGACTTCC"
            "TTGGGGACTTCC"
            "TCGGGGATTCAT"
            "TCGGGGATTCCT"
            "TAGGGGAACTAC"
            "TCGGGTATAACC"
        ]
        |> Seq.map Genome.OfString
        |> Seq.toArray
        |> MotifMatrix.OfRows

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.MotifEnumeration () =
        let expectedOutput = [|"ATA";"ATT";"GTT";"TTT"|] |> Array.map Genome.OfString
        let dnaLines = [|"ATTTGGC";"TGCCTTA";"CGGTATC";"GAAAATT"|] |> Array.map Genome.OfString
        let output = motifEnumeration 3 1 dnaLines
        Assert.SequenceEquals expectedOutput output

    [<Test>]
    member this.MotifCount () =
        let expectedOutput = 
            [
                [2; 2; 0; 0; 0; 0; 9; 1; 1; 1; 3; 0]
                [1; 6; 0; 0; 0; 0; 0; 4; 1; 2; 4; 6]
                [0; 0; 10; 10; 9; 9; 1; 0; 0; 0; 0; 0]
                [7; 2; 0; 0; 1; 1; 0; 5; 8; 7; 3; 4]
            ]
            |> array2D
        let input = nMatrix

        let output = motifCount input
        Assert.AreEqual (expectedOutput, output)

    [<Test>]
    member this.MotifProfile () =
        let expectedOutput = 
            [
                [0.2; 0.2; 0.0; 0.0; 0.0; 0.0; 0.9; 0.1; 0.1; 0.1; 0.3; 0.0]
                [0.1; 0.6; 0.0; 0.0; 0.0; 0.0; 0.0; 0.4; 0.1; 0.2; 0.4; 0.6]
                [0.0; 0.0; 1.0; 1.0; 0.9; 0.9; 0.1; 0.0; 0.0; 0.0; 0.0; 0.0]
                [0.7; 0.2; 0.0; 0.0; 0.1; 0.1; 0.0; 0.5; 0.8; 0.7; 0.3; 0.4]
            ]
            |> array2D
        let input = nMatrix

        let output = (motifCount >> motifProfile) input
        for i = 0 to expectedOutput.GetLength 0 - 1 do
            for j = 0 to expectedOutput.GetLength 1 - 1 do
                Assert.AreEqual (expectedOutput.[i, j], output.[i, j], 0.001)

    [<Test>]
    member this.Entropy () =
        let expectedOutput = 1.371
        let input = [0.2; 0.6; 0.0; 0.2]

        let output = entropy input
        Assert.AreEqual (expectedOutput, output, 0.001)

    [<Test>]
    member this.MatrixEntropy () =
        let expectedOutput = 9.9162900053569718
        let input = nMatrix

        let output = motifEntropy input
        Assert.AreEqual (expectedOutput, output)

    [<Test>]
    member this.GenomeEnumerate () =
        let output = Genome.Enumerate 3
        Assert.IsNotNull output

    [<Test>]
    member this.MedianString () =
        let dna =
            [
                "AAATTGACGCAT"
                "GACGACCACGTT"
                "CGTCAGCGCCTG"
                "GCTGAGCACCGG"
                "AGTTCGGGACAG"
            ]
            |> Seq.map Genome.OfString
        let output = Week3.medianString 3 dna
        Assert.AreEqual (Genome.OfString "GAC", output)

    [<Test>]
    member this.ProfileMostProbableKmer () =
        let text = Genome.OfString "ACCTGTTTATTGCCTAAGTTCCGAACAAACCCAATATAGCCCGAGGGCCT"
        let k = 5
        let profile =
            [
                [0.2; 0.2; 0.3; 0.2; 0.3]
                [0.4; 0.3; 0.1; 0.5; 0.1]
                [0.3; 0.3; 0.5; 0.2; 0.4]
                [0.1; 0.2; 0.1; 0.1; 0.2]
            ]
            |> array2D
        let output = profileMostProbableKmer text k profile
        Assert.AreEqual (Genome.OfString "CCGAG", output)
        
    [<Test>]
    member this.GreedyMotifSearch () =
        let expectedOutput = [|"CAG";"CAG";"CAA";"CAA";"CAA"|] |> Array.map Genome.ofString
        let dnaLines = [|"GGCGTTCAGGCA";"CAAGGAGTTCGC";"CACGTCAATCAC";"CAATAATATTCG"|] |> Array.map Genome.ofString
        let output = greedyMotifSearch 3 5 dnaLines
        Assert.SequenceEquals expectedOutput output
