namespace Tests

open NUnit.Framework
open Bio
open Bio.Week2

[<TestFixture>]
type Week2Tests () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.TestSkews () =
        let expectedOutput = [|0;-1;-1;-1;0;1;2;1;1;1;0;1;2;1;0;0;0;0;-1;0;-1;-2|]
        let genome = Genome.ofString "CATGGGCATCGGCCATACGCC"
        let output = skews genome
        Assert.SequenceEquals expectedOutput output

    [<Test>]
    member this.TestMinSkewPositions () =    
        let expectedOutput = [|11;24|]
        let genome = Genome.ofString "TAAAGACTGCCGAGAGGCCAACACGAGTGCTAGAACGAGGGGCGTAAACGCGGGTCCGAT"
        let output = minSkewPositions genome
        Assert.SequenceEquals expectedOutput output

    [<Test>]
    member this.TestHammingDistance () =    
        let expectedOutput = 3
        let a = Genome.ofString "GGGCCGTTGGT"
        let b = Genome.ofString "GGACCGTTGAC"
        let output = hammingDistance a b
        Assert.AreEqual (expectedOutput, output)

    [<Test>]
    member this.TestApproximatePatternMatch () =  
        let expectedOutput = [|6;7;26;27|]
        let pattern = Genome.ofString "ATTCTGGA"
        let genome = Genome.ofString "CGCCCGAATCCAGAACGCATTCCCATATTTCGGGACCACTGGCCTCCACGGTACGGACGTCAATCAAAT"
        let d = 3
        let output = approximatePatternMatch pattern genome d
        Assert.SequenceEquals expectedOutput output

    [<Test>]
    member this.TestCountD () =  
        let expectedOutput = 11
        let pattern = Genome.ofString "AAAAA"
        let genome = Genome.ofString "AACAAGCTGATAAACATTTAAAGAG"
        let d = 2
        let output = countD pattern genome d
        Assert.AreEqual (expectedOutput, output)

    [<Test>]
    member this.TestNeighbors () =  
        let expectedOutput =
            ["CCG";"TCG";"GCG";"AAG";"ATG";"AGG";"ACA";"ACC";"ACT";"ACG"]
            |> List.map Genome.ofString
        let pattern = Genome.ofString "ACG"
        let d = 1
        let output = neighbors pattern d
        Assert.AreEqual (Set.ofList expectedOutput, Set.ofList output)

    [<Test>]
    member this.TestFrequentWordsWithMismatches () =
        let expectedOutput = 
            [|"GATG";"ATGC";"ATGT"|]
            |> Array.map Genome.ofString
        let pattern = Genome.ofString "ACGTTGCATGTCGCATGATGCATGAGAGCT"
        let k = 4
        let d = 1
        let output = frequentWordsWithMismatches pattern k d
        Assert.AreEqual (Set.ofArray expectedOutput, Set.ofArray output)

    [<Test>]
    member this.TestFrequentWordsWithMismatchesAndReverseComplements () =
        let expectedOutput = 
            [|"ATGT";"ACAT"|]
            |> Array.map Genome.ofString
        let pattern = Genome.ofString "ACGTTGCATGTCGCATGATGCATGAGAGCT"
        let k = 4
        let d = 1
        let output = frequentWordsWithMismatchesAndReverseComplements pattern k d
        Assert.AreEqual (Set.ofArray expectedOutput, Set.ofArray output)