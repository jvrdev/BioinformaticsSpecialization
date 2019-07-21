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
        let genome = Genome.OfString "CATGGGCATCGGCCATACGCC"
        let output = skews genome
        Assert.SequenceEquals expectedOutput output

    [<Test>]
    member this.TestMinSkewPositions () =    
        let expectedOutput = [|11;24|]
        let genome = Genome.OfString "TAAAGACTGCCGAGAGGCCAACACGAGTGCTAGAACGAGGGGCGTAAACGCGGGTCCGAT"
        let output = minSkewPositions genome
        Assert.SequenceEquals expectedOutput output

    [<Test>]
    member this.TestHammingDistance () =    
        let expectedOutput = 3
        let a = Genome.OfString "GGGCCGTTGGT"
        let b = Genome.OfString "GGACCGTTGAC"
        let output = hammingDistance a b
        Assert.AreEqual (expectedOutput, output)

    [<Test>]
    member this.TestApproximatePatternMatch () =  
        let expectedOutput = [|6;7;26;27|]
        let pattern = Genome.OfString "ATTCTGGA"
        let genome = Genome.OfString "CGCCCGAATCCAGAACGCATTCCCATATTTCGGGACCACTGGCCTCCACGGTACGGACGTCAATCAAAT"
        let d = 3
        let output = approximatePatternMatch pattern genome d
        Assert.SequenceEquals expectedOutput output

    [<Test>]
    member this.TestCountD () =  
        let expectedOutput = 11
        let pattern = Genome.OfString "AAAAA"
        let genome = Genome.OfString "AACAAGCTGATAAACATTTAAAGAG"
        let d = 2
        let output = countD pattern genome d
        Assert.AreEqual (expectedOutput, output)

    [<Test>]
    member this.TestEquals () =  
        let expectedOutput = true
        let a = Genome.OfString "ATA"
        let b = Genome.OfString "ATA"
        let output = a.Equals b
        Assert.AreEqual (expectedOutput, output)

    [<Test>]
    member this.TestGetHashCode () =
        let a = Genome.OfString "ATA"
        let b = Genome.OfString "ATA"
        let outputA = a.GetHashCode ()
        let outputB = b.GetHashCode ()
        Assert.AreEqual (outputA, outputB)
    
    [<Test>]
    member this.TestEquals2 () =  
        let expectedOutput = false
        let a = Genome.OfString "ATA"
        let b = Genome.OfString "ATC"
        let output = a.Equals b
        Assert.AreEqual (expectedOutput, output)

    [<Test>]
    member this.TestGetHashCode2 () =
        let a = Genome.OfString "ATA"
        let b = Genome.OfString "ATC"
        let outputA = a.GetHashCode ()
        let outputB = b.GetHashCode ()
        Assert.AreNotEqual (outputA, outputB)

    [<Test>]
    member this.TestNeighbors () =  
        let expectedOutput =
            ["CCG";"TCG";"GCG";"AAG";"ATG";"AGG";"ACA";"ACC";"ACT";"ACG"]
            |> List.map Genome.OfString
        let pattern = Genome.OfString "ACG"
        let d = 1
        let output = neighbors pattern d
        Assert.AreEqual (Set.ofList expectedOutput, Set.ofList output)

    [<Test>]
    member this.TestFrequentWordsWithMismatches () =
        let expectedOutput = 
            [|"GATG";"ATGC";"ATGT"|]
            |> Array.map Genome.OfString
        let pattern = Genome.OfString "ACGTTGCATGTCGCATGATGCATGAGAGCT"
        let k = 4
        let d = 1
        let output, _ = frequentWordsWithMismatches pattern k d
        Assert.AreEqual (Set.ofArray expectedOutput, Set.ofArray output)

    [<Test>]
    member this.TestFrequentWordsWithMismatchesAndReverseComplements () =
        let expectedOutput = 
            [|"ATGT";"ACAT"|]
            |> Array.map Genome.OfString
        let pattern = Genome.OfString "ACGTTGCATGTCGCATGATGCATGAGAGCT"
        let k = 4
        let d = 1
        let output, _ = frequentWordsWithMismatchesAndReverseComplements pattern k d
        Assert.AreEqual (Set.ofArray expectedOutput, Set.ofArray output)