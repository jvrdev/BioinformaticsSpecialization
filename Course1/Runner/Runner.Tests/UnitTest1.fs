namespace Tests

open NUnit.Framework
open Bio
open Bio.Week1

type Assert() =
    static member SequenceEquals (expected : seq<'t>) (actual : seq<'t>) =
        Assert.AreEqual (Seq.length expected, Seq.length actual)
        for (c, d) in Seq.zip expected actual do 
            Assert.AreEqual (c, d)

[<TestFixture>]
type TestClass () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.TestFrequentWords () =
        let frequentWords = frequentWordsFast "CGGAGGACTCTAGGTAACGCTTATCAGGTCCATAGGACATTCA" 3
        Assert.SequenceEquals [|"AGG"|] frequentWords

    [<Test; TestCaseSource("PatternToNumberSamples")>]
    member this.TestPatternToNumber x (expectedY : int) =
        let y = patternToNumber (String.toReadOnlySpan x)
        Assert.AreEqual (expectedY, y)

    [<Test; TestCaseSource("PatternToNumberLSamples")>]
    member this.TestPatternToNumberL x (expectedY : int64) =
        let y = patternToNumberL (String.toReadOnlySpan x)
        Assert.AreEqual (expectedY, y)

    [<Test; TestCaseSource("NumberToPatternSamples")>]
    member this.TestNumberToPattern k (x : int) (expectedY : string) =
        let y = numberToPattern k x
        Assert.AreEqual (expectedY, y)

    [<Test; TestCaseSource("FindClumpSamples")>]
    member this.TestFindClump genome k L t expectedKmers =
        let kmers = findClump genome k L t
        Assert.SequenceEquals expectedKmers kmers

    [<Test; TestCaseSource("FindClumpSamples")>]
    member this.TestFindClumpFast genome k L t expectedKmers =
        let kmers = findClumpFast genome k L t
        Assert.SequenceEquals expectedKmers kmers

    [<Test>]
    member this.Week1Dot5Stept () =
        let sampleInput = "ACGCGGCTCTGAAA\r\n2"
        let expectedOutput = "2 1 0 0 0 0 2 2 1 2 1 0 0 1 1 0"
        let actualOutput = runComputingFrequencies sampleInput
        Assert.AreEqual (expectedOutput, actualOutput)

    static member PatternToNumberSamples : obj[][] =
        [|
            [| "A"; 0 |]
            [| "AA"; 0 |]
            [| "T"; 3 |]
            [| "TA"; 12 |]
            [| "AGT"; 11 |]
            [| "TT"; 15 |]
            [| "ATGCAA"; 912|]
        |]

    static member PatternToNumberLSamples : obj[][] =
        [|
            [| "TCCGTTATACCGGTT"; 901563823L |]
        |]

    static member NumberToPatternSamples : obj[][] =
        [|
            [| 2; 0; "AA" |]
            [| 0; 0; "" |]
            [| 4; 0; "AAAA" |]
            [| 4; 4; "AACA" |]
            [| 4; 5; "AACC" |]
            [| 4; 6; "AACG" |]
            [| 4; 7; "AACT" |]
            [| 7; 5437; "CCCATTC" |]
            [| 8; 5437; "ACCCATTC" |]
            [| 7; 5539; "CCCGGAT" |]
        |]

    static member FindClumpSamples : obj[][] =
        [|
            [| "CGGACTCGACAGATGTGAAGAACGACAATGTGAAGACTCGACACGACAGAGTGAAGAGAAGAGGAAACATTGTAA"; 5; 50; 4; ["CGACA"; "GAAGA"] |]
        |]
