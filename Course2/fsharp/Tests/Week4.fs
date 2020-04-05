module Week4

open Course2
open NUnit.Framework
open AntibioticSequencing
open Week4
open System
open System.Numerics

let areEqual (expected : 't) (actual : 't) =
    Assert.AreEqual (expected, actual)
    
[<Test>]
let ``peptide scoring problem`` () =
    let input = """NQEL
0 99 113 114 128 227 257 299 355 356 370 371 484"""
    let expected = "11"
    
    let actual = cyclopeptideScoringProblemS input

    areEqual expected actual

[<Test>]
let ``peptide linear scoring`` () =
    Peptide.parse "NQEL"
    |> Option.bind (fun peptide ->
        Spectrum.parse "0 99 113 114 128 227 257 299 355 356 370 371 484"
        |> Option.map (fun spectrum ->
            let actual = linearScorePeptide peptide spectrum
            areEqual 8 actual
        )
    ) 
    |> Option.defaultWith (fun () -> Assert.Fail ())


[<Test>]
let ``trim`` () =
    let input = """LAST ALST TLLT TQAS
0 71 87 101 113 158 184 188 259 271 372
2"""
    let expected = "LAST ALST"
    
    let actual = trimS input

    areEqual expected actual