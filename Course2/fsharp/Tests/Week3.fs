module Week3

open Course2
open NUnit.Framework
open Weeks1And2
open System.Numerics


let areEqual (expected : 't) (actual : 't) =
    Assert.AreEqual (expected, actual)
    
[<Test>]
let ``protein translation problem`` () =
    let rna = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
    let actual = Week3.proteinTranslationS rna
    areEqual "MAMAPRTEINSTRING" actual

[<Test>]
let ``peptide encoding problem`` () =
    let s = """ATGGCCATGGCCCCCAGAACTGAGATCAATAGTACCCGTATTAACGGGTGA
MA
"""
    let expected = """ATGGCC
GGCCAT
ATGGCC"""
    let actual = Week3.peptideEncodingS s
    areEqual expected actual

[<Test>]
let ``peptide countring problem`` () =
    let factorial n = 
        if n > BigInteger.One
        then 
            seq { BigInteger.One .. n } |> Seq.fold (*) BigInteger.One
        else BigInteger.One
    
    let combinatorial n k = 
        let nBang = factorial n
        let nkBang = factorial (n - k)
        let kBang = factorial k
        nBang / (nkBang * kBang)
    
    let f l = 
        let two = BigInteger 2 
        two * combinatorial l two

    areEqual (BigInteger 980597910) (f (BigInteger 18500))