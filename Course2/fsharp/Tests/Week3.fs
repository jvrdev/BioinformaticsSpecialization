module Week3

open Course2
open NUnit.Framework
open Weeks1And2

let areEqual (expected : 't) (actual : 't) =
    Assert.AreEqual (expected, actual)
    
[<Test>]
let ``protein translation problem`` () =
    let rna = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
    let actual = Week3.proteinTranslationS rna
    areEqual "MAMAPRTEINSTRING" actual

