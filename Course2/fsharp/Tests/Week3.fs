module Week3

open Course2
open NUnit.Framework
open AntibioticSequencing
open Week3
open System
open System.Numerics

let areEqual (expected : 't) (actual : 't) =
    Assert.AreEqual (expected, actual)
    
[<Test>]
let ``protein translation problem`` () =
    let rna = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
    let actual = proteinTranslationS rna
    areEqual "MAMAPRTEINSTRING" actual

[<Test>]
let ``peptide encoding problem`` () =
    let s = """ATGGCCATGGCCCCCAGAACTGAGATCAATAGTACCCGTATTAACGGGTGA
MA
"""
    let expected = """ATGGCC
GGCCAT
ATGGCC"""
    let actual = peptideEncodingS s
    areEqual expected actual

[<Test>]
let ``number of cyclic peptides of length l`` () =
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

    areEqual (BigInteger 980597910) (f (BigInteger 31315))

[<Test>]
let ``linear spectrum`` () =
    let input = "NQEL"
    let expected = "0 113 114 128 129 242 242 257 370 371 484"
    let actual = linearSpectrumS input
    areEqual expected actual

[<Test>]
let ``cyclic spectrum`` () =
    let input = "LEQN"
    let expected = "0 113 114 128 129 227 242 242 257 355 356 370 371 484"
    let actual = cyclicSpectrumS input
    areEqual expected actual

[<Test>]
let ``cyclic spectrum 2`` () =
    let input = "IAQMLFYCKVATN"
    let expected = "0 71 71 99 101 103 113 113 114 128 128 131 147 163 170 172 184 199 215 227 227 231 244 259 260 266 271 286 298 298 310 312 328 330 330 372 385 391 394 399 399 399 401 413 423 426 443 443 470 493 498 502 513 519 526 527 541 554 556 557 564 569 590 598 616 626 640 654 657 658 665 670 682 697 697 703 711 729 729 753 753 771 779 785 785 800 812 817 824 825 828 842 856 866 884 892 913 918 925 926 928 941 955 956 963 969 980 984 989 1012 1039 1039 1056 1059 1069 1081 1083 1083 1083 1088 1091 1097 1110 1152 1152 1154 1170 1172 1184 1184 1196 1211 1216 1222 1223 1238 1251 1255 1255 1267 1283 1298 1310 1312 1319 1335 1351 1354 1354 1368 1369 1369 1379 1381 1383 1411 1411 1482"
    let actual = cyclicSpectrumS input
    areEqual expected actual

[<Test>]
let ``number of peptides of total mass m`` () =
    let mass = 1024
    let expected = 14712706211L
    let actual = peptideCountWithMass mass
    areEqual expected actual

[<Test>]
let ``k * C ** m equation`` () =
    let aInt = 1500
    let bInt = 1600

    let countA = double <| peptideCountWithMass aInt
    let countB = double <| peptideCountWithMass bInt

    let a = double aInt
    let b = double bInt

    let k =
        let top = (Math.Log countA - (a / b * Math.Log countB))
        let bottom = 1.0 + a / b
        Math.Exp (top / bottom)

    let c =
        Math.Exp ((Math.Log countB - Math.Log k) / b)

    Assert.AreEqual (1.025, c, 0.005)

[<Test>]
let ``cyclopeptide sequencing`` () =
    let input = "0 113 128 186 241 299 314 427"
    let expected = "186-128-113 186-113-128 128-186-113 128-113-186 113-186-128 113-128-186"
    let actual = cyclopeptideSequencingS input
    areEqual expected actual

[<Test>]
let ``cyclopeptide sequencing 2`` () =
    let input = "0 71 97 99 103 113 113 114 115 131 137 196 200 202 208 214 226 227 228 240 245 299 311 311 316 327 337 339 340 341 358 408 414 424 429 436 440 442 453 455 471 507 527 537 539 542 551 554 556 566 586 622 638 640 651 653 657 664 669 679 685 735 752 753 754 756 766 777 782 782 794 848 853 865 866 867 879 885 891 893 897 956 962 978 979 980 980 990 994 996 1022 1093"
    let expected = "103-137-71-131-114-113-113-115-99-97 103-97-99-115-113-113-114-131-71-137 113-113-114-131-71-137-103-97-99-115 113-113-115-99-97-103-137-71-131-114 113-114-131-71-137-103-97-99-115-113 113-115-99-97-103-137-71-131-114-113 114-113-113-115-99-97-103-137-71-131 114-131-71-137-103-97-99-115-113-113 115-113-113-114-131-71-137-103-97-99 115-99-97-103-137-71-131-114-113-113 131-114-113-113-115-99-97-103-137-71 131-71-137-103-97-99-115-113-113-114 137-103-97-99-115-113-113-114-131-71 137-71-131-114-113-113-115-99-97-103 71-131-114-113-113-115-99-97-103-137 71-137-103-97-99-115-113-113-114-131 97-103-137-71-131-114-113-113-115-99 97-99-115-113-113-114-131-71-137-103 99-115-113-113-114-131-71-137-103-97 99-97-103-137-71-131-114-113-113-115"
    let actual = cyclopeptideSequencingS input
    areEqual expected actual
