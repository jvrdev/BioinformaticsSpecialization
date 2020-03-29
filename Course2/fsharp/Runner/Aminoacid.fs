namespace Course2

type Aminoacid =
    | Histidine
    | Glutamine
    | Proline
    | AsparticAcid
    | GlutamicAcid
    | Alanine
    | Glycine
    | Valine
    | Tyrosine
    | Cysteine
    | Tryptophan
    | Phenylalanine
    | Leucine
    | Asparagine
    | Lysine
    | Threonine
    | Serine
    | Arginine
    | Isoleucine
    | Methionine

type Peptide = seq<Aminoacid>

type Spectrum = seq<int>

module Aminoacid =
    open System

    let all = 
        Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<Aminoacid>)
        |> Seq.map (fun x -> Microsoft.FSharp.Reflection.FSharpValue.MakeUnion(x, Array.zeroCreate(x.GetFields().Length)) :?> Aminoacid)
        |> Seq.toArray

    let ofChar =
        Char.ToUpperInvariant
        >> function 
        | 'H' -> Some Histidine
        | 'Q' -> Some Glutamine
        | 'P' -> Some Proline
        | 'D' -> Some AsparticAcid
        | 'E' -> Some GlutamicAcid
        | 'A' -> Some Alanine
        | 'G' -> Some Glycine
        | 'V' -> Some Valine
        | 'Y' -> Some Tyrosine
        | 'C' -> Some Cysteine
        | 'W' -> Some Tryptophan
        | 'F' -> Some Phenylalanine
        | 'L' -> Some Leucine
        | 'N' -> Some Asparagine
        | 'K' -> Some Lysine
        | 'T' -> Some Threonine
        | 'S' -> Some Serine
        | 'R' -> Some Arginine
        | 'I' -> Some Isoleucine
        | 'M' -> Some Methionine
        | _ -> None

    let toChar =
        function
        | Histidine -> 'H'
        | Glutamine -> 'Q'
        | Proline -> 'P'
        | AsparticAcid -> 'D'
        | GlutamicAcid -> 'E'
        | Alanine -> 'A'
        | Glycine -> 'G'
        | Valine -> 'V'
        | Tyrosine -> 'Y'
        | Cysteine -> 'C'
        | Tryptophan -> 'W'
        | Phenylalanine -> 'F'
        | Leucine -> 'L'
        | Asparagine -> 'N'
        | Lysine -> 'K'
        | Threonine -> 'T'
        | Serine -> 'S'
        | Arginine -> 'R'
        | Isoleucine -> 'I'
        | Methionine -> 'M'

    let integerMass =
        let map =
            """G 57
A 71
S 87
P 97
V 99
T 101
C 103
I 113
L 113
N 114
D 115
K 128
Q 128
E 129
M 131
H 137
F 147
R 156
Y 163
W 186"""
            |> String.splitLines
            |> Seq.map (fun line ->
                match String.split " " line with
                | [|letter; mass|] -> 
                    match String.tryHead letter |> Option.bind ofChar, Int32.TryParse mass with
                    | Some amino, (true, number) -> amino, number
                    | _ -> failwithf "Unexpected line '%s'" line
                | other -> failwithf "Unexpected line fields '%A'" other
               )
            |> Map.ofSeq

        fun a -> Map.find a map

    let ofCodon : Codon -> option<Aminoacid> =
        let blob = """AAA K
AAC N
AAG K
AAU N
ACA T
ACC T
ACG T
ACU T
AGA R
AGC S
AGG R
AGU S
AUA I
AUC I
AUG M
AUU I
CAA Q
CAC H
CAG Q
CAU H
CCA P
CCC P
CCG P
CCU P
CGA R
CGC R
CGG R
CGU R
CUA L
CUC L
CUG L
CUU L
GAA E
GAC D
GAG E
GAU D
GCA A
GCC A
GCG A
GCU A
GGA G
GGC G
GGG G
GGU G
GUA V
GUC V
GUG V
GUU V
UAA 
UAC Y
UAG 
UAU Y
UCA S
UCC S
UCG S
UCU S
UGA 
UGC C
UGG W
UGU C
UUA L
UUC F
UUG L
UUU F
"""
        let table =
            String.splitLines blob
            |> Seq.choose (fun s -> 
                if System.String.IsNullOrWhiteSpace s
                then None
                else
                    match s.Split ' ' with
                    | [|codon; amino|] ->
                        let maybeC = codon |> Seq.choose Base.ofChar |> Seq.toArray |> Codon.ofArray
                        let maybeA = if amino.Length < 1 then None else ofChar amino.[0]
                        match maybeC, maybeA with
                        | Some c, Some a -> Some (c, a)
                        | _ -> None
                    | _ -> None
            )
            |> Map.ofSeq
        fun codon -> Map.tryFind codon table

module Peptide = 
    let toString : Peptide -> string = Seq.map Aminoacid.toChar >> Seq.toArray >> System.String
        
    let length : Peptide -> int = Seq.length

    let ofRna (x : Rna) : Peptide =
        x |> Rna.toCodons |> Seq.choose Aminoacid.ofCodon

    let parse (s : string) : option<Peptide> =
        s
        |> Seq.choose Aminoacid.ofChar
        |> Some

module Spectrum =
    let parse (s : string) : option<Spectrum> =
        s
        |> String.split " "
        |> Seq.choose (fun x -> 
            match System.Int32.TryParse x with
            | false, _ -> None
            | true, i -> Some i)
        |> Seq.toArray
        |> seq
        |> Some


    let isCompatible (reference : Spectrum) =
        let sortSpectrum =
            Seq.groupBy id
            >> Seq.map (fun (mass, masses) -> mass, Seq.length masses)
        let map = sortSpectrum reference |> Map.ofSeq
        printfn "%1024A" map
        fun (s : Spectrum) ->
            let sSorted = sortSpectrum s |> Map.ofSeq
            if Seq.length sSorted = 4 then printfn "%A" sSorted
            let compatible = 
                sSorted
                |> Seq.forall (fun kvp -> 
                    let (mass, count) = kvp.Key, kvp.Value
                    match map |> Map.tryFind mass with
                    | Some referenceCount -> referenceCount >= count
                    | None -> false
                )
            if Seq.length sSorted = 4 then 
                printfn "%s %1024A" (if compatible then "Yes" else "No ") map
            compatible

    let isEqualToSortedSpectrum (reference : Spectrum) =
        let sortedReference =
            reference
            |> Seq.sort
            |> Seq.toArray
        fun (sorted : Spectrum) ->
            sequenceEquals
                sortedReference
                sorted