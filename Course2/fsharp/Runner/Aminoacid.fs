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

module Aminoacid =
    let ofChar =
        System.Char.ToUpperInvariant
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
            splitLines blob
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
        

