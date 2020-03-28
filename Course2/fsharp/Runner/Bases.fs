namespace Course2

type Base =
    | Adenosine
    | Cytosine
    | Guanine
    | Thymine
    | Uracil

module Base =
    let ofChar = 
        System.Char.ToUpperInvariant 
        >> function
        | 'A' -> Some Adenosine
        | 'C' -> Some Cytosine
        | 'G' -> Some Guanine
        | 'T' -> Some Thymine
        | 'U' -> Some Uracil
        | _ -> None

    let toChar = function 
        | Adenosine -> 'A'
        | Cytosine -> 'C'
        | Guanine -> 'G'
        | Thymine -> 'T'
        | Uracil -> 'U'

    let transcribe = function
        | Uracil -> invalidArg "x" "Uracil is already transcribed"
        | Thymine -> Uracil
        | other -> other

    let complementRna = function
        | Thymine -> invalidArg "x" "Thymine cannot be present in RNA"
        | Uracil -> Adenosine
        | Adenosine -> Uracil
        | Cytosine -> Guanine
        | Guanine -> Cytosine

    let complementDna = function
        | Uracil -> invalidArg "x" "Uracil cannot be present in DNA"
        | Thymine -> Adenosine
        | Adenosine -> Thymine
        | Cytosine -> Guanine
        | Guanine -> Cytosine
        
type Codon = Base * Base * Base

module Codon =
    let ofArray : Base[] -> option<Codon> = function
        | [|a; b; c|] -> Some (a, b, c)
        | _ -> None

type Dna = private Dna of seq<Base>

type Rna = private Rna of seq<Base>

module Dna = 
    let mk (x : seq<Base>) =
        if not <| Seq.forall ((<>)Uracil) x
        then invalidArg "x" "DNA cannot contain Uracil"
        else Dna x

    let transcribe (Dna x) : Rna =
        x
        |> Seq.map Base.transcribe
        |> Rna

    let reverseComponent (Dna x) : Dna =
        x
        |> Seq.rev
        |> Seq.map Base.complementDna
        |> Dna
       
    let toString (Dna x) =
        x
        |> Seq.map Base.toChar
        |> String.ofSeq

    let parse (s : string) : option<Dna> =
        let bases =
            s
            |> Seq.choose Base.ofChar
            |> Seq.toArray

        if bases.Length = s.Length
        then   
            Some <| mk bases
        else None

module Rna =
    let mk (x : seq<Base>) =
        if not <| Seq.forall ((<>)Thymine) x
        then invalidArg "x" "RNA cannot contain Thymine"
        else Rna x

    let toCodons (Rna x) : seq<Codon> =
        x
        |> Seq.chunkBySize 3
        |> Seq.choose Codon.ofArray

        