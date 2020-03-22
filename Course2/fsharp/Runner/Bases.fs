namespace Course2

type Base =
    | Adenosine
    | Cytosine
    | Guanine
    | Thymine
    | Uracil

type Codon = Base * Base * Base

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

module Codon =
    let ofArray : Base[] -> option<Codon> = function
        | [|a; b; c|] -> Some (a, b, c)
        | _ -> None
        