namespace Bio

open System.IO
open System

[<AutoOpen>]
module Common =
    let read = File.ReadAllText
    let writeToResultsFile output = File.WriteAllText ("result.txt", output, System.Text.Encoding.ASCII)
    let format x = String.concat " " x
    
    module String = 
        let rev (s : string) =
            let a = s.ToCharArray()
            Array.Reverse a
            String a
        let toReadOnlySpan (s : string) = 
            ReadOnlySpan (s.ToCharArray ())
    
    module Seq =
        let choosei (f : int -> 'T -> option<'U>) =
            Seq.indexed
            >> Seq.choose (fun (i, x) -> f i x)

        let findAllIndexes f = Seq.indexed >> Seq.filter (snd >> f) >> Seq.map fst

        let findAllMinIndexes x =
            let min = Seq.min x
            findAllIndexes ((=)min) x
    
    let inline pow ``base`` exp = 
        let mutable x = 1
        for i = 1 to exp do
            x <- x * ``base``
        x

    [<StructuralEquality;StructuralComparison>]
    type Nucleobase = | A | C | G | T
    with
        static member ToChar = function
            | A -> 'A'
            | C -> 'C'
            | G -> 'G'
            | T -> 'T'
        static member ofChar (c : char) =
            match c with
            | 'A' | 'a' -> A
            | 'C' | 'c' -> C
            | 'G' | 'g' -> G
            | 'T' | 't' -> T
            | other -> failwithf "Unexpected nucleobase %c" other
        static member ToInt = function
            | A -> 0
            | C -> 1
            | G -> 2
            | T -> 3
        static member OfInt : int -> Nucleobase = function
            | 0 -> A
            | 1 -> C
            | 2 -> G
            | 3 -> T
            | other -> failwithf "Unexpected int value %d" other
        static member Enumerate  = [ A; C; G; T ]

    [<CustomComparison;CustomEquality>]
    type Genome = Genome of ArraySegment<Nucleobase>
    with
        interface IComparable<Genome> with
            member this.CompareTo (other : Genome) =
                let (Genome thisInner) = this
                let (Genome otherInner) = other
                let lengthCompare = thisInner.Count.CompareTo otherInner.Count
                if lengthCompare = 0 then
                    Seq.zip thisInner otherInner
                    |> Seq.map (fun (a, b) -> compare a b)
                    |> Seq.tryFind ((<>)0)
                    |> Option.defaultValue 0
                else lengthCompare
        interface IComparable with
            member this.CompareTo (other : obj) =
                match other with
                | :? Genome as o -> (this :> IComparable<Genome>).CompareTo(o)
                | _ -> -1
        interface IEquatable<Genome> with
            member this.Equals (other : Genome) =
                compare this other = 0
        static member ofString (s : string) : Genome =
            s
            |> Seq.map Nucleobase.ofChar
            |> Seq.toArray
            |> ArraySegment
            |> Genome
        static member Slice (index : int) (count : int) (Genome g) : Genome =
            Genome (g.Slice (index, count))
        static member Suffix (Genome g) : Genome =
            Genome (g.Slice 1)
        static member Prepend (n : Nucleobase) (Genome g) : Genome =
            Genome (Seq.append (Seq.singleton n) g |> Seq.toArray |> ArraySegment)
        static member Item (i : int) (Genome g) : Nucleobase =
            g.[i]
        static member Kmers (k : int) (g : Genome) : seq<Genome> =
            seq { for i = 0 to g.Length - k do yield Genome.Slice i k g }
        override this.Equals (other : obj) =
            match other with
            | :? Genome as o -> (this :> IEquatable<Genome>).Equals(o)
            | _ -> false
        override this.GetHashCode () =
            let (Genome thisInner) = this
            thisInner
            |> Seq.fold (fun s x -> s * 13 + Nucleobase.ToInt x) 0
        override this.ToString () =
            let (Genome thisInner) = this
            String (thisInner |> Seq.map Nucleobase.ToChar |> Seq.toArray)
        member g.Length =
            let (Genome arraySegment) = g
            arraySegment.Count
