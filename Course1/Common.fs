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
    
    module Array2D =
        let columnWise (m : 't[,]) =
            seq {
                for j in 0 .. m.GetLength 1 - 1 do
                    yield seq {
                        for i in 0 .. m.GetLength 0 - 1 do
                            yield m.[i, j]
                    }
            }

        let rowWise (m : 't[,]) =
            seq {
                for i in 0 .. m.GetLength 0 - 1 do
                    yield seq {
                        for j in 0 .. m.GetLength 1 - 1 do
                            yield m.[i, j] 
                    }
            }

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
        static member Complement = function
            | A -> T
            | C -> G
            | G -> C
            | T -> A
        static member Enumerate  = [ A; C; G; T ]

    [<CustomComparison;CustomEquality;StructuredFormatDisplay "Genome {StructuredFormatted}">]
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
        static member OfSeq (s : seq<Nucleobase>) =
            s
            |> Seq.toArray
            |> ArraySegment
            |> Genome
        static member OfString (s : string) : Genome =
            Genome.OfCharSeq s
        static member OfCharSeq (s : seq<char>) : Genome =
            s
            |> Seq.map Nucleobase.ofChar
            |> Genome.OfSeq
        static member ToSeq (Genome s) : seq<Nucleobase> =
            s :> seq<Nucleobase>
        static member Slice (index : int) (count : int) (Genome g) : Genome =
            Genome (g.Slice (index, count))
        static member Suffix (Genome g) : Genome =
            Genome (g.Slice 1)
        static member Prepend (n : Nucleobase) (Genome g) : Genome =
            Genome (Seq.append (Seq.singleton n) g |> Seq.toArray |> ArraySegment)
        static member Item (i : int) (Genome g) : Nucleobase =
            g.[i]
        static member ReverseComplement (Genome g) =
            Genome (g |> Seq.rev |> Seq.map Nucleobase.Complement |> Seq.toArray |> ArraySegment)
        static member Kmers (k : int) (g : Genome) : seq<Genome> =
            seq { for i = 0 to g.Length - k do yield Genome.Slice i k g }
        static member Enumerate (k : int) =
            let rec f (acc : list<list<Nucleobase>>) i =
                if i = k then acc
                elif i = 0 then f (Nucleobase.Enumerate |> List.map List.singleton) 1
                else 
                    let accPrime = 
                        acc 
                        |> List.collect (
                            fun a -> 
                                Nucleobase.Enumerate |> List.map (
                                    fun nucleobase -> nucleobase :: a))
                    f accPrime (i + 1)
            f [] 0
            |> List.map Genome.OfSeq

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
        member private g.StructuredFormatted = g.ToString ()

    let hammingDistance (Genome a) (Genome b) =
        Seq.zip a b
        |> Seq.sumBy (fun (a, b) -> if a = b then 0 else 1)

    let patternDistance (pattern : Genome) (genome : Genome) : int =
        Genome.Kmers pattern.Length genome
        |> Seq.map (hammingDistance pattern)
        |> Seq.min

    type MotifMatrix = private MotifMatrix of Genome[]
    with 
        static member OfRows : Genome[] ->  MotifMatrix = function
            | [||] -> MotifMatrix [||]
            | rows -> 
                let rowLength = rows.[0].Length
                if rows |> Array.forall (fun r -> r.Length = rowLength) |> not then
                    invalidArg "rows" "Not all rows are of the same length"
                else MotifMatrix rows
        static member OfRowSeq = Seq.toArray >> MotifMatrix.MotifMatrix
        static member ToRows (MotifMatrix rows) = rows
        static member MapRows f = MotifMatrix.ToRows >> Seq.map f >> MotifMatrix.OfRowSeq
        member x.ColCount = 
            match MotifMatrix.ToRows x with
            | [||] -> 0
            | rows -> rows.[0].Length
        member x.RowCount = 
            match MotifMatrix.ToRows x with
            | [||] -> 0
            | rows -> rows.Length
        
    let motifCount (m : MotifMatrix) : int[,] =
        let count = Array2D.create 4 m.ColCount 0
        for row in MotifMatrix.ToRows m do
            for (j, cell) in Genome.ToSeq row |> Seq.indexed do
                let i = Nucleobase.ToInt cell
                count.[i, j] <- count.[i, j] + 1
        count

    let motifPseudoCount = motifCount >> Array2D.map ((+)1)

    let motifProfile (m : int[,]) : float[,] =
        let sums = Array.create (m.GetLength 1) 0.0
        for i = 0 to m.GetLength 0 - 1 do
            for j = 0 to m.GetLength 1 - 1 do
                sums.[j] <- sums.[j] + float m.[i, j]
        Array2D.mapi (fun i j x -> (float x) / sums.[j]) m

        
    let kmerProbability (profile : float[,]) (kmer : Genome) : float = 
        let k = kmer.Length
        if profile.GetLength 0 <> 4 then invalidArg "profile" "Profile is expected to have 4 rows"
        elif profile.GetLength 1 <> k then invalidArg "profile" "Profile column count is expected to match length of k-mer"
        else
            seq {
                for j = 0 to k - 1 do
                    let i = Nucleobase.ToInt (Genome.Item j kmer)
                    yield profile.[i, j]
            }
            |> Seq.fold (*) 1.0

    let profileMostProbableKmer (text : Genome) (k : int) (profile : float[,]) : Genome =
        Genome.Kmers k text
        |> Seq.maxBy (kmerProbability profile)

    let motifsFromMostProbableKmer k (profile : float [,]) (dnas : seq<Genome>) : seq<Genome> =
        dnas |> Seq.map (fun dna -> profileMostProbableKmer dna k profile)

    let motifConsensus =
        Array2D.columnWise
        >> Seq.map (Seq.indexed >> Seq.maxBy snd >> fst >> Nucleobase.OfInt)
        >> Genome.OfSeq

    /// Returns a score for a motif matrix measured as the sum of the distance
    /// of all rows with the matrix' consensus genome
    let motifScore (m : MotifMatrix) : int =
        let count = motifCount m
        let profile = motifProfile count
        let consensus = motifConsensus profile
        MotifMatrix.ToRows m |> Seq.sumBy (hammingDistance consensus)

    let motifDistance (pattern : Genome) (m : MotifMatrix) : int =
        MotifMatrix.ToRows m
        |> Seq.sumBy (patternDistance pattern)