namespace Bio

open System
open System.Text
open System.IO

module Week1 = 
    let complement = function 
        | 'A' | 'a' -> Some 'T'
        | 'T' | 't' -> Some 'A'
        | 'C' | 'c' -> Some 'G'
        | 'G' | 'g' -> Some 'C'
        | _ -> None

    let baseToValue = function
        | 'A' | 'a' -> 0
        | 'C' | 'c' -> 1
        | 'G' | 'g' -> 2
        | 'T' | 't' -> 3
        | other -> failwithf "Unexpected nucleobase %c" other

    let patternToNumberString pattern =
        pattern
        |> Seq.fold (fun s x -> s * 4 + baseToValue x) 0

    let patternToNumberL (pattern : ReadOnlySpan<char>) =
        let mutable a = 0L
        for i = 0 to pattern.Length - 1 do
            a <- a * 4L + (int64 (baseToValue pattern.[i]))
        a

    let patternToNumber (pattern : ReadOnlySpan<char>) =
        let mutable a = 0
        for i = 0 to pattern.Length - 1 do
            a <- a * 4 + baseToValue pattern.[i]
        a

    let valueToBase = function
        | 0 -> 'A'
        | 1 -> 'C'
        | 2 -> 'G'
        | 3 -> 'T'
        | other -> failwithf "Unexpected nucleobase %d" other

    let numberToPatternOld k index =
        let b = StringBuilder 1024
        let mutable x = index
        for i = 1 to k do
            let c = x % 4
            x <- x / 4
            b.Append (valueToBase c) |> ignore
        b.ToString () |> String.rev

    let numberToPattern k index =
        let a = Array.create k 'A'
        let mutable x = index
        for i = k - 1 downto 0 do
            a.[i] <- valueToBase (x % 4)
            x <- x / 4
        String a

    let rec _patternCount (text : ReadOnlySpan<char>) (pattern : ReadOnlySpan<char>) (n : int) =
        if text.Length < pattern.Length
        then n
        else 
            let a = if text.Slice(0, pattern.Length).SequenceEqual(pattern) then 1 else 0
            _patternCount (text.Slice(1, pattern.Length)) pattern (n + a)

    let patternCount (text : ReadOnlySpan<char>) (pattern : ReadOnlySpan<char>) : int =
        _patternCount text pattern 0

    let frequentWords (text : string) (k : int) = 
        let textSpan = String.toReadOnlySpan text
        let a = Array.create (text.Length - k + 1) 0
        for i = 0 to text.Length - k do
            a.[i] <- patternCount textSpan (textSpan.Slice (i, k))
        let max = Array.max a
        seq { 0 .. text.Length - k }
        |> Seq.filter (fun i -> a.[i] = max)
        |> Seq.map (fun i -> text.Substring (i, k))
        |> Seq.distinct

    let private mkFrequencyArray k =
        let kmerCount = pow 4 k
        Array.create kmerCount 0

    let private fillFrequencyArray (a : int[]) (textSpan : ReadOnlySpan<char>) k =
        Array.Fill (a, 0)
        for i = 0 to textSpan.Length - k do
            let pattern = textSpan.Slice (i, k)
            let index = patternToNumber pattern
            a.[index] <- a.[index] + 1

    let private fillClump t (frequency : int[]) (clump : int[]) =
        for i = 0 to frequency.Length - 1 do
            if frequency.[i] >= t
            then clump.[i] <- 1

    let private findPatternsOfFrequencyGreaterOrEqualTo (k : int) (t : int) =
        Seq.choosei (fun i x -> if x >= t then Some (numberToPattern k i) else None)

    let frequentWordsFast (text : string) (k : int) =
        let textSpan = String.toReadOnlySpan text
        let frequency = mkFrequencyArray k
        fillFrequencyArray frequency textSpan k
        let max = Array.max frequency
        findPatternsOfFrequencyGreaterOrEqualTo k max frequency 

    let reverseComplement (pattern : string) =
        pattern
        |> Seq.choose complement
        |> Seq.rev 
        |> Seq.toArray
        |> (fun s -> System.String (s))

    let patternPositions (pattern : string) (text : string) =
        let patternArray = pattern.ToCharArray ()
        let textArray = text.ToCharArray ()
        seq { 0 .. text.Length - pattern.Length }
        |> Seq.filter (fun i -> ReadOnlySpan(textArray, i, patternArray.Length).SequenceEqual(ReadOnlySpan (patternArray)))
        |> Seq.map string
        |> Seq.toArray

    let findClump (genome : string) (k : int) (L : int) (t : int) =
        let genomeSpan = String.toReadOnlySpan genome
        let a = mkFrequencyArray k
        let clumps = System.Collections.Generic.List<string> ()
        for i = 0 to genome.Length - L do
            fillFrequencyArray a (genomeSpan.Slice (i, L)) k
            let windowClumps = findPatternsOfFrequencyGreaterOrEqualTo k t a
            clumps.AddRange windowClumps
        clumps |> Seq.distinct

    let findClumpFast (genome : string) (k : int) (L : int) (t : int) =
        let genomeSpan = String.toReadOnlySpan genome
        let frequency = mkFrequencyArray k
        let clump = mkFrequencyArray k
        fillFrequencyArray frequency (genomeSpan.Slice (0, L)) k
        fillClump t frequency clump
        for i = 1 to genome.Length - L do
            let left = genomeSpan.Slice (i - 1, k)
            let leftIndex = patternToNumber left
            frequency.[leftIndex] <- frequency.[leftIndex] - 1
            let right = genomeSpan.Slice (i + L - k, k)
            let rightIndex = patternToNumber right
            frequency.[rightIndex] <- frequency.[rightIndex] + 1
            if frequency.[rightIndex] = t
            then clump.[rightIndex] <- 1
        clump |> Seq.choosei (fun i x -> if x = 1 then Some (numberToPattern k i) else None)

    let runClump path = 
        let [| genome; argsLine |] = File.ReadAllLines path
        let [| kText; LText; tText |] = argsLine.Split ' '
        let clumps = findClump genome (int kText) (int LText) (int tText)
        let output = clumps |> format
        printfn "%s" output
        writeToResultsFile (format clumps)

    let runClump2 path k L t = 
        let genome = read path
        let clumps = findClumpFast genome k L t
        let output = clumps |> Seq.length |> string
        printfn "%s" output
        writeToResultsFile (format clumps)

    let runComputingFrequencies (input : string) =
        let [| genome; kText |] = input.Replace("\r\n", "\n").Split([|'\n'|], 2)
        let k = int kText
        let a = mkFrequencyArray k
        fillFrequencyArray a (String.toReadOnlySpan genome) k
        a
        |> Seq.map string
        |> format

    let runComputingFrequencies2 (path : string) =
        let content = read path
        let output = runComputingFrequencies content
        writeToResultsFile output

    let runOnFile f path =
        let input = File.ReadAllLines path
        let args = input |> Array.filter (System.String.IsNullOrWhiteSpace >> not)
        let result = f args
        let output = result |> format
        writeToResultsFile output