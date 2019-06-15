open System
open System.IO
open System.Text

let read = File.ReadAllText
let writeToResultsFile output = File.WriteAllText ("result.txt", output, System.Text.Encoding.ASCII)
let format = String.concat " "

module String = 
    let rev (s : string) =
        let a = s.ToCharArray()
        Array.Reverse a
        String a

let complement = function 
    | 'A' | 'a' -> Some 'T'
    | 'T' | 't' -> Some 'A'
    | 'C' | 'c' -> Some 'G'
    | 'G' | 'g' -> Some 'C'
    | _ -> None


let patternToIndex pattern =
    let baseToValue = function
        | 'A' | 'a' -> 0
        | 'T' | 't' -> 1
        | 'C' | 'c' -> 2
        | 'G' | 'g' -> 3
        | other -> failwithf "Unexpected nucleobase %c" other
    pattern
    |> Seq.fold (fun s x -> s * 4 + baseToValue x) 0

let valueToBase = function
   | 0 -> 'A'
   | 1 -> 'T'
   | 2 -> 'C'
   | 3 -> 'G'
   | other -> failwithf "Unexpected nucleobase %d" other

let indexToPattern k index =
    let b = StringBuilder 1024
    let mutable x = index
    for i = 1 to k do
        let c = x % 4
        x <- x / 4
        b.Append (valueToBase c) |> ignore
    b.ToString () |> String.rev

let patternCount (text : string) (pattern : string) : int =
    seq { 0 .. (text.Length - pattern.Length) }
    |> Seq.sumBy (fun i ->
        if text.Substring(i, pattern.Length) = pattern
        then 1
        else 0)

let frequentWords (text : string) (k : int) = 
    let a = Array.create (text.Length - k + 1) 0
    for i = 0 to text.Length - k do
        a.[i] <- patternCount text (text.Substring (i, k))
    let max = Array.max a
    seq { 0 .. text.Length - k }
    |> Seq.filter (fun i -> a.[i] = max)
    |> Seq.map (fun i -> text.Substring (i, k))
    |> Seq.distinct
    |> String.concat " "

let inline pow ``base`` exp = 
    let mutable x = 1
    for i = 1 to exp do
        x <- x * ``base``
    x

let frequentWordsFast (text : string) (k : int) =
    let kmerCount = pow 4 k
    let a = Array.create kmerCount 0
    for i = 0 to text.Length - k do
        let pattern = text.Substring (i, k)
        let index = patternToIndex pattern
        a.[index] <- a.[index] + 1 
    let max = Array.max a
    printfn "%d" max
    seq { 0 .. text.Length - k }
    |> Seq.filter (fun i -> a.[i] = max)
    |> Seq.map (indexToPattern k)

let reverseComplement (pattern : string) =
    pattern
    |> Seq.choose complement
    |> Seq.rev 
    |> Seq.toArray
    |> (fun s -> System.String (s))

let patternPositions (pattern : string) (text : string) =
    seq { 0 .. (text.Length - pattern.Length) }
    |> Seq.filter (fun i -> text.Substring(i).StartsWith pattern)
    |> Seq.map string
    |> Seq.toArray

let runOnFile f path =
    let input = File.ReadAllLines path
    let args = input |> Array.filter (System.String.IsNullOrWhiteSpace >> not)
    let result = f args
    let output = result |> format
    writeToResultsFile output

[<EntryPoint>]
let main argv =
    let input = read "/home/javier/Downloads/Vibrio_cholerae.txt"
    let output = patternPositions "patternMatching" input |> format
    writeToResultsFile output
    0