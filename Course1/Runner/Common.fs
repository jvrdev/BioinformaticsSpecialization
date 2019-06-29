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
    
    let inline pow ``base`` exp = 
        let mutable x = 1
        for i = 1 to exp do
            x <- x * ``base``
        x

