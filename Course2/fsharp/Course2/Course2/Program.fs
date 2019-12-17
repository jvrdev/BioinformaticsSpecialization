// Learn more about F# at http://fsharp.org

open System

let splitS (separator : string) (s : string) =
    s.Split (separator)

let replaceS (u : string) (v : string) (s : string) =
    s.Replace (u, v)

let splitLines (s : string) =
    s |> replaceS "\r\n" "\n" |> splitS "\n"

let trim (s : string) =
    s.Trim ()

type Nucleobase = A | C | G | T

type AdjacencyList<'a> = list<'a * list<'a>>

let parse (parseNode : string -> 'a) (s : string) : AdjacencyList<'a> =
    let lines = splitLines s
    lines 
    |> Seq.map (
        trim
        >> splitS " -> " 
        >> (fun [|a;  b|] -> parseNode a, splitS "," b |> Seq.map parseNode |> Seq.toList))
    |> Seq.toList

let print (printNode : 'a -> string) (x : AdjacencyList<'a>) =
    x
    |> Seq.map (fun (a, b) -> sprintf "%s -> %s" (printNode a) (b |> Seq.map printNode |> String.concat ","))
    |> String.concat System.Environment.NewLine

[<EntryPoint>]
let main argv =
    0