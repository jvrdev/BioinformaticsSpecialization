namespace Course2

open System

[<AutoOpen>]
module Prelude =
    let sequenceEquals<'a when 'a : equality> (us : seq<'a>) (vs : seq<'a>) : bool =
        (Seq.length us) = (Seq.length vs) &&
            Seq.zip us vs |> Seq.forall (fun (a, b) -> a = b)

    module String =
        let split (separator : string) (s : string) : string[] =
            s.Split ([|separator|], System.StringSplitOptions.None)

        let replace (u : string) (v : string) (s : string) : string =
            s.Replace (u, v)

        let splitLines (s : string) : string[] =
            s |> replace "\r\n" "\n" |> split "\n"

        let trim (s : string) =
            s.Trim ()

        let ofSeq : seq<Char> -> String = Seq.toArray >> String

        let tryHead (s : String) : option<char> = 
            if s.Length > 0 then Some s.[0] else None
