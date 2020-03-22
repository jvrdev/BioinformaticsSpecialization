namespace Course2

[<AutoOpen>]
module Prelude =
    let sequenceEquals<'a when 'a : equality> (us : seq<'a>) (vs : seq<'a>) : bool =
        (Seq.length us) = (Seq.length vs) &&
            Seq.zip us vs |> Seq.forall (fun (a, b) -> a = b)

    let splitS (separator : string) (s : string) : string[] =
        s.Split ([|separator|], System.StringSplitOptions.None)

    let replaceS (u : string) (v : string) (s : string) : string =
        s.Replace (u, v)

    let splitLines (s : string) : string[] =
        s |> replaceS "\r\n" "\n" |> splitS "\n"

    let trim (s : string) =
        s.Trim ()