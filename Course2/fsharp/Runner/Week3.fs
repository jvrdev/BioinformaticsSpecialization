namespace Course2

module Week3 =
    let proteinTranslation (pattern : seq<Base>) : Peptide =
        pattern
        |> Seq.chunkBySize 3
        |> Seq.choose (fun a ->
            match Codon.ofArray a with
            | Some codon ->
                Aminoacid.ofCodon codon
            | None -> None
        )

    let proteinTranslationS (s : string) : string =
        let pattern = s |> Seq.choose Base.ofChar
        let peptide = proteinTranslation pattern
        Peptide.toString peptide