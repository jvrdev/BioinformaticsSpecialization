namespace Bio

module Program = 
    [<EntryPoint>]
    let main argv =
        let output = Week3.runProfileMostProbableKmer """C:\Users\Javier\Downloads\dataset_159_3.txt"""
        //runComputingFrequencies2 
        printfn "%s" output
        0