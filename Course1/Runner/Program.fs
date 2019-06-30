namespace Bio

module Program = 
    [<EntryPoint>]
    let main argv =
        let output = Week2.runFrequentWordsWithMismatches """C:\Users\Javier\Downloads\dataset_9_7.txt"""
        //runComputingFrequencies2 
        printfn "%s" output
        0