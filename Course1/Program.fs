namespace Bio

module Program = 
    [<EntryPoint>]
    let main argv =
        let output = Week4.runGibbsSampler """C:\Users\Javier\Downloads\dataset_163_4(7).txt"""
        //runComputingFrequencies2 
        printfn "%s" output
        0