namespace Bio

module Program = 
    [<EntryPoint>]
    let main argv =
        let output = Week3.runMotifDistance """C:\Users\Javier\Downloads\dataset_5164_1.txt"""
        //runComputingFrequencies2 
        printfn "%s" output
        0