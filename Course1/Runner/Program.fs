namespace Bio

module Program = 
    [<EntryPoint>]
    let main argv =
        Week1.runClump2 """C:\Users\Javier\Downloads\E_coli.txt""" 9 500 3
        //runComputingFrequencies2 """C:\Users\Javier\Downloads\dataset_2994_5.txt"""
        0