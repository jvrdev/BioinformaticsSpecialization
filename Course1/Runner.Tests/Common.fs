namespace Tests

open NUnit.Framework

type Assert() =
    static member SequenceEquals (expected : seq<'t>) (actual : seq<'t>) =
        Assert.AreEqual (Seq.length expected, Seq.length actual)
        for (c, d) in Seq.zip expected actual do 
            Assert.AreEqual (c, d)