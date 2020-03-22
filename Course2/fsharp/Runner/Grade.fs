namespace Course2

type Grade = { InGrade : int; OutGrade : int }
with
    static member balance {InGrade = a; OutGrade = b} = a - b
