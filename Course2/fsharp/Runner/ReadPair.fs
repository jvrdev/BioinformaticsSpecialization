namespace Course2

type ReadPair<'t> = 't * 't

module ReadPair =
    let map f (a, b) = f a, f b