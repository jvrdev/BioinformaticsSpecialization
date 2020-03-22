namespace Course2

type Edge<'a> = 'a * 'a

module Edge =
    let source (a, _) = a
    let destination (_, b) = b
