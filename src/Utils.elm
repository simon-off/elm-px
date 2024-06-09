module Utils exposing (doIf, ternary)


ternary : Bool -> a -> a -> a
ternary condition a b =
    if condition then
        a

    else
        b


doIf : (a -> a) -> Bool -> a -> a
doIf transformation condition value =
    if condition then
        transformation value

    else
        value
