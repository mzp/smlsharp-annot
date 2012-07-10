fun fact n =
    if n = 0 then
        1
    else
        n * (fact (n - 1))

val _ = print (Int.toString (fact 10))
val _ = print "\n"
