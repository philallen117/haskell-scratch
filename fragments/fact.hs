aux acc 0 = acc
aux acc n = aux (n * acc) (n - 1)

fact = aux 1
