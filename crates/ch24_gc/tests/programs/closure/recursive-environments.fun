f bias n = let { even k = if k == 0 then bias else odd (k-1); odd k = if k == 0 then bias+1 else even (k-1); } in even n
main = print (f 40 21)
