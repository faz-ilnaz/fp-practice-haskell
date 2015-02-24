f n = if n==0 then 4 else 4*(-1)**(n)/(2*n+1) + f(n-1)
main  = print(f 1000000)