f n = if n==0 then 3 else 4*(-1)**(n+1)/((2*n)*(2*n+1)*(2*n+2)) + f(n-1)
main  = print(f 1000000)