f i n = 
	if n==0 then 4 else
	if i==0 then 4/f (i+1) n else
	if i==n then ((2*i-1) + i**2) else 
		(2*i-1) + (i**2)/f (i+1) n 
main  = print(f 0 1000)