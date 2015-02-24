f i n = 
	if n==0 then 4 else
	if i==0 then 3 + 1/f (i+1) n else 
	if n==1 then 15 else
	if i==1 then 6 + ((2*i+1)**2)/f (i+1) n else 
	if i==n then (6 + (2*i+1)**2) else 
		6 + ((2*i+1)**2)/f (i+1) n 
main  = print(f 0 100000)