{- 
 -
 -                4
 - pi = -------------------
 -                1^2
 -       1 + --------------
 -                   3^2
 -            2 + ---------
 -                     5^2
 -                 2 + ----
 -                      ...
 -}
pi1 n = f 0 n
  where f i n = 
            	if n==0 then 4 else
            	if n==1 then 2 else
            	if i==0 then 4/f (i+1) n else 
            	if i==1 then 1 + ((2*i-1)**2)/f (i+1) n else 
            	if i==n then (2 + (2*i-1)**2) else 
            		  2 + ((2*i-1)**2)/f (i+1) n 

{-
 -                  1^2
 - pi = 3 + -------------------
 -                   3^2
 -           6 + -------------
 -                     5^2
 -                6 + ------
 -                     ...
 -}
pi2 n = f 0 n
  where f i n = 
            if n==0 then 4 else
            if i==0 then 3 + 1/f (i+1) n else 
            if n==1 then 15 else
            if i==1 then 6 + ((2*i+1)**2)/f (i+1) n else 
            if i==n then (6 + (2*i+1)**2) else 
              6 + ((2*i+1)**2)/f (i+1) n 

{-
 -                 4
 - pi = ------------------------
 -                   1^2
 -       1 + ------------------
 -                    2^2
 -            3 + -----------
 -                     3^2
 -                 5 + ---
 -                     ...
 -}
pi3 n = f 0 n
  where f i n = 
            if n==0 then 4 else
            if i==0 then 4/f (i+1) n else
            if i==n then ((2*i-1) + i**2) else 
              (2*i-1) + (i**2)/f (i+1) n 

{-       4     4     4     4
 - pi = --- - --- + --- - --- + ...
 -       1     3     5     7
 -}
pi4 n = if n==0 then 4 else 4*(-1)**(n)/(2*n+1) + pi4(n-1)

{-             4         4         4         4
 - pi = 3 + ------- - ------- + ------- - -------- + ...
 -           2*3*4     4*5*6     6*7*8     8*9*10
 -}
pi5 n = if n==0 then 3 else 4*(-1)**(n+1)/((2*n)*(2*n+1)*(2*n+2)) + pi5(n-1)