(fib States (
  R2 >> n;
  R3 >> lst;
  if n == 0 then (R0<<0); (R1<<R3)
  else if lst[n]!=0 then (R0<<lst[n]); (R1<<R3)
  else (
    (R2 << n - 1); Do; 
    R0>>z;
    (R2 << n - 2);(R3<<R1); Do; 
    x := (z + R0));
    lst := lst[n]<<x;
    R0<<x;
    R1<<lst
  )
);
n := 9 + 9 + 2;
x:= [0,1];
for(i:=0; i<n; i:=i+1) -> 
  (x:= 0 @ x)
;
(Set fib);
R2 << n;
R3 << x;
Do;
R0 >> x