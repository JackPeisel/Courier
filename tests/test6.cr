(fact States (
  R0 >> n; 
  if n == 0 then (R1 << 1) 
  else (R0 << n - 1); Do; R1<<(n * (R1 >> z))
));
(Set fact);
R0 << 4;
Do;
R1 >> x