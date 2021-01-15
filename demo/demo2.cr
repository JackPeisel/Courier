y States (
  if R1 == 0 then R0<<(hd R2) 
  else 
    match R2 with 
    Null List: R0<<Null |
    A' List: R2 << (tl R2); R1 << R1 - 1; Do; R0
);
(Set y);
R1<<2;
R2<<[9,8,7,6,5];
Do;
R0