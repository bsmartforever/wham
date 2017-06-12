function significant, num, sig

;+
; Purpose - To modifiy the number to a specific significants. 
;
; num     - number to modify
; sig     - decimal place of significants
; 
; e.g., significant(1234.567,-2) => 1234.560
;	    significant(1234.567, 2) => 1200.000
;
; By Dr Kat Barger 05/2013
;-

sig=fix(sig)

   return, round(num*10.^(-sig))/10.^(-sig);

end