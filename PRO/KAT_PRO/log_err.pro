function log_err,variable,dvariable,reverse=reverse,natural=natural 

variable=fltarr(n_elements(variable))+variable
dvariable=fltarr(n_elements(dvariable))+dvariable

factor=alog(4D)/alog10(4D)
;Don't need factor if natural logs.

e=2.71828 ;natural log factor

if keyword_set(reverse) AND keyword_set(natural) then $
   return,dvariable*e^(variable);

if keyword_set(reverse) then $
   return,dvariable*10.0^(variable)*factor;

if keyword_set(natural) and (NOT keyword_set(reverse)) then $
   return,dvariable/(variable);

return,dvariable/(variable*factor);

end