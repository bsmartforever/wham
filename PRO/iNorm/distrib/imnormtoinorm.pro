;+
;                               iMNORMTOINORM.PRO
;                               Version 1.0;
;Program Description:
; This procedure will update files *o.save files to *i.save files
; with updated variable names. 
;
;Latest Update Comments:
;       04/15/13  NL: Version 1.0
;
;External Routines called:
;       None
;----------------------------------------------------------------------------
PRO iMNORMTOINORM

           file = file_search('*o.save')

for i =0,n_elements(file) -1 do begin
  restore,file(i)
  if n_elements(vlsr) eq 0 then vlsr = 0.  
ion = el
gam = gv
fval=fv
object = name
redshift = z
wavc = wc 
wni = STRTRIM(string(wavc,'(f8.1)'),2)

  save,fil=ion+wni+'i.save',v,f,ef,ion,wni,wavc,fval,gam,redshift,object,vlsr

endfor 

print,'Done......................'

end
