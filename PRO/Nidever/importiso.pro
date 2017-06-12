function importiso,fname,washington=washington,twomass=twomass,sloan=sloan

; This program imports the Padova Isochrone data
; So far only Washington, 2MASS and Sloan filters are supported


; Basic directories

spawn,'echo $HOST',host
unixdir = '/home/frosty/dln5q/'
linuxdir = '/net/grads1/dln5q/'
unix = findfile(unixdir)
linux = findfile(linuxdir)
if unix(0) ne '' then dir = unixdir
if unix(0) eq '' and linux(0) ne '' then dir = linuxdir
if unix(0) eq '' and linux(0) eq '' then begin
  print,'NO FILES FOUND'
  return
endif

;dir = userdir()
fdir = dir+'isochrone/'

; What data directory are we using?  Washington by default
datadir=fdir+'washington/'
if keyword_set(twomass) then datadir=fdir+'2mass/'
if keyword_set(sloan) then datadir=fdir+'sloan/'

; Checking that the file exists
d = findfile(datadir+fname)
if d eq '' then begin
  print,'IMPORTISO.PRO - ERROR: FILE ',datadir+fname,' DOES NOT EXIST'
  return,-1
endif

spawn,'wc -l '+datadir+fname,line
lines = float(getwrd(line,0))

; Open file
openr,unit,datadir+fname,/get_lun

; What type of filters are we using
type = file_basename(datadir)

; Setting up the structure
case type of

  'washington': begin
    dum={age:0.0, M_ini:0.0, M_act:0.0, logL:0.0, logTe:0.0, logg:0.0, Mbol:0.0,$
         C:0.0, M:0.0, T1:0.0, T2:0.0, B:0.0, V:0.0, R:0.0, I:0.0, Flum:0.0}
  end

  '2mass': begin
    dum={age:0.0, M_ini:0.0, M_act:0.0, logL:0.0, logTe:0.0, logg:0.0, Mbol:0.0,$
         J:0.0, H:0.0, Ks:0.0, Flum:0.0}
  end

  'sloan': begin
   ;log(age/yr)   M_ini           M_act   logL/Lo logTe   logG    Mbol    u'      g'      r'      i'      z'      Flum
    dum={age:0.0, M_ini:0.0, M_act:0.0, logL:0.0, logTe:0.0, logg:0.0, Mbol:0.0,$
         umag:0.0, gmag:0.0, rmag:0.0, imag:0.0, zmag:0.0, Flum:0.0}
  end

  else: begin

  end

endcase

;; WASHINGTON 
;if not keyword_set(twomass) then begin
;  dum={age:0.0, M_ini:0.0, M_act:0.0, L:0.0, Te:0.0, logg:0.0, Mbol:0.0,$
;       C:0.0, M:0.0, T1:0.0, T2:0.0, B:0.0, V:0.0, R:0.0, I:0.0, Flum:0.0}
;
;; 2MASS
;endif else begin
;  dum={age:0.0, M_ini:0.0, M_act:0.0, L:0.0, Te:0.0, logg:0.0, Mbol:0.0,$
;       J:0.0, H:0.0, Ks:0.0, Flum:0.0}
;endelse

; Creating the structure
array = replicate(dum,lines)

; Reading it in
readf,unit,array

; Clean up
close,unit
free_lun,unit

;stop

return,array

end
