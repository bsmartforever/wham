;+
; MSCMAGMA
;
; This is a modified version of Mike Siegel's MAGMA program that
; calibrates raw photometry.  MAGMA is very versatile and can
; average multiple exposures per band, offsets, variables stars
; and the more.  MSCMAGMA just calibrates raw photometry
; non-interactively.  If you need more than that then you should
; probably use MAGMA instead.
;
; INPUTS:
;  inpfile    This is an input file that lists information about
;             the raw photometry files.  You can calibrate many
;             raw photometry files at once.  Each file needs its
;             own line in the input file that has the following
;             information:
;
;             raw photometry filename, Band1 name, Band1 airmass,
;             Band1 exptime, Band1 aperture correction, Band2 ...
;
; This is an example inpfile:
; obj1110_1.raw  I  1.7270  90.0  0.0141  M  1.6960  60.0  0.0081  D  1.7040  420.0  0.0079
;
;             The input files can easily be generated with the
;             MSCMAGMA_PREP.PRO program.
;
;             The raw photometry files are assumed to be in
;             the DAOMASTER format:
;             -The first three lines are a header, and are skipped
;             -Each star has a separate line with the format:
;              ID, X, Y, Band1, Band1 error, Band2, Band2 error, ...
;               chi, sharp
;
;             The raw files can have as many bands as you like,
;             but otherwise they must conform to this format.
;
;
;  transfile  This gives the transformation information needed
;             to calibrate the raw photometry.  Normally there
;             is a different one of these for every night.
;
;             There need to be two lines per band.
;             FIRST line:  band name,  color name, transformation
;             equation coefficients (zero point, airmass, color
;             airmass*color, color^2)
;             SECOND line: errors in the transformation equation
;             coefficients
;
;     This is an example transfile:
;     M    M-T  -0.9990    0.1402     -0.1345    0.0000   0.0000
;               1.094E-02  5.037E-03  2.010E-03  0.0000   0.0000
;     T    M-T  -0.0061    0.0489     0.0266     0.0000   0.0000
;               6.782E-03  3.387E-03  1.374E-03  0.0000   0.0000
;     D    M-D  1.3251     0.1403     -0.0147    0.0000   0.0000
;               1.001E-02  5.472E-03  2.653E-02  0.0000   0.0000
;
;  /silent  Don't print anything out
;  /stp     Stop at the end of the program
;
; OUTPUTS:
;  A calibrated photometry file will be output for each raw photometry
;  file in the input file.  The ".phot" extension will be used for
;  the calibrated photometry files.
;
;
; EXAMPLE:
;  IDL>mscmagma,'field1.input','n1.trans'
; 
;
; History:  1989:       getmags.for written by Majewski
;           1997-2003:  Magma.pro written by Mike Siegel
;           2007:       Mscmagma.pro by David Nidever
;-


FUNCTION simplerr,inerr,am,cl,cler,t
;=====================================================================
;
;   This propogates photometric error through the transformation equations
;   This version takes into account error in transformation constants, although
;   it assumes perfect airmass
;
; sigma(V)=sqrt[ sigma(mV)^2 + + sigma(v1)^2 + (XV*sigma(v2))^2+((B-V)*sigma(v3))^2 
;		+ (XV*(B-V)*sigma(v4))^2 + (B-V*sigma(v5))^2]
;                +(v3+v4*XV*+v5*2*(B-V))^2*sigma(B-V)^2               
;
;=====================================================================

temper = inerr^2 + t.zptermsig^2 + (am*t.amtermsig)^2
temper = temper + (cl*t.coltermsig)^2 + (am*cl*t.amcoltermsig)^2
temper = temper + (cl*cl*t.colsqtermsig)^2
temper = temper + (t.colterm+t.amcolterm*am+2*t.colsqterm*cl)^2*cler^2
outerr = sqrt(temper)

; Set bad ones to 9.9999
bd = where(inerr gt 9.,nbd)
if nbd gt 0 then outerr[bd] = 9.9999

return,outerr

end

;------------------------------------------------------------


FUNCTION simplestar,inmag,am,colr,apcorr,exp,t
;=====================================================================
;
;   This uses the solved colors and other terms to find the magnitude in each band
;
;   V = mV - v1 - v2 * XV - v3 * (B-V) - v4 * XV*(B-V) - v5 * (B-V) * (B-V)
;         +(aperture correction) + (time correction)
; 
;=====================================================================

outmag = inmag - t.zpterm - t.amterm*am - t.colterm*colr
outmag = outmag - t.amcolterm*am*colr-t.colsqterm*colr*colr
outmag = outmag + apcorr

; Correct for exposure time
if (exp gt 0) then begin
   outmag = outmag+2.5*alog10(exp)
endif

; Set bad ones to 99.9999
bd = where(inmag gt 90.,nbd)
if nbd gt 0 then outmag[bd] = 99.9999

return,outmag

end

;------------------------------------------------------------

PRO solvestar,instar,trans,inp,outstar
;=====================================================================
;
;   The heart of the program.  This iteratively solves for each star.  It applies 
;   the transformation equation assuming a color of zero.  It then averages the 
;   passbands that are common, solves for the color and resolves for each
;   magnitude given the new color, gradually iterating until convergence.
;   
;   The solved magnitude is in the form: (V in the example)
;   V = mV - v1 - v2 * XV - v3 * (B-V) - v4 * XV*(B-V) - v5 * (B-V) * (B-V)
;         +(aperture correction)+(time correction)
;
;  It sends back to the code outstar, an array of average values in each
;  passband and tempstar, an array of the individual solved magnitudes
;
;=====================================================================

; Setting up some important arrays
numobs = n_elements(inp.band)
;passband = inp.band
passband = trans.band
colband = trans.colband
colsign = trans.colsign

; Information about the observations
; The some for all stars and bands
airmass = inp.airmass
exptime = inp.exptime
apcorr = inp.apcorr

; The input magnitudes and errors
inmag = instar[*,2*lindgen(numobs)+3]
inerr = instar[*,2*lindgen(numobs)+4]

; Initializing some arrays
clr = inmag*0.
clrerr = inmag*0.
tempmag = inmag*0.
temperr = inmag*0.
laststar = inmag*0.


;#############################
;# First we set the color terms to zero, then solve for the magnitudes using
;#   simplestar

; Loop through the bands
for a=0,numobs-1 do begin

  ; Assume an initial color and color error of zero
  clr[*,a] = 0
  clrerr[*,a] = 0

  ; Run simplestar to calculate the tranformed magnitudes
  newmag = SIMPLESTAR(inmag[*,a],airmass[a],clr[*,a],apcorr[a],exptime[a],trans[a])
  tempmag[*,a] = newmag


  ; Run simplerr to calculate the error in the transformed magnitudes
  newerr = SIMPLERR(inerr[*,a],airmass[a],clr[*,a],clrerr[*,a],trans[a])
  temperr[*,a] = newerr

endfor


; This is the index of the passband to use for the color
colbandind = lonarr(numobs)

; Loop through the bands
for d=0,numobs-1 do begin
  ind = first_el(where(passband eq colband[d],nind))
  colbandind[d] = ind  
end


;##################################
;# Now begin the iteration loop
;##################################

niter = 0
converge = 0

WHILE (converge eq 0) do begin


  ; #############################
  ; First set the color term.
  ; Passbands with an indefinite color will have it set to zero.

  ; Loop through the bands
  for d=0,numobs-1 do begin

    ; Index of the passband to use for the color
    ind = colbandind[d]

    ; We have a valid color index
    if (ind gt -1) then begin

      ; Color sign = 1
      if (colsign[d] eq 1) then begin
        clr[*,d] = tempmag[*,d]-tempmag[*,ind]
    
      ; Color sign = 2
      endif else begin
        clr[*,d] = tempmag[*,ind]-tempmag[*,d]
      endelse

      ; Bad photometry, use color=0
      bd = where( (tempmag[*,ind] gt 90.) OR (tempmag[*,d] gt 90.) ,nbd)
      if nbd gt 0 then begin
        clr[bd,d] = 0.0
        clrerr[bd,d] = 0.0
      endif

      ; To avoid the color^2 recursion, color error is taken from instrumental errors
      gd = where(inerr[*,ind] lt 9.0,ngd)
      if ngd gt 0 then clrerr[gd,d]  = inerr[gd,ind]

    ; No valid color index, set color=0
    endif else begin
      clr[*,d] = 0.0
      clrerr[*,d] = 0.0
    endelse


  endfor ; looping through the bands


  ; ############################
  ; Now resolve the star

  ; Loop through the stars
  for f=0,numobs-1 do begin

    ; Run simplestar to calculate the tranformed magnitudes
    newmag = SIMPLESTAR(inmag[*,f],airmass[f],clr[*,f],apcorr[f],exptime[f],trans[f])
    tempmag[*,f] = newmag

  
    ; Run simplerr to calculate the error in the transformed magnitudes
    newerr = SIMPLERR(inerr[*,f],airmass[f],clr[*,f],clrerr[*,f],trans[f])
    temperr[*,f] = newerr

  endfor


  ; #######################
  ; Check for convergence
  ; the iteration loop recycles until the solution is good
  ; Every star+band must not change at the 0.002 level in order to stop
  ; OR niter>30

  converge = 1  ; assume good at first

  bd = where( abs(tempmag-laststar) gt 0.002,nbd)
  if nbd gt 0 then converge=0

  ; Copying current solution to "last" solution
  if (converge eq 0) then laststar = tempmag

  ; Go up to 20 iterations, send out an error message if it doesn't converge
  if (niter gt 30) then print,'Star', instar[0] ,' failed to converge.'

  ; Increment
  niter = niter+1

ENDWHILE


; Putting together the output array
outstar = instar*0.
; Transfer over the id,position,chi and sharp
outstar[*,0] = instar[*,0]
outstar[*,1] = instar[*,1]
outstar[*,2] = instar[*,2]
outstar[*,2*numobs+3] = instar[*,2*numobs+3]
outstar[*,2*numobs+4] = instar[*,2*numobs+4]

; Transfer the final magnitudes and errors
outstar[*,2*lindgen(numobs)+3] = tempmag
outstar[*,2*lindgen(numobs)+4] = temperr

end



;----------------------------------------------------------



PRO  mscmagma,inpfile,transfile,silent=silent,stp=stp
;=====================================================================
;
;   This is the main program.  It reas in the data from magfile and trans
;   after asking for user input, initializes variables and then starts 
;   solving for each star.
;
;=====================================================================

; Not enough inputs
if n_params() lt 2 then begin
  print,'Syntax - mscmagma,inpfile,transfile'
  return
endif

; Testing the files

test = file_test(inpfile)
if test eq 0 then begin
  print,'FILE ',inpfile,' DOES NOT EXIST'
  return
endif

test2 = file_test(transfile)
if test2 eq 0 then begin
  print,'FILE ',transfile,' DOES NOT EXIST'
  return
endif


;# #####################################################
;# READ THE TRANSFORMATION FILE
;# Two lines per band.
;# First line:  band name,  color name, transformation equation coefficients
;# Second line: errors in the transformation equation coefficients

trans1 = {band:'',color:'',colband:'',colsign:0,zpterm:0.0d,amterm:0.0d,colterm:0.0d,amcolterm:0.0d,colsqterm:0.0d,$
          zptermsig:0.0d,amtermsig:0.0d,coltermsig:0.0d,amcoltermsig:0.0d,colsqtermsig:0.0d}

openr,unit,/get_lun,transfile

while (~EOF(unit)) do begin

  ; Reading in the transformation coefficients line
  line=''
  readf,unit,line
  arr = strsplit(line,' ',/extract)
  
  trans1.band = arr[0]
  trans1.color = arr[1]
  trans1.zpterm = arr[2]
  trans1.amterm = arr[3]
  trans1.colterm = arr[4]
  trans1.amcolterm = arr[5]
  trans1.colsqterm = arr[6]

  ; Reading in the error line
  line2=''
  readf,unit,line2
  arr2 = strsplit(line2,' ',/extract)

  trans1.zptermsig = arr2[0]
  trans1.amtermsig = arr2[1]  
  trans1.coltermsig = arr2[2] 
  trans1.amcoltermsig = arr2[3]
  trans1.colsqtermsig = arr2[4]

  ; Add to final transformation structure
  push,trans,trans1

end

close,unit
free_lun,unit

numbands = n_elements(trans)


; Figure out the colband and colsign for each band
for i=0,numbands-1 do begin

  band = strtrim(trans[i].band,2)

  col = strcompress(trans[i].color,/remove_all)

  ; Splitting up the two bands
  arr = strsplit(col,'-',/extract)

  ind = where(arr eq band,nind)

  ; colsign = 1 means band - colband
  if (ind eq 0) then begin
    trans[i].colband = arr[1]
    trans[i].colsign = 1
  endif

  ; colsign = 2 means colband - band
  if (ind eq 1) then begin
    trans[i].colband = arr[0]
    trans[i].colsign = 2
  endif

  if (ind eq -1) then begin
    trans[i].colband = ''
    trans[i].colsign = -1
  endif

end


; Print the transformation equations
if not keyword_set(silent) then begin
  print,' TRANSFORMATION EQUATIONS'
  print,'------------------------------------------------------------------'
  print,' BAND   COLOR  ZERO-POINT  AIRMASS   COLOR     AIR*COL   COLOR^2 '
  print,'------------------------------------------------------------------'
  for i=0,numbands-1 do begin

    form = '(A-5,A8,F10.4,F10.4,F10.4,F10.4,F10.4)'
    print,format=form,'  '+trans[i].band,trans[i].color,trans[i].zpterm,trans[i].amterm,trans[i].colterm,trans[i].amcolterm,trans[i].colsqterm
    print,format=form,'','',trans[i].zptermsig,trans[i].amtermsig,trans[i].coltermsig,trans[i].amcoltermsig,trans[i].colsqtermsig

  end
  print,'------------------------------------------------------------------'
  print,''
endif


;###############################
;# READ THE INPUT FILE
;# Read in the input file which has the following information:
;# photometry filename, Band1 name, Band1 airmass, Band1 exptime, Band1 aperture correction, Band2 ...
inparr = importascii(inpfile,/noprint)
ninp = n_elements(inparr)

tags = tag_names(inparr)
ntags = n_elements(tags)

; Transferring to a more user-friendly structure
numobs = (ntags-1)/4
dum = {magfile:'',outfile:'',band:strarr(numobs),airmass:dblarr(numobs),exptime:dblarr(numobs),apcorr:dblarr(numobs)}
input = replicate(dum,ninp)
input.magfile = strtrim(inparr.(0),2)
for i=0,numobs-1 do begin
  input.band[i] = strtrim(inparr.(1+i*4),2)
  input.airmass[i] = double(inparr.(2+i*4))
  input.exptime[i] = double(inparr.(3+i*4))
  input.apcorr[i] = double(inparr.(4+i*4))
end

; Making the output filename
ext = 'phot'
for i=0,ninp-1 do begin
  magfile = input[i].magfile
  arr = strsplit(magfile,'.',/extract)
  narr = n_elements(arr)
  outfile = strjoin(arr[0,narr-2],'')+'.'+ext
  input[i].outfile = outfile
end

if not keyword_set(silent) then begin
  print,'Running MSCMAGMA on ',strtrim(ninp,2),' input files'
  print,''
endif


; Looping through the magnitude files
FOR i=0L,ninp-1 do begin

  inp = input[i]
  magfile = inp.magfile

  ; Testing the file
  test = file_test(magfile)
  if test eq 0 then begin
    print,'FILE ',magfile,' DOES NOT EXIST'
    goto,BOMB
  endif

  ; Stars in this file
  numstar = file_lines(magfile)-3L

  ; Print file info
  if not keyword_set(silent) then begin
    print,format='(A-9,A-20)','FILE ',input[i].magfile
    print,format='(A-9,'+strtrim(numobs,2)+'A-7)','BAND',input[i].band
    print,format='(A-9,'+strtrim(numobs,2)+'F-7.4)','AIRMASS',input[i].airmass
    print,format='(A-9,'+strtrim(numobs,2)+'F-7.1)','EXPTIME',input[i].exptime
    print,format='(A-9,'+strtrim(numobs,2)+'F-7.4)','APCORR',input[i].apcorr
    print,format='(A-9,I-8)','NSTARS',numstar
    print,''
  endif


  ;#############################
  ;# READING IN THE PHOTOMETRY
  ;#############################

  ; mastable is where everything is stored, id, x, y, unsolved magnitudes, chi sharp
  mastable = fltarr(numstar,2*numobs+5)

  ; Reading in the magnitude file
  get_lun,unit
  openr, unit, magfile

  line=''
  readf,unit,line
  readf,unit,line
  readf,unit,line

  for j=0l,numstar-1 do begin
    instr=' '
    inline = fltarr(2*numobs+5)
    readf, unit, instr
    reads,instr,inline
    mastable[j,0:2*numobs+4] = inline[0:2*numobs+4]
  endfor

  close, unit
  free_lun,unit

  raarray = reform(mastable[*,1])
  decarray = reform(mastable[*,2])

  ; Initializing the arrays
  goodstar = fltarr(2*numbands+5)
  manystars = fltarr(2*numobs)
  startable = fltarr(numstar,2*numbands+5)
  indystar = fltarr(numstar,2*numobs)


  ;########################
  ;# Making the transformation structure for the bands of this input file
  trans2 = replicate(trans[0],numobs)

  ; Associate each observed passband with each trans band
  for j=0,numobs-1 do begin
    gd = where(trans.band eq inp.band[j],ngd)

    if (ngd eq 0) then begin

      ; Checking for I=T2
      if inp.band[j] eq 'I' then begin
        gd = where(trans.band eq 'T',ngd)
        if ngd eq 0 then gd = where(trans.band eq 'T2',ngd)
      endif

      ; Checking T2=I
      if inp.band[j] eq 'T2' or inp.band[j] eq 'T' then begin
        gd = where(trans.band eq 'I',ngd)
      endif
    endif

    ; Found the transformation for this band
    if (ngd gt 0) then begin
      trans2[j] = trans[gd[0]]
    endif else begin
      print,'NO TRANSFORMATION INPUT FOR ',inp.band[j]
      stop
    endelse
  end


  ; Ah, the meat of the program.  One by one, each star is popped off of
  ; the observation file, thrown into the solution engine (SOLVESTAR), solved
  ; and then brought out as an average solution (goodstar) and individual measures
  ; (indystar).  After that, frame to frame residuals are calculated
  ; indystar is where the individual solved magnitudes will be stored
  SOLVESTAR,mastable,trans2,inp,goodstar


  ; Printing results to output file
  ;Header information is printed (an index of columns) and then
  ;the stars, one by one
  ;now set up for the various prinitng options
  outfile = input[i].outfile

  openw,unit,/get_lun,outfile

  ; Print the header
  header = '    ID      X        Y        '
  for j=0,numobs-1 do header=header+inp.band[j]+'       '+inp.band[j]+'ERR      '
  header = header+'CHI     SHARP'
  printf,unit,header

  ; Loop through the stars
  for d=0l,numstar-1 do begin
    printf,unit,format='(2X,I5,2F9.3,'+strtrim(2*numobs+2,2)+'F9.4)',goodstar[d,*]
  endfor

  close,unit
  free_lun,unit

  BOMB:

END ; loop through the magnitude files

if not keyword_set(silent) then print,'MSCMAGMA FINISHED'

if keyword_set(stp) then stop

end
