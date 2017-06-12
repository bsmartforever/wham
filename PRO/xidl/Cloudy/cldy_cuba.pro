;+ 
; NAME:
; cldy_cuba
;   Version 1.1
;
; PURPOSE:
;   Creates a Cloudy input file from a CUBA output file given a
;    redshift 
;
; CALLING SEQUENCE:
;   
;   cldy_cuba, fil, z, outfil, FIXG=fixg
;
; INPUTS:
;   fil  - CUBA output file
;   z    - Redshift
;
; RETURNS:
;
; OUTPUTS:
;   outfil  - Cloudy output file
;
; OPTIONAL INPUTS:
;  /FIXG -- I do not remember what this is for!
;  CALCU= -- Calculate the U parameter assuming n_H = 1 cm^-3
;
; OPTIONAL OUTPUTS:
; STRCT -- Stucture containing the wavelength and flux values
;
; COMMENTS:
;
; EXAMPLES:
; cldy_cuba, '/u/xavier/Cloudy/Spec/Data/CUBA/Q1G0/bkgthick.out',
; 0.35, '/u/xavier/Cloudy/Spec/Output/q1g0_z035.spec'
;
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:
;   06-Nov-2003 Written by JXP
;-
;------------------------------------------------------------------------------
pro cldy_cuba, fil, z, outfil, FIXG=fixg, STRCT=strct, CALCU=calcu, PHI=phi

  if  N_params() LT 3  then begin 
      print, 'Syntax - ' +$
        'cldy_cuba, fil, z, outfil, /FIXG [v1.1]'
      return
  endif 

  ;; Open Madau file
  
  ;; quasar only?
;  ipos = strpos(fil, 'Q1G0')
;  if strmid(fil,ipos+4,1) EQ '1' then nlin = 433L else nlin = 432L
;  stop
  nlin = 432L
  close, /all
  openr, 1, fil
  zval = fltarr(10)
  wv = dblarr(nlin)
  flux = dblarr(nlin,10)
  tmp = dblarr(nlin)
  dumf = dblarr(11)

  ;; Loop
  for ii=0L,999 do begin
      readf, 1, zval, FORMAT='(11x,10f11.4)' 
      if z LT zval[0] then begin 
          flg = 3 
          tmp[*] = flux[*,9]
      endif else flg = 1
      for jj=0L,nlin-1 do begin
          readf, 1, dumf;, FORMAT='(10f11.4)'
          flux[jj,*] = dumf[1:*]
          wv[jj] = dumf[0]
      endfor
      if flg EQ 3 then begin
          ;; Shift
          flux = shift(flux,0,1)
          zval = shift(zval,1)
          zval[0] = lstz
          stop
          flux[*,0] = tmp
          break
      endif
      if (z GE zval[0] and z LE zval[9]) then break
      lstz = zval[9]
  endfor

  strct = { $
          wv: wv, $
          flux: fltarr(n_elements(wv)) $
          }

  close, 1

  openw, 2, outfil
  printf, 2, 'interpolate (0.00001 -30.0)'

  ;; Interpolate at each wavelength!
  esv = 0.
  svjnu = fltarr(432L)
  sveng = fltarr(432L)
  for ii=431L,0,-1 do begin
      fx = interpol(flux[ii,*], zval, z, /spline)
      fx = fx[0]
      energy = 912./wv[ii]
      strct.flux[ii] = fx
      if fx GT 1E-30 then jnu = alog10(fx) else jnu = -30.
      if keyword_set(FIXG) and wv[ii] LT 50 then jnu = -30.
      if energy NE esv then $  ; Multiple energies in a few spots
        printf, 2, 'continue ('+strtrim(energy,2)+' '+ $
        string(jnu,FORMAT='(f7.3)')+')' 
      esv = energy
      sveng[ii] = energy
      svjnu[ii] = jnu
  endfor
  printf, 2, 'continue (7400000 -30.0)'
  close, 2

  if arg_present(CALCU) then begin
      c = x_constants()
      nu = sveng * c.Ryd / c.h  ; Hz
      mn = min( abs(sveng-1.), imn)
      dnu = nu - shift(nu,-1)
      dnu[431] = dnu[430]
      Phi = total(4*!pi* (10^svjnu[0:imn])*dnu[0:imn] / (c.h*nu[0:imn]))
      U = Phi / 1. / c.c
      calcU = alog10(U)
  endif


  return
  
end
