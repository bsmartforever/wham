pro deredden,instr,outstr,cmd=cmd,stp=stp

;+
;
; DEREDDEN
;
; This program can be used to deredden photometry
; using the schlegel maps.  Right now it only dereddens
; M, T2 and DDO51 magnitudes.  The 
; 
; INPUTS:
;  instr    The input structure.  The recognized names for M, T2 and
;           DDO51 are: 
;           'M'  for M
;           'T', 'T2' or 'I' for T2
;           'D', 'DDO', or 'DDO51' for DDO51
; 
;  /cmd     Plot the CMD and 2CD of the dereddened and undereddened photometry
;  /stp     Stop at the end of the program
;
; OUTPUTS:
;  outst    The output structure with dereddened magnitudes and colors
;
; USAGE:
;  IDL>deredden,instr,outstr
; 
; By D.Nidever  January 2007
;-

; Not enough inputs
if n_elements(instr) eq 0 then begin
  print,'Syntax - deredden,instr,outstr,stp=stp'
  return
endif

; Checking that it's a structure
type = size(instr,/type)
if type ne 8 then begin
  print,'INSTR IS NOT A STRUCTURE'
  return
endif

; Checking that we've got coordinates
tags = tag_names(instr)
raind = where(tags eq 'RA',nraind)
decind = where(tags eq 'DEC',ndecind)
if nraind eq 0 or ndecind eq 0 then begin
  print,'INSTR DOES NOT HAVE RA/DEC COORDINATES'
  return
endif


;######################################
;# INITIALIZING THE OUTPUT STRUCTURE

; Starting the output structure
outstr = instr

; Add an EBV tag to the structure
dum = where(strupcase(tags) eq 'EBV',nebv)
if nebv eq 0 then $
  add_tag,outstr,'EBV',0.0,outstr

nstars = n_elements(outstr)
print,'Dereddening ',strtrim(nstars,2),' sources'


;#####################################
;# GETTING THE REDDENING

ra = outstr.ra
dec = outstr.dec

; Convert to galactic coordinates
glactc,ra,dec,2000.,glon,glat,1,/degree

; Get the E(B-V) reddening from the Schlegel maps
ebv = dust_getval(glon,glat,/interp)

; Put into structure
outstr.EBV = EBV

; Convert to extinction in the band
;
; This is from Majewski et al. 2000, AJ, 120, 2550,  Paper I
; Using the average interstellar extinction curve R_V = A_V/E(B-V) = 3.1
; E(M-V) = 0.33 * E(B-V)
; E(T2-V) = -1.27 * E(B-V)
; E(M-T2) = 1.60 * E(B-V)
; E(DDO51-V) = 0.27 * E(B-V)
; E(M-DDO51) = 0.06 * E(B-V)
; A(M) = 3.43 * E(B-V)
;
; A(T2) = A(M) - E(M-T2) = 3.43 * E(B-V) - (1.60 * E(B-V))
; A(T2) = 1.83 * E(B-V)
; A(DDO51) = A(M) - E(M-DDO51) = 3.43 * E(B-V) - (0.06 * E(B-V))
; A(DDO51) = 3.37 * E(B-V)
am = 3.43 * ebv
at = 1.83 * ebv
ad = 3.37 * ebv


;##########################################
;# DEREDDENING THE MAGNITUDES AND COLORS

; The dereddened magnitude should be brighter, therefore, smaller
; subtract the extinction from the observed magnitude.
; Mo = M - A(M)

; Getting the magnitude tags
mtag = where(tags eq 'M',nmtag)
mtag = mtag[0]
ttag = where(tags eq 'T' or tags eq 'T2' or tags eq 'I',nttag)
ttag = ttag[0]
dtag = where(tags eq 'D' or tags eq 'DDO' or tags eq 'DDO51',ndtag)
dtag = dtag[0]

if nmtag eq 0 then print,'NO M TAG'
if nttag eq 0 then print,'NO T TAG'
if ndtag eq 0 then print,'NO D TAG'
if (nmtag eq 0 and nttag eq 0 and ndtag eq 0) then begin
  print,'ERROR: NO M, T or D TAGS.  RETURNING'
  return
end


; Correcting the M magnitudes
if (nmtag gt 0) then begin

  ; Adding dereddened M tag: M0
  dum = where(strupcase(tags) eq 'M0',nm0)
  if nm0 eq 0 then $
    add_tag,outstr,'M0',99.9999,outstr

  ; Dereddening only those stars that have decent magnitudes
  mgd = where(outstr.(mtag) lt 50.,nmgd)
  if nmgd gt 0 then begin
    M0 = fltarr(nstars)+99.9999                      ; all bad to start
    M0[mgd] = outstr[mgd].(mtag) - am[mgd]
    outstr.M0 = M0                                    ; put into structure
  endif

endif  ; correcting M mags


; Correcting the T magnitudes
if (nttag gt 0) then begin

  ; Adding dereddened T tag: T0
  dum = where(strupcase(tags) eq 'T0',nt0)
  if nt0 eq 0 then $
    add_tag,outstr,'T0',99.9999,outstr

  ; Dereddening only those stars that have decent magnitudes
  tgd = where(outstr.(ttag) lt 50.,ntgd)
  if ntgd gt 0 then begin
    T0 = fltarr(nstars)+99.9999                      ; all bad to start
    T0[tgd] = outstr[tgd].(ttag) - at[tgd]
    outstr.T0 = T0                                    ; put into structure
  endif

endif  ; correcting T mags


; Correcting the D magnitudes
if (ndtag gt 0) then begin

  ; Adding dereddened D tag: D0
  dum = where(strupcase(tags) eq 'D0',nd0)
  if nd0 eq 0 then $
    add_tag,outstr,'D0',99.9999,outstr

  ; Dereddening only those stars that have decent magnitudes
  dgd = where(outstr.(dtag) lt 50.,ndgd)
  if ndgd gt 0 then begin
    D0 = fltarr(nstars)+99.9999                      ; all bad to start
    D0[dgd] = outstr[dgd].(dtag) - ad[dgd]
    outstr.D0 = D0                                    ; put into structure
  endif

endif  ; correcting D mags


; Making dereddened (M-T)o color
if (nmtag gt 0 and nttag gt 0) then begin

  ; Adding dereddened M-T tag: MT0
  dum = where(strupcase(tags) eq 'MT0',nmt0)
  if nmt0 eq 0 then $
    add_tag,outstr,'MT0',99.9999,outstr

  ; Dereddened M-T color
  MT0 = M0-T0

  ; Dereddening only those stars that have decent magnitudes
  mtbad = where(M0 gt 50. OR T0 gt 50.,nmtbad)
  if nmtbad gt 0 then MT0[mtbad] = 99.9999           ; all bad to start
  outstr.MT0 = MT0                                    ; put into structure

endif


; Making dereddened (M-D)o color
if (nmtag gt 0 and nttag gt 0) then begin

  ; Adding dereddened M-T tag: MT0
  dum = where(strupcase(tags) eq 'MD0',nmd0)
  if nmd0 eq 0 then $
    add_tag,outstr,'MD0',99.9999,outstr

  ; Dereddened M-D color
  MD0 = M0-D0

  ; Dereddening only those stars that have decent magnitudes
  mdbad = where(M0 gt 50. OR D0 gt 50.,nmdbad)
  if nmdbad gt 0 then MD0[mdbad] = 99.9999           ; all bad to start
  outstr.MD0 = MD0                                    ; put into structure

endif


;##################################
;# FILLING IN THE STRUCTURE
;outstr.ebv = ebv
;outstr.M0 = M0
;outstr.T0 = T0
;outstr.D0 = D0
;outstr.MT0 = MT0
;outstr.MD0 = MD0


;#############################
;# PLOTTING
if keyword_set(cmd) then begin

  window,0
  erase

  ; CMD
  !p.multi=[2,2,1]
  tit='Color-Magnitude Diagram'
  plot,outstr.MT0,outstr.M0,ps=3,yr=[25,12],xr=[0,4],$
       xs=1,ys=1,xtit='M-T',ytit='M',tit=tit
  oplot,outstr.(mtag)-outstr.(ttag),outstr.(mtag),ps=3,co=250

  ; Color-Color diagram
  !p.multi=[1,2,1]
  tit='Two Color Diagram'
  plot,outstr.MT0,outstr.MD0,ps=3,yr=[-0.7,0.7],xr=[0,4],xs=1,ys=1,xtit='M-T',ytit='M-D',tit=tit
  oplot,outstr.(mtag)-outstr.(ttag),outstr.(mtag)-outstr.(dtag),ps=3,co=250

  xyouts,0.535,0.91,'Dereddened',align=0.5,/normal
  xyouts,0.535,0.86,'Undereddened',align=0.5,/normal,co=250

  !p.multi=0

endif

if keyword_set(stp) then stop

end
