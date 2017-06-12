pro mlr,L=L,mass=mass,Mv=Mv,bv=bv,reverse=reverse,smith=smith,$
        griffiths=griffiths,henry1=henry1,henry2=henry2,$
        noprint=noprint,plot=plot

; This program has quantitative information on the Mass-Luminosity
; Relation.  You can get luminosity from the mass and the reverse.
; Ordinarily, Mass is obtained from Luminosity.  The /reverse
; keyword is used to get Luminosity from Mass.

; Empirical fits are obtained from different sources.

;Getting Mv from B-V
if keyword_set(bv) and not keyword_set(Mv) and not keyword_set(L) then begin
  absmag,m=mv,bv=bv,/noprint
endif

if not keyword_set(Mass) and not keyword_set(L) and not keyword_set(mv) then begin
  print,'You Must Enter (Mass), (L)uminosity or Absolute Visual Magnitude (Mv)'
  return
endif

if not keyword_set(L) and not keyword_set(Mv) then reverse=1
if not keyword_set(Mass) then reverse = 0
;if keyword_set(Mv) and not keyword_set(L) then begin
;  lumin,L=L,M=Mv
;endif
if not keyword_set(Mv) and keyword_set(L) then begin
  if L lt 100. then  lumin,L=L,M=Mv,/reverse,/noprint
endif  

; IF NO FIT IS SPECIFIED USE THESE:
if not keyword_set(griffiths) and not keyword_set(smith) and not keyword_set(henry1) and $
   not keyword_set(henry2) then begin

  if keyword_set(reverse) then griffiths = 1
  if not keyword_set(reverse) then begin
    if not keyword_set(Mv) then griffiths=1
    if keyword_set(Mv) then begin
      if (Mv lt 1.45) then griffiths = 1
      if (Mv ge 1.45 and Mv lt 12.89) then henry1 = 1
      if (Mv gt 12.89) then henry2 = 1
    endif
  endif
endif

; The Observatory, vol. 103, p. 29-31 (1983)
; "An empirical stellar mass-luminosity relationship"
; by Smith,R.C.
; with mass data from Popper, ARAA, 1980, 18, 115.

; For M/Msun >= 0.43
;   log(L/Lsun) = (3.99 +/- 0.03)log(M/Msun)
;
; For M/Msun <= 0.43
;   log(L/Lsun) = (2.26 +/- 0.20)log(M/Msun) - (0.64 +/- 0.16)
;
; good for -1.0 < log(M/Msun) < +1.3
; good for -3.0 < log(L/Lsun) < +5.0

If keyword_set(smith) then begin

  ;Getting Luminosity from Mass
  if keyword_set(reverse) then begin
    ;Setting limits/range
    if alog10(M) lt -1.000 or alog10(M) gt 1.300 then begin
      print,'Log(M) Must Be In The Range -1.0 to +1.3'
      return
    endif

    if (Mass gt 0.43) then  logL = double(3.99) * alog10(M) 
    if (Mass le 0.43) then  logL = double(2.26) * alog10(M) - 0.64d 
    L = double(10.)^logL
  endif

  ;Getting Mass from Luminosity
  if not keyword_set(reverse) then begin
    ;Getting Luminosity from Abs. Vis. Mag.
    if not keyword_set(L) and keyword_set(Mv) then begin
      lumin,l=l,m=mv,/noprint
    endif

    ;Setting limits/range
    if alog10(L) lt -3.0 or alog10(L) gt 5.0 then begin
      print,'Log(L) Must Be In The Range -3.0 to +5.0'
      return
    endif

    if (alog10(L) gt -1.46246) then  logM = double(1./3.99) * alog10(L)
    if (alog10(L) le -1.46246) then  logM = double(1./2.26) * alog10(L) + double(0.64/2.26)
    Mass = double(10.)^double(logM)
  endif

Endif


; J. Roy. Astron. Soc. Can., Vol.82, No.1, 1988
; "Mass-Luminosity Relations from Binary-Star Data"
; by Griffiths, S.C., Hicks, R.B. and Milone, E.F.

; log(L) = -0.52 + 2.44log(M)     log(M) < -0.4
; log(L) = 0.006 + 4.16log(M)     -0.4 < log(M) < 0.7 
; log(L) = 0.37 + 3.51log(M)      log(M) > 0.7
;
; good for -1.0 < log(M/Msun) < +1.2
; good for -3.0 < log(L/Lsun) < +5.0

If keyword_set(griffiths) then begin

  ;Getting Luminosity from Mass
  if keyword_set(reverse) then begin
    ;Setting limits/range
    if alog10(Mass) lt -1.000 or alog10(Mass) gt 1.200 then begin
      print,'Log(M) Must Be In The Range -1.0 to +1.2'
      return
    endif

    ; Which case?
    if (alog10(Mass) le -0.4) then c=1
    if (alog10(Mass) gt -0.4 and alog10(Mass) le 0.7) then c=2
    if (alog10(Mass) gt 0.7) then c=3
    
    case c of
      1: logL = double(-0.52) + double(2.44)*alog10(Mass)
      2: logL = double(0.006) + double(4.16)*alog10(Mass) 
      3: logL = double(0.37) + double(3.51)*alog10(Mass)
    endcase
    L = double(10.)^double(logL)
  endif

  ;Getting Mass from Luminosity
  if not keyword_set(reverse) then begin
    ;Getting Luminosity from Abs. Vis. Mag.
    if not keyword_set(L) and keyword_set(Mv) then begin
      lumin,l=l,m=mv,/noprint
    endif

    ;Setting limits/range
    if alog10(L) lt -3.0 or alog10(L) gt 5.0 then begin
      print,'Log(L) Must Be In The Range -3.0 to +5.0'
      return
    endif

    ; Which case?
    if (alog10(L) le -1.4960) then c=1
    if (alog10(L) gt -1.4960 and alog10(L) le 2.9180) then c=2
    if (alog10(L) gt 2.9180) then c=3
 
    case c of
      1: logM = double(0.52/2.44) + double(1./2.44)*alog10(L)
      2: logM = double(-0.006/4.16) + double(1./4.16)*alog10(L)
      3: logM = double(-0.37/3.51) + double(1./3.51)*alog10(L)
    endcase
    Mass = double(10.)^double(logM)
  endif

Endif


; The Astronomical Journal, Vol. 106, No. 2, 1993
; "The Mass-Luminosity Relation For Stars Of Mass 1.0 To 0.08 Msun"
; by Henry, Todd J. and McCarthy, Donald W., Jr.

; For 2.00 >= M >= 0.50 Msun, Mv=1.45 to 10.25, rms (log(M))=0.032
;   log(M/Msun) = +0.002456 Mv^2 - 0.09711 Mv + 0.4365
; For 0.50 >= M >= 0.18 Msun, Mv=10.25 to 12.89, rms (log(M))=0.081
;   log(M/Msun) = -0.1681 Mv + 1.4217
; For 0.18 >= M >= 0.08 Msun, Mv=12.89 to 17.59, rms (log(M))=0.060
;   log(M/Msun) = +0.005257 Mv^2 - 0.2351 Mv + 1.4124
;
; good for 1.45 < Mv < 17.59
; good for 2.00 > M > 0.08 Msun


If keyword_set(henry1) then begin

  ;Getting Luminosity from Mass
  if keyword_set(reverse) then begin
    print,'Cant do it right now'
    stop
  endif

  ;Getting Mass from Luminosity
  if not keyword_set(reverse) then begin
    ;Need to get Mv
    if not keyword_set(Mv) then begin
      lumin,L=L,M=Mv,/noprint
    endif

    ;Setting limits/range
    if Mv lt 1.45 or Mv gt 17.59 then begin
      print,'Mv Must Be In The Range of 1.45 to 17.59'
      return
    endif
    
    ; Which case?
    if (Mv ge 1.45 and Mv le 10.25) then c=1
    if (Mv gt 10.25 and Mv le 12.89) then c=2
    if (Mv gt 12.89 and Mv le 17.59) then c=3

    case c of
      1: logM = double(+0.002456)*Mv^2 + double(-0.09711)*Mv + 0.4365d
      2: logM = double(-0.1681)*Mv + 1.4217d
      3: logM = double(+0.005257)*Mv^2 + double(-0.2351)*Mv + 1.4124d
    endcase
    Mass = double(10.)^double(logM)
  endif

Endif


; APJ, 512, 864, 1999
; "The Optical Mass-Luminosity Relation At The End Of The Main Sequence"
; by Henry, Todd J., et al.
; For Masses of 0.20 - 0.08 Msun

; log(M/Msun) = +0.005239 Mv^2 - 0.2326 Mv + 1.3785
;
; good for 0.2 > M > 0.08 Msun
; good for 12.89 < Mv < 19.0

If keyword_set(henry2) then begin

  ;Getting Luminosity from Mass
  if keyword_set(reverse) then begin
    print,'Cant do it right now'
    stop
  endif

  ;Getting Mass from Luminosity
  if not keyword_set(reverse) then begin
    ;Need to get Mv
    if not keyword_set(Mv) then begin
      lumin,L=L,M=Mv,/noprint
    endif

    ;Setting limits/range
    if Mv lt 12.89 or Mv gt 19.0 then begin
      print,'Mv Must Be In The Range of 12.89 to 19.0'
      return
    endif    

    logM = double(0.005239)*Mv^2 + double(-0.2326)*Mv + 1.3785d
    Mass = double(10.)^double(logM)
  endif

Endif

;Printing Results
if not keyword_set(noprint) then begin
  if keyword_set(L) then print,'L  = ',strtrim(L,2),' Lsun'
  if keyword_set(Mv) then print,'Mv  = ',strtrim(Mv,2)
  print,'Mass = ',strtrim(mass,2),' Msun'
endif

;Plotting
if keyword_set(plot) then begin
  n = 100.
  larr = dindgen(n)/(n-1.)*8.-3.
  larr = double(10.)^larr
  marr = dblarr(n)
  for i=0,n-1 do begin
    mmm=0 & lll=larr(i)
    mlr,mass=mmm,l=lll,/noprint
    marr(i) = mmm
  end

  psym8,0.7
  plot,alog10(marr),alog10(larr),xtit='Log M/Msun',ytit='Log L/Lsun',$
       tit='Mass-Luminosity Relation',thick=5
  oplot,[alog10(mass)],[alog10(L)],ps=4
  oplot,[alog10(mass),alog10(mass)],[-10.,10.],linestyle=2
  oplot,[-10.,10.],[alog10(L),alog10(L)],linestyle=2
  xyouts,-1.3,4.7,'L = '+strmid(strtrim(L,2),0,6)+' Lsun',charsize=1.1
  xyouts,-1.3,4.0,'M = '+strmid(strtrim(mass,2),0,5)+' Msun',charsize=1.1

  ;stop
endif

;stop

end

















