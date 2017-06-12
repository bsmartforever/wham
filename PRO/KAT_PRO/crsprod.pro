;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
;      CRSPROD (IUE Production Library) (01 April 1988)
;
;*CLASS:
;     cross-correlation
;
;*CATEGORY:
;
;*PURPOSE:
;   To compute a normalized cross correlation for two spectral segments
;   which are sampled on the same log(lambda) scale. 
;
;*CALLING SEQUENCE:
;   CRSPROD,FFIR,FSEC,NSPR,CROSS,CRMIN,CRMAX
;
;*PARAMETERS:
;   FFIR    (REQ) (I)  (1) (F)
;           Required input vector giving the flux data for the first
;           spectrum.
;
;   FSEC    (REQ) (I)  (1) (F)
;           Required input vector giving the flux data for the second 
;           spectrum.
;
;   NSPR    (REQ) (I)  (0) (F)
;           Required input parameter specifying the spectral range to
;           be considered in the cross-correlation function.
;
;   CROSS   (REQ) (O)  (1) (F)
;           Required output vector containing the cross-correlation 
;           function.
;
;   CRMIN   (REQ) (O)  (0) (F)
;           Required output vector containing the minimum of the 
;           cross-correlation function. 
;
;   CRMAX
;           Required output vector containing the maximum of the 
;           cross-correlation function. 
;
;*EXAMPLES:
;    To compute the cross-correlation function for two spectra, FIRST
;    and SECOND, using the recommended initial spectral range from CRSCOR,
;
;    CRSPROD,FIRST,SECOND,15,CROSS,CRMIN,CRMAX
;
;*SYSTEM VARIABLES USED:
;    None    
;
;*INTERACTIVE INPUT:
;    None
;
;*SUBROUTINES CALLED:
;    PARCHECK
;
;*FILES USED:
;    None
;
;*SIDE EFFECTS:
;
;*RESTRICTIONS:
;
;*NOTES:
;
;*PROCEDURE:
;     After subtracting the average flux from each spectrum, the cross
;     correlation function is computed from
;   
;     for each point in the spectra, 
;      TEMP = (second spectrum)*
;
;      CROSS(L) = TOTAL(TEMP)
;      END
;
;
;*MODIFICATION HISTORY:
;      Mar 11 1991      JKF/ACC    - moved to GHRS DAF (IDL Version 2)
;-
;-------------------------------------------------------------------------------
pro crsprod,ffir,fsec,nspr,cross,crmin,crmax

if n_params(0) lt 1 then $
	message,'Calling Sequence - crsprod,ffir,fsec,nspr,cross,crmin,crmax'

;
;  compute a normalized cross correlation function
;
  mi   = n_elements(ffir) 
  avg1 = total(ffir)/mi & ff= ffir - avg1    ; subtract average fluxes 
  avg2 = total(fsec)/mi & fs= fsec - avg2
  ntot = nspr+nspr+1
  cross= fltarr(ntot)
  temp = fs
  for l=0,ntot-1 do begin
      ns = nspr - l
      temp = fs*shift(ff,ns)
      ls = ns > 0
      us = mi  - 1 + (ns < 0)
      nele = us - ls + 1
      cross(l) = total(temp(ls:us)) / nele
      end
  crmax= max(cross) & crmin= min(cross)
  cross= (cross -crmin)/(crmax-crmin)       ; normalize function 
return     
end