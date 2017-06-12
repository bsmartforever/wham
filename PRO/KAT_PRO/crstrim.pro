;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
;      CRSTRIM (IUE Production Library) (01 April 1988)
;
;*CLASS:
;      cross-correlation, spectral extraction, resampling
;
;*CATEGORY:
;
;*PURPOSE:
;   To extract the spectral data within the user-specified wavelength
;   range, and to interpolate the spectral data to a common log(lambda)
;   scale. 
;
;*CALLING SEQUENCE:
;   CRSTRIM,BEGLAM,ENDLAM,W,F,N,RWLOG,FF,M,JUMP
;
;*PARAMETERS:
;   BEGLAM  (REQ) (I)  (0)   (F)
;           Beginning wavelength for the extraction, given in the same
;           units as the spectral data.
;   ENDLAM  (REQ) (I)  (0)   (F)
;           Ending wavelength for the extraction, given in the same
;           units as the spectral data.
;   W       (REQ) (I)  (1)   (F)
;           Required input vector giving the wavelength data for the 
;           spectrum of interest.
;   F       (REQ) (I)  (1)   (F)
;           Required input vector giving the flux data for the spectrum of
;           interest.
;   N       (REQ) (I)  (0)   (I)
;           Required input parameter giving the number of data points
;           in the spectrum.
;   RWLOG   (REQ) (I)  (1)   (F)
;           Required input vector giving the log(lambda) grid for the
;           spectral data to be resampled onto.
;   FF      (REQ) (O)  (1)   (F)
;           Required output vector containing the resampled flux data
;   M       (REQ) (O)  (0)   (I)
;           Required output scalar giving the number of points in the
;           extracted and resampled spectrum.
;   JUMP    (REQ) (O)  (0)   (I)
;           Required output error flag. If JUMP=0, the data are O.K..
;           If JUMP=1 then the spectral data do not include the 
;           wavelength range specified by the user for the cross-correlation.
;
;*EXAMPLES:
;  To trim and resample a spectrum from wave1 to wave2 onto a log(lambda)
;  grid given by RWLOG,
;
;  CRSTRIM,wave1,wave2,wave,flux,N_elements(wave),RWLOG,ff,m1,JUMP
;
;*SUBROUTINES CALLED:
;      TABINV
;      QUADTERP
;      PARCHECK
;
;*NOTES:
;     Used by procedure CRSCOR
;
;*PROCEDURE:
;     see documentation for CRSCOR
;
;*MODIFICATION HISTORY:
;     for earlier modification history, see CRSCOR
;     05  May 1988  CAG   GSFC   add VAX RDAF-style prolog, PARCHECK
;     Mar 4 1991      JKF/ACC    - moved to GHRS DAF (IDL Version 2), removed
;			IUE specific filename.
;-
;-------------------------------------------------------------------------------
PRO CRSTRIM,BEGLAM,ENDLAM,W,F,N,RWLOG,FF,M,JUMP
PARCHECK,N_PARAMS(DUM),9,'CRSTRIM'
;
; CHECK WAVELENGTHS AND INTERPOLATE TO LOG-LAMBDA SCALE
;
  IF (W(0) GT BEGLAM) OR (W(N-1) LT ENDLAM) THEN BEGIN
    PRINT,' WAVELENGTHS NOT INCLUSIVE,',W(0),' TO ',W(N-1)
    JUMP=1
  END ELSE BEGIN
    TABINV,W,BEGLAM,J
    TABINV,W,ENDLAM,K
    J=FIX(J)-1>0
    M=FIX(K+1)<(N-1)
    F1 = F(J:M)
    WLOG=ALOG10(W(J:M))                                   
    QUADTERP,WLOG-ALOG10(BEGLAM),F1,RWLOG,FF
  END
RETURN
END