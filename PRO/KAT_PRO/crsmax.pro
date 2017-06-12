;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
;    CRSMAX (IUE Production Library) (01 April 1988)
;
;*CLASS:
;    cross-correlation
;
;*CATEGORY:
;
;*PURPOSE:
;    DETERMINE THE MAXIMUM OF THE CROSS CORRELATION FUNCTION
;
;*CALLING SEQUENCE:
;    CRSMAX,CROSS,VSPACE,INP,DELV,SIGY
; 
;*PARAMETERS:
;   CROSS    (REQ) (I) (1)  (F)
;            Required input vector giving the cross-correlation function 
;            to be fit.
;
;   VSPACE   (REQ) (I) (1)  (F)
;            Required input vector specifying the velocity spacing for
;            the cross-correlation function.
;   
;   INP      (REQ) (I) (0)  (I)
;            Required input scalar specifying the number of Gaussian
;            components to be fit to the cross-correlation function.
;            If this parameter is zero, the cross-correlation function
;            will be fit using a least squares fit to the first difference
;            function, and the point of zero slope identified. 
;
;   DELV     (REQ) (O) (0)  (F)
;            Velocity difference (in km/s) of the two spectra as
;            determined from the maximum of the cross-correlation function.
;            This quantity is determined either by the centroid of the 
;            fitted gaussian, or via the first difference technique. 
;
;   SIGY     (REQ) (O) (0)  (F)
;            Required output variable giving the rms deviation
;            of the gaussian fit from the cross-correlation function.
;            This variable will be non-zero if and only if a gaussian
;            fit to the cross-correlation function is requested.
;
;   XPOS     (REQ) (IO) (0) (F)
;            x position to start writing output in device units.
;            
;   YPOS     (REQ) (IO) (0) (F)
;            y position to start writing output. Returned value
;            designates position of last line of output.
;
;*SYSTEM VARIABLES USED:
;
;*INTERACTIVE INPUT:
;     None
;
;*SUBROUTINES CALLED:
;      GAUSSFITS
;      LINFIT
;      PARCHECK
;
;*FILES USED:
;      None
;
;*NOTES:
;     See CRSCOR
;
;*PROCEDURE:
;     The normalized cross-correlation function can be fit either
;     with one or more gaussian profiles, or if the function has a single
;     well-defined maximum, via a least squares fit to the first differences,
;     and interpolating to the point of zero slope. 
;
;*EXAMPLES:
;     To fit a normalized cross-correlation function with one Gaussian,
;     CRSMAX,CROSS,VSPACE,1,DELV,SIGY
;
;     To fit the same cross-correlation function by finding the local
;     extremum,
;     CRSMAX,CROSS,VSPACE,0,DELV,DUMMY
;   
;*MODIFICATION HISTORY:
;     for earlier history, see CRSCOR
;
;     05  May 1988   CAG     GSFC   add VAX RDAF-style prolog, PARCHECK
;
;     8/25/89 RWT GSFC Unix mods: add xpos & ypos parameters, remove !c,
;         use xyouts instead of print,
;    Mar 4 1991      JKF/ACC    - moved to GHRS DAF (IDL Version 2)
;-
;-------------------------------------------------------------------------------
pro crsmax,cross,vspace,inp,delv,sigy,xpos,ypos
parcheck,n_params(),7,'crsmax'
;
; determine the maximum of the cross correlation function
;
  sigy = 0.0
  dely = 1.1 * !d.y_ch_size
  if inp gt 0 then begin     ; gaussian fit
      gaussfits,float(vspace),cross,0,inp,a,yfit,sig
      delv= a(0)
      sigy=sqrt(total((cross-yfit)*(cross-yfit))/(inp*3+1))
  end else begin             ; max by first differences
      ntot = n_elements(cross)
      j=max(cross)
      ind = where(cross eq j)
      kb=ind(0)-3>0 & ke=ind(0)+3<(ntot-1)
      ;xyouts,xpos,ypos,font=0,/device,'     km/s      function'
      f = '(2x,f9.4,2x,f9.4)'
      for k=kb,ke do begin   
        ypos = ypos - dely
        ;xyouts,xpos,ypos,font=0,/device,string(format=f,vspace(k),cross(k))
	end
      temp = cross - shift(cross,-1)
      diff = temp(kb:ke-2)
      temp = (vspace + shift(vspace,-1))/2.
      vsub = temp(kb:ke-2)
      nt = n_elements(diff)
      wt = fltarr(nt)
      xmid = float(nt)/2.0 - 0.5
      wt = 1. + 2*(xmid - abs(xmid-indgen(nt)))     ;calculate weightt 
      linfit,vsub,diff,wt,a,b,si                    ; use least squares fit
      delv = -a/b
      ypos = ypos - 2*dely
      stf = '(1x,a,f7.1," km/s for file1 rel. to file2")'
      st = string(format=stf,'Resulting velocity shift is',delv)
      ;xyouts,xpos,ypos,font=0,/device,st
    end ;max by first differences
return
end