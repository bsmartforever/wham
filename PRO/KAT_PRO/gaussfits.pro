;************************************************************************
;+
;*NAME:
;
;    GAUSSFITS     JUNE,1984
;  
;*PURPOSE:
;    To fit Gaussians and a polynomial baseline to data points
;  
;*CALLING SEQUENCE:
;       GAUSSFITS,X,Y,NDEG,NCOMP,A,YFIT,SIG
;  
;*PARAMETERS:
;
;    	X	(REQ) (I) (1) (I L F D)
;		Independent variable vector.
;
;    	Y	(REQ) (I) (1) (I L F D)
;		Dependent variable vector.
;
;    	NDEG	(REQ) (I) (1) (I)
;		Degree of polynomial to be fit to baseline -1 signifies no 
;		fit is to be made to baseline.
;
;    	NCOMP	(REQ) (I) (1) (I)
;		Number of Gaussian components.
; 
;	A	(REQ) (O) (1) (I L F D)
;		Vector of function parameters.
;                 	A(3I-3) is the center of the Ith Gaussian
;                     	A(3I-2) is the 1 sigma width of the Ith Gaussian
;                     	A(3I-1) is the height of the Ith Gaussian above
;                     	        the baseline
;                     	A also stores the parameters of the baseline fit
;                     	in the last NDEG+1 elements.
;
;    	YFIT	(REQ) (O) (1) (I L F D)
;		Vector of calculated Y values.
;
;    	SIG	(REQ) (O) (1) (I L F D)
;		Vector of 1 sigma errors in fitted parameters of Gaussians 
;		(ordered like A).
;  
;*INTERACTIVE INPUT:
;
;     	The user specifies the boundary of the feature to be fitted
;     	and the approximate location of the Gaussians with the
;     	cursor.
;  
;*FILES USED:
;  
;*SYSTEM VARIABLES USED:
;  
;*SUBROUTINES CALLED:
;
;    	BASEREM
;    	GAUSS
;    	WFIT
;  
;*SIDE EFFECTS:
;  
;*RESTRICTIONS:
;  
;*NOTES:
;
;     - GAUSSFIT outputs a plot showing the data, the individual
;       Gaussian components, the sum of all components, and the 
;       deviations. The symbols used are shown below:
;     - histogram: vector of data points Y
;     - solid line: the total fit (sum of all Gaussians & baseline)
;     - dots: individual Gaussian components
;     - plus signs: difference between data points and total fit
;     - High order fits to baseline may require scaling input arrays.
;  
;*PROCEDURE:
;
;     	The routine uses BASEREM to fit the baseline with a
;     	polynomial or makes no baseline fitting as specified 
;     	by the user.  The procedure uses the standard  
;     	deviation of a single point in the baseline section of the 
;     	data to create an equal-weight vector for all data points.   
;     	The user inputs the approximate location and height of each 
;     	Gaussian with the cursor.  The procedure uses WFIT to solve for 
;     	the center, dispersion, and height of all Gaussian components, 
;     	and their 1 sigma errors.  Absolute errors for the Gaussian 
;     	parameters are calculated using the 1 sigma errors of a point
;     	in the baseline fit.
;  
;*INF_1:
;  
;*EXAMPLES:
;
;  
;*MODIFICATION HISTORY:
;
;     	PDP VERSION: I. D. AHMAD
;     	VAX mods: R. Thompson & C. Grady
;     	June, 1984: N. R. Evans: to correct WFIT calling error,
;             	introduce sigma squared weighting, use sigma from 
;             	baseline fit to determine absolute errors, correct
;             	plotting, and document.
;     	7-7-84 RWT modified WT value passed to WFIT, moved YFIT
;             	routine to WFIT and updated documentation.
;     	9-20-84 RWT deleted unnecessary code, corrected code for extracting
;             	feature from input array (i.e. backgnd subtraction) and uses new
;             	version of WFIT.
;     	4-13-87 RWT VAX mods: add PARCHECK & PLTPARM, remove INSERT, and use
;             	SET_XY
;     	9-21-87 RWT correct error in DELTAA(IN) calculation
;     	10-7-87 CAG rename procedure as GAUSSFITS, and alter order of plotting
;             	so that plot autoscales correctly for observed data.
;     	11-16-87 RWT VAX mods: remove restrictions of baseline
;             	polynomial, improve plot annotation, compress code, 
;             	use new BASEREM, and change name to GAUSSFITS
;         8-24-89 RWT Unix mods: 
;       Mar 4 1991      JKF/ACC    - moved to GHRS DAF (IDL Version 2)
;-
;************************************************************************
pro gaussfits,x,y,ndeg,ncomp,a,yfit,sig
;
npar = n_params(0)
if npar eq 0 then begin
   print,' GAUSSFITS,X,Y,NDEG,NCOMP,A,YFIT,SIG'
   retall & end
parcheck,npar,7,'gaussfits'
ind    = 3 * ncomp
nterms = ndeg + ind + 1
npts   = n_elements(x)
b      = fltarr(ind)
a      = fltarr(nterms)
deltaa = b
;
; fit polynomial to baseline
;
  xl=0.0 & xr=0.0
  baserem,x,y,ndeg,xl,xr,a,ybf,chis
;
; extract feature & subtract background from y array 
;
  xg = x(xl:xr)
  i = 0
  yfit = a(ind)
  while i lt ndeg do begin
       i = i+1
       yfit = yfit + a(ind+i)*(xg^i)
       end
  yg = y(xl:xr) - yfit
;
; estimate starting parameters of gaussian(s) for wfit
;
  b1=(x(xr)-x(xl))/(6.4*ncomp)  ; first estimate of one sigma width
  sq=sqrt((xr-xl+1.)/ncomp)*3.
  plot,xg,yg
  xpos = !d.x_ch_size 
  ypos = !d.y_size - !d.y_ch_size 
  dely = 1.1 * !d.y_ch_size
  xyouts,xpos,ypos,font=0,/device, $
    'place cursor at each component peak (left to right) & press any key'
  for i=0,ncomp-1 do begin
    in = i*3
    cursor,xc,yc,1,/data          ; 1st estimate of gaussian peak
    b([in,in+1,in+2])  = [xc,b1,yc]
    deltaa([in,in+1,in+2])  = [b1/sq,b1/sq,sqrt(chis)]
    wait,0.1
    end ; i loop
; 
; use wfit to fit gaussian to feature (use 1/chis for wt)
;
  ifit  = intarr(ind) + 1
  wfit,xg,yg,yg*0+1/chis,0.,ifit,deltaa,b,ygf,sig      
  for i = 0,ncomp-1 do b(3*i+1) = abs(b(3*i+1))
  a(0) = b                              ; set first 3 parameters to fit values
;
; insert feature + baseline combination into yfit
;
  yfit  = 0*x
  yfit(xl) = ygf
  i = 0
  bfit = a(ind)
  while i lt ndeg do begin
       i = i+1
       bfit = bfit + a(ind+i)*(x^i)
       end
  yfit  = yfit + bfit
;
; set graph limits & plot output
;
pmax = max([max(y),max(yfit),max(y-yfit)])
pmin = min([min(y),min(yfit),min(y-yfit)])
  plot,x,y,psym=10,yrange=[pmin,pmax]  ; actual pts marked using histogram
  oplot,x,yfit,psym=0   ; fitted pts connected by line
  oplot,x,y-yfit,psym=1   ; y-yfit shown as '+'
  for i=1,ncomp do begin  
      i3=(i-1)*3
      gauss,x,a(i3),a(i3+1),a(i3+2),yf
      oplot,x,yf,psym=3  ; dots for indiv. gaussians
      end ; i loop
;
;    restore original plot parameters
;
  ypos = ypos - dely
  xyouts,xpos,ypos,/device,font=0,'Gaussian Component(s):'
  ypos = ypos - dely
  xyouts,xpos,ypos,/device,font=0,  $
   '     Center     (error)      Sigma    (error)       Peak     (error)'
  stf = "(2(f11.4,2x,'(',f8.5,')'),1x,e11.2,2x,'(',e8.2,')')"
  for i=0,3*(ncomp-1),3 do begin
    ypos = ypos - dely
    st = string(format=stf,b(i),sig(i), b(i+1),sig(i+1),b(i+2),sig(i+2))
    xyouts,xpos,ypos,font=0,/device,st
    end
return
end