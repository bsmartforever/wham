pro getphot_xy,x,y,u,g,r,i,z,plot=plot,trap=trap,spl=spl,fuv=fuv,nuv=nuv,red=red

;+
;	We derive synthetic photometry for the SDSS system (ugriz) ;
;	from an input spectrum
;
;	IN: 	x	fltarr  array of wavelengths (AA)
;			y	fltarr  array of fluxes (F_lambda erg/cm2/s/AA)
;	
;	OUT:	u	float	u magnitude
;		g	float	g magnitude
;		r	float	r magnitude
;		i	float	i magnitude
;		z	float	z magnitude
;
;	KEYWORDS: plot	shows the filters response on top of the flux 
;	distribution
;
;		  trap changes the integration method from a 5-point
;			Newtow-Cotes formula to the composed trapezoidal rule
;
;		  spl changes the interpolation of the filter responses to
;			splines instead of linear
;
;		 nuv GALEX band is output on request
;		 fuv GALEX band ""
;
;		  red - adds some amount of reddening (std. R=3.1 curve,
;			see Fitzpatrick 1999, PASP, 111, 63; astro-ph/9809387)
;
;	Response curves from 
;       http://www.sdss3.org/instruments/filters
;
;	C. Allende Prieto, UT, Aug 2002
;	"		       Feb 2004 - changed to handle the lack of a
;						model fluxes graciously
;			       May 2005 - changed to interpolate the responses
;						instead of the fluxes; keywords
;						trap and spl added
;
;				June 2008 - adapted from getphot.pro
;				October 2008 - added nuv/fuv keywords
;				April 2010 - avoid returning a modified y array
;				October 2011 -- added red keyword
;-

rpath='~/idl/idl_database'

if n_elements(x) eq 1 then begin
	print,'% GETPHOT: No photometry returned'
	u=-1000
	g=-1000
	r=-1000
	i=-1000
	z=-1000
	fuv=-1000
	nuv=-1000
	return
endif 

if n_params() lt 3 then begin
	print,'% GETPHOT_XY:  use -- getphot_xy,x,y,u,g,r,i,z[,plot=plot,trap=trap,spl=spl,fuv=fuv,nuv=nuv,red=red]'
	return
endif

;hold on to the original values for y, so they can be restored later
y_original=y

if keyword_set(red) then begin
	if red lt 0.0 then print,'% GETPHOT_XY: Input reddening is < 0 - the synthetic fluxes will be DEREDDENED'
	fm_unred,x,y,-red
endif

; K models give f_lambda, so we first change that to f_nu
l2nu,x,y,nu,fnu & 	y2=fnu

;get the right units for the GALEX calculations
y=y*1d4

get_lun,l
openr,l,rpath+'/sdss_response/u_response.dat'
u=fltarr(5,47)
readf,l,u
close,l
openr,l,rpath+'/sdss_response/g_response.dat'
g=fltarr(5,89)
readf,l,g
close,l
openr,l,rpath+'/sdss_response/r_response.dat'
r=fltarr(5,75)
readf,l,r
close,l
openr,l,rpath+'/sdss_response/i_response.dat'
i=fltarr(5,89)
readf,l,i
close,l
openr,l,rpath+'/sdss_response/z_response.dat'
z=fltarr(5,141)
readf,l,z
close,l
free_lun,l

if arg_present(fuv) then begin
	load,rpath+'/galex_response/galex_FUV.par',fuv,sk=46
	fuv=double(fuv[1:2,*]) & fuv[1,*]=fuv[1,*]/100.	
endif
if arg_present(nuv) then begin
	load,rpath+'/galex_response/galex_NUV.par',nuv,sk=46
	nuv=double(nuv[1:2,*]) & nuv[1,*]=nuv[1,*]/100.
endif

xu=where(x ge min(u(0,*)) and x le max(u(0,*)))
xg=where(x ge min(g(0,*)) and x le max(g(0,*)))
xr=where(x ge min(r(0,*)) and x le max(r(0,*)))
xi=where(x ge min(i(0,*)) and x le max(i(0,*)))
xz=where(x ge min(z(0,*)) and x le max(z(0,*)))
if arg_present(fuv) then xfuv=where(x ge min(fuv(0,*)) and x le max(fuv(0,*)))
if arg_present(nuv) then xnuv=where(x ge min(nuv(0,*)) and x le max(nuv(0,*)))

if keyword_set(spl) then begin
	uu=spline(u(0,*),u(1,*),x[xu])
	gg=spline(g(0,*),g(1,*),x[xg]) 
	rr=spline(r(0,*),r(1,*),x[xr]) 
	ii=spline(i(0,*),i(1,*),x[xi]) 
	zz=spline(z(0,*),z(1,*),x[xz]) 
if arg_present(fuv) then 	ffuv=spline(fuv(0,*),fuv(1,*),x[xfuv])
if arg_present(nuv) then 	nnuv=spline(nuv(0,*),nuv(1,*),x[xnuv])
endif else begin
	uu=interpol(u(1,*),u(0,*),x[xu])
	gg=interpol(g(1,*),g(0,*),x[xg]) 
	rr=interpol(r(1,*),r(0,*),x[xr]) 
	ii=interpol(i(1,*),i(0,*),x[xi]) 
	zz=interpol(z(1,*),z(0,*),x[xz]) 	
if arg_present(fuv) then 	ffuv=interpol(fuv(1,*),fuv(0,*),x[xfuv])
if arg_present(nuv) then 	nnuv=interpol(nuv(1,*),nuv(0,*),x[xnuv])
endelse

; from Fig. 5 in Gunn et al. 1998 I think the filter responses
; 	from the sdss/stsci web site ( Michael Strauss, Jim Gunn, Aug. 2001)
;	are S_nu, instead of S_lambda, so I don't touch that, otherwise the
; 	lines below would be uncommented
;	I use the values for airmass=1.3 and a point source
;	
;l2nu,u(0,*),u(1,*),nu,fnu & u(1,*)=fnu
;l2nu,g(0,*),g(1,*),nu,fnu & g(1,*)=fnu
;l2nu,r(0,*),r(1,*),nu,fnu & r(1,*)=fnu
;l2nu,i(0,*),i(1,*),nu,fnu & i(1,*)=fnu
;l2nu,z(0,*),z(1,*),nu,fnu & z(1,*)=fnu


; now we follow Eq. (3) in Lenz et al. (1998) or Eq. (7) in 
; Fukugita et al. (1996):
;	num= integral[f_nu*S_nu d(alog10(nu))] 
;	   propto  integral[f_nu*S_nu/lambda d(lambda)]
;	den= integral[S_nu d(alog10(nu))] 
;	   propto  integral[S_nu/lambda d(lambda)]
; m=-2.5*alog10(num/den)-48.60
;

if keyword_set(trap) then begin
	num=trapz(x[xu],y2[xu]*uu/x[xu])
	den=trapz(x[xu],uu/x[xu])
	mu=-2.5d0*alog10(num/den)-48.6d0

	num=trapz(x[xg],y2[xg]*gg/x[xg])
	den=trapz(x[xg],gg/x[xg])
	mg=-2.5d0*alog10(num/den)-48.6d0

	num=trapz(x[xr],y2[xr]*rr/x[xr])
	den=trapz(x[xr],rr/x[xr])
	mr=-2.5d0*alog10(num/den)-48.6d0

	num=trapz(x[xi],y2[xi]*ii/x[xi])
	den=trapz(x[xi],ii/x[xi])
	mi=-2.5d0*alog10(num/den)-48.6d0

	num=trapz(x[xz],y2[xz]*zz/x[xz])
	den=trapz(x[xz],zz/x[xz])
	mz=-2.5d0*alog10(num/den)-48.6d0

if arg_present(fuv) then begin	
	num=trapz(x[xfuv],y[xfuv]*ffuv)
	;den=trapz(x[xfuv],ffuv)
	mfuv=-2.5d0*alog10(num)
endif

if arg_present(nuv) then begin
	num=trapz(x[xnuv],y[xnuv]*nnuv)
	;den=trapz(x[xnuv],nnuv)
	mnuv=-2.5d0*alog10(num)
endif

endif else begin
	num=int_tabulated(x[xu],y2[xu]*uu/x[xu])
	den=int_tabulated(x[xu],uu/x[xu])
	mu=-2.5d0*alog10(num/den)-48.6d0

	num=int_tabulated(x[xg],y2[xg]*gg/x[xg])
	den=int_tabulated(x[xg],gg/x[xg])
	mg=-2.5d0*alog10(num/den)-48.6d0

	num=int_tabulated(x[xr],y2[xr]*rr/x[xr])
	den=int_tabulated(x[xr],rr/x[xr])
	mr=-2.5d0*alog10(num/den)-48.6d0

	num=int_tabulated(x[xi],y2[xi]*ii/x[xi])
	den=int_tabulated(x[xi],ii/x[xi])
	mi=-2.5d0*alog10(num/den)-48.6d0

	num=int_tabulated(x[xz],y2[xz]*zz/x[xz])
	den=int_tabulated(x[xz],zz/x[xz])
	mz=-2.5d0*alog10(num/den)-48.6d0
	
	if arg_present(fuv) then begin
	num=int_tabulated(x[xfuv],y[xfuv]*ffuv)
	;den=int_tabulated(x[xfuv],ffuv)
	mfuv=-2.5d0*alog10(num)
	endif

	if arg_present(nuv) then begin
	num=int_tabulated(x[xnuv],y[xnuv]*nnuv)
	;den=int_tabulated(x[xnuv],nnuv)
	mnuv=-2.5d0*alog10(num)
	endif

endelse


if keyword_set(plot) then begin
if arg_present(nuv) or arg_present(fuv) then minlam=1000. else minlam=3000.
plot,x,fnu,xr=[minlam,11000],xstyl=1,xtitle='Wavelength (A)',$
ytitle='f_nu (erg cm-2 s-1 Hz-1)',charsi=1.4,position=[0.2,0.1,0.95,0.95]
oplot,u(0,*),u(1,*)/max(r(1,*))*max(y2)
oplot,g(0,*),g(1,*)/max(r(1,*))*max(y2)
oplot,r(0,*),r(1,*)/max(r(1,*))*max(y2)
oplot,i(0,*),i(1,*)/max(r(1,*))*max(y2)
oplot,z(0,*),z(1,*)/max(r(1,*))*max(y2)
if arg_present(nuv) then oplot,nuv(0,*),nuv(1,*)/max(nuv(1,*))*max(y2)
if arg_present(fuv) then oplot,fuv(0,*),fuv(1,*)/max(fuv(1,*))*max(y2)
endif

u=mu & g=mg & r=mr & i=mi & z=mz  
if arg_present(fuv) then fuv=mfuv  
if arg_present(nuv) then nuv=mnuv

;restore original y
y=y_original

end
