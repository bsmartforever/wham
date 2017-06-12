pro robust_fit,x,y,nord,coef,perror=perror,chisq=chisq,sigma=sigma,$
               final=final,stp=stp,noplot=noplot,silent=silent

;+
; This does a polynomial fit to data
; Basically copied from fit_bvteff.pro
; Can also use Buie's GOODPOLY.PRO
;
; INPUTS:
;  x       The array of X values
;  y       The array of Y values
;  nord    The order of the polynomial to fit
;  =final  The fitted values
;  /stp    Stop at the end
;  /noplot Don't plot the results
;  /silent Don't output anything
;
; OUTPUTS:
;  coef    The polynomial coefficients of the fit
;  =perror The errors in the coefficients
;  =sigma  The stdev between the input values and fit
;  =chisq  The chi squared of the fit
;
; By D.Nidever Oct. 2006
;-

n = n_elements(x)

; Not enough inputs
if n eq 0 then begin
  print,'Syntax - robust_fit,x,y,nord,coef'
  return
end

ind = where(finite(x) eq 1 and finite(y) eq 1,nind)
nx = nind

yy = y[ind]
xx = x[ind]

; Remove a line first
;lin = fit(yy,1,xx,lincoef)
lincoef = goodpoly(xx,yy,1,3.0,lin)  ;,newx,newy)

y2 = yy - lin
mndiff = mean(y2)
sig = stddev(y2)


; First fit with GOODPOLY
coef = goodpoly(xx,y2,nord,3.0,f)
sigma = stdev(f-y2)
;f = fit(y2,nord,xx,coef,sigma)
mndiff = mean(abs(f-y2))
pars1 = reform(coef)
gd = where(abs(f-y2) lt (mndiff+3.0*sigma),ngd)

; Second fit with GOODPOLY
pars2 = goodpoly(xx[gd],y2[gd],nord,3.0,f2)
mndiff = mean(f2-y2[gd])
sig = stdev(f2-y2[gd])
bd = where(abs(f2-y2[gd]) gt mndiff+2.0*sig,ngd) 
remove,bd,gd
ngd = n_elements(gd)


; Fit with MPFITEXPR.PRO with only good points
err = fltarr(n)+1.0
expr = 'p(0)'
for i=1,nord do expr=expr+'+p('+strtrim(i,2)+')*x^'+strtrim(i,2)+'.0'

pars3 = mpfitexpr(expr, xx[gd], y2[gd], err[gd], pars2, bestnorm=chisq3, perror=perror3,/quiet)
f3 = mpevalexpr(expr,xx[gd],pars2)

; Scaling the errors by chisq
dof = ngd - n_elements(pars3)
perror = perror3 * sqrt(chisq3/dof)

; Putting final coefficients together
coef = pars2
coef[0] = coef[0] + lincoef[0,0]
coef[1] = coef[1] + lincoef[0,1]

; Getting fitted values for the inputs
final = x*0.0+99.99
final[ind] = poly(xx,coef)

; Getting the sigma value
sigma = stdev(final[ind] - yy)

; Get the fit values
xfit = scale_vector(findgen(1000),min(xx),max(xx))
yfit = poly(xfit,coef)

; Plotting
if not keyword_set(noplot) then begin
  loadct,39,/silent

  minx = min(xx)
  maxx = max(xx)
  miny = min(yy)
  maxy = max(yy)
  xrange = maxx-minx
  yrange = maxy-miny
  xr = [minx-0.1*xrange,maxx+0.1*xrange]
  yr = [miny-0.1*xrange,maxy+0.1*yrange]

  plot,xx,yy,ps=1,xtit='X',ytit='Y',xr=xr,yr=yr,xs=1,ys=1
  oplot,xx[gd],yy[gd],ps=1,co=150
  oplot,xfit,yfit,co=250

end ; not /noplot

; Printing results
if not keyword_set(silent) then begin
  print,'Chi Sq = ',chisq3
  print,'Sigma = ',sigma

  coef_out = 'COEF = [ '
  for i=0,nord do begin
    if i ne nord then tag = ',  ' else tag =''
    coef_out = coef_out+string(coef[i],format='(G12.7)')+tag
  end
  coef_out = coef_out+' ]'

  perror_out = 'PERROR = [ '
  for i=0,nord do begin
    if i ne nord then tag = ',  ' else tag =''
    perror_out = perror_out+string(perror[i],format='(G12.7)')+tag
  end
  perror_out = perror_out+' ]'

  ;Printing
  print,coef_out
  print,perror_out

  ;print,FORMAT='(A9,'+strtrim(nord+1,2)+'(F0,:,",  "),A2)','COEF = [ ',coef,' ]'

end  ; not /silent

if keyword_set(stp) then stop

end
