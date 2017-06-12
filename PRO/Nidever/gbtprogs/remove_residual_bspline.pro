pro remove_residual_bspline,final,pl=pl


fname = final[0].object+'_ses'+strtrim(final[0].session,2)+'_s'+strtrim(min(final.scan),2)+'-'+strtrim(max(final.scan),2)

nchan = n_elements(final[0].comb_corr)
nfinal = n_elements(final)

; remove structure
;tot0 = TOTAL(final.comb_corr,2,/nan)/total(finite(final.comb_corr),2)
med = MEDIAN(final.comb_corr,dim=2)
;sig0 = mad(tot-medfilt1d(tot0,51,/edge))
sig0 = mad(final.comb_corr)
diff = final.comb_corr-med#replicate(1,nfinal)
smdiff = smooth(diff,[20,1],/edge_truncate,/nan)
xx = findgen(nchan)#replicate(1,nfinal)
mask = (abs(smdiff) gt 2.5*(sig0/sqrt(20)) and (final.comb_corr lt 5*sig0 or xx lt 275))
mask = convol(mask,fltarr(31,1)+1)
mask = mask/(mask>1)
;bd = where(abs(final.comb_corr-tot0#replicate(1,nfinal)) gt 2.5*sig0,nbd)
bd = where(mask eq 1,nbd)
temp = final.comb_corr
if nbd gt 0 then temp[bd]=!values.f_nan

; Do the correction in ~500 integration blocks
nblocks = round(nfinal/500.0) > 1
print,'Using ',strtrim(nblocks,2),' 500 integration blocks'

for i=0,nblocks-1 do begin

  lo = i*500
  hi = (lo+500-1) < (nfinal-1)
  if i eq nblocks-1 then hi=nfinal-1
  final1 = final[lo:hi]
  temp1 = temp[*,lo:hi]

  ; Remove a polynomial baseline from this session
  nfinal1 = n_elements(final1)
  ; THIS IS NOW DONE ABOVE.  If there's real structure it helps having
  ;  lots of integrations
  ;; remove structure
  ;;tot0 = TOTAL(final1.comb_corr,2,/nan)/total(finite(final1.comb_corr),2)
  ;med = MEDIAN(final1.comb_corr,dim=2)
  ;;sig0 = mad(tot-medfilt1d(tot0,51,/edge))
  ;sig0 = mad(final1.comb_corr)
  ;diff = final1.comb_corr-med#replicate(1,nfinal1)
  ;smdiff = smooth(diff,[20,1],/edge_truncate,/nan)
  ;xx = findgen(nchan)#replicate(1,nfinal1)
  ;mask = (abs(smdiff) gt 2.5*(sig0/sqrt(20)) and (final1.comb_corr lt 5*sig0 or xx lt 275))
  ;mask = convol(mask,fltarr(31,1)+1)
  ;mask = mask/(mask>1)
  ;;bd = where(abs(final1.comb_corr-tot0#replicate(1,nfinal1)) gt 2.5*sig0,nbd)
  ;bd = where(mask eq 1,nbd)
  ;temp = final1.comb_corr
  ;if nbd gt 0 then temp[bd]=!values.f_nan
  tot = TOTAL(temp1,2,/nan)/total(finite(temp1),2)
  ; stuff in tot0 for pixels with no good points
  bd = where(finite(tot) eq 0,nbd)
  if nbd gt 0 then tot[bd]=med[bd]

  ;subcube = cube[*,min(yindarr):max(yindarr),*]
  ;sz = size(subcube)
  ;tot1 = total(subcube,1)/sz[1]
  ;sig1 = mad(tot1)
  ;bd1 = where(tot1 gt 4*sig1,nbd1)
  ;if nbd1 gt 0 then tot1[bd1] = !values.f_nan
  ;tot = total(tot1,1)/sz[2]
  ;xx1 = findgen(sz[3])
  xx1 = findgen(nchan)
  yy1 = tot
  sig = mad(tot)
  ;bd = where(xx1 gt 275 or (xx1 ge 23 and xx1 le 37) or finite(tot) eq 0,nbd)
  bd = where(xx1 gt 275 or finite(tot) eq 0,nbd)
  invvar = fltarr(n_elements(xx1))+1.0/sig^2
  if nbd gt 0 then invvar[bd] = 0
  ;;bd = where(xx gt 275 or (xx ge 37 and xx le 53),nbd)
  ;remove,bd,xx1,yy1
  ; also remove MS region
  ;bd2 = where(xx1 ge 130 and xx1 le 180,nbd2)
  ;remove,bd2,xx1,yy1

  ; Set NAN to zero
  bd = where(finite(yy1) eq 0,nbd)
  if nbd gt 0 then yy1[bd]=0.0

  if fname eq 'MSTIP-survey_ses2_s10-42' then begin
    bd = where(xx1 ge 122 and xx1 le 160,nbd)
;    if nbd gt 0 then invvar[bd] = 0
  ;  remove,bd,xx1,yy1
  endif

  if fname eq 'MSgrid4_588_ses38_s10-13' then begin
    bd = where(xx1 ge 160 and xx1 le 252,nbd)
;    if nbd gt 0 then invvar[bd] = 0
  ;  remove,bd,xx1,yy1
  endif

  if fname eq 'MSgrid4_597_ses38_s14-29' then begin
    bd = where(xx1 ge 156 and xx1 le 250,nbd)
;    if nbd gt 0 then invvar[bd] = 0
  ;  remove,bd,xx1,yy1
  endif

  if fname eq 'MSgrid4_588_ses56_s34-49' then begin
    bd = where(xx1 ge 160 and xx1 le 252,nbd)
;    if nbd gt 0 then invvar[bd] = 0
  ;  remove,bd,xx1,yy1
  endif

  if fname eq 'MSgrid4_607_ses38_s30-45' then begin
    bd = where(xx1 ge 165 and xx1 le 242,nbd)
;    if nbd gt 0 then invvar[bd] = 0
  ;  remove,bd,xx1,yy1
  endif

  if fname eq 'MSgrid4_588_ses37_s30-41' then begin
    bd = where(xx1 ge 160 and xx1 le 252,nbd,comp=gd)
;    if nbd gt 0 then invvar[bd] = 0
  ;  remove,bd,xx1,yy1
  endif

  if fname eq 'MSgrid4_597_ses56_s50-65' then begin
    bd = where(xx1 ge 164 and xx1 le 241,nbd,comp=gd)
;    if nbd gt 0 then invvar[bd] = 0
  ;  remove,bd,xx1,yy1
  endif

  if fname eq 'MSgrid4_626_ses48_s8-10' then begin
    bd = where(xx1 ge 164 and xx1 le 200,nbd,comp=gd)
;    if nbd gt 0 then invvar[bd] = 0
  ;  remove,bd,xx1,yy1
  endif

  ; MSgrid4_636_ses48_s11-25_all2.dat
  ; This has a hump near the MS vel, but not sure if
  ; it's the baseline or not.
  if fname eq 'MSgrid4_636_ses48_s11-25' then begin
    bd = where(xx1 ge 163 and xx1 le 232,nbd,comp=gd)
;    if nbd gt 0 then invvar[bd] = 0
  ;  remove,bd,xx1,yy1
  endif


  ; Polynomial fit
  ;plcoef1 = robust_poly_fit(xx1,yy1,5)
  ;model = poly(xx1,plcoef1)
  ;diff = yy1-model
  ;sig = mad(diff)  
  ;
  ; Outlier rejection
  ;bd = where(abs(diff) gt 2.5*sig,nbd,comp=gd,ncomp=ngd)
  ;plcoef = robust_poly_fit(xx1[gd],yy1[gd],5)
  ;
  ;; The final1 model
  ;xx = findgen(nchan)
  ;plbase = poly(xx,plcoef)

  ; B-spline fit
  ;bd = where(abs(stemp-median([stemp])) gt 3*mad([sdata]),nbd,comp=gd)
  nord = 3 ;4
  bkspace = 50
  invvar[0:59]=0
  ;invvar[280:*]=0
  dum = bspline_iterfit(xx1,yy1,invvar=invvar,nord=nord,bkspace=bkspace,yfit=model1)
  gd1 = where(invvar gt 0.0,ngd1)
  diff = yy1-model1
  sig2 = mad(diff[gd1])

  ; Outlier rejection
  bd2 = where(abs(diff) gt 2.5*sig2,nbd2)
  if nbd2 gt 0 then invvar[bd2] = 0
  dum = bspline_iterfit(xx1,yy1,invvar=invvar,nord=nord,bkspace=bkspace,yfit=model)
  gd = where(invvar gt 0.0,ngd,comp=bd,ncomp=nbd)

  ; Fit polynomial to 0-60 pixels, since this is where
  ; pixels were masked in the reduction and there is possibly
  ; an offset
  plcoef1 = robust_poly_fit(xx1[0:59],yy1[0:59],2)
  plmodel1 = poly(xx1[0:59],plcoef1)
  diff = yy1[0:59]-plmodel1
  ; Outlier rejection
  bd = where(abs(diff) gt 2.5*sig,nbd,comp=gd,ncomp=ngd)
  plcoef = robust_poly_fit(xx1[gd],yy1[gd],2)
  plmodel = poly(xx1[0:59],plcoef)

  model[0:59] = plmodel

  ; Plotting
  if keyword_set(pl) then begin
    yr = [-4,4]*stddev(yy1[gd])+median(yy1[gd])
    yr[0] = -0.035
    ;yr = [-0.1,0.1]
    plot,findgen(nchan),tot,ps=1,/xsty,yr=yr,ys=1,xr=[0,275],$
      tit=fname+'  Block '+strtrim(i+1,2)+' of '+strtrim(nblocks,2)
    oplot,xx1,yy1,ps=1,co=150
    ;plot,xx1,yy1,ps=1,tit=fname,/xsty,yr=yr,ys=1
    if nbd gt 0 then oplot,[xx1[bd]],[yy1[bd]],ps=1,co=250
    ;;swave = func_cospoly(xx,fpar)
    ;;oplot,xx,swave,co=250
    ;oplot,xx,poly(xx,plcoef),co=150
    oplot,xx1,model,co=250
    oplot,[0,300],[0,0],linestyle=2
    ;residuals
    oplot,xx1,yy1-model-0.020,ps=4
    oplot,[0,300],[0,0]-0.020,linestyle=2
    if nblocks gt 1 then wait,0.5
  endif

  ;stop

  ; Now remove it
  ;base2 = plbase#replicate(1.0,nfinal1)
  base2 = model#replicate(1.0,nfinal1)
  final1.comb_corr -= base2  ; correct comb_corr

  ; Stuff in FINAL
  final[lo:hi].comb_corr = final1.comb_corr

  ;stop

endfor

;stop

end
