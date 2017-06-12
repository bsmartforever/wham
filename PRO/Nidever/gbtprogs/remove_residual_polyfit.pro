pro remove_residual_polyfit,final,pl=pl


fname = final[0].object+'_ses'+strtrim(final[0].session,2)+'_s'+strtrim(min(final.scan),2)+'-'+strtrim(max(final.scan),2)

nchan = n_elements(final[0].comb_corr)

  ; Remove a polynomial baseline from this session
  nfinal = n_elements(final)
  tot = TOTAL(final.comb_corr,2)/nfinal
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
  bd = where(xx1 gt 275 or (xx1 ge 23 and xx1 le 37) or finite(tot) eq 0,nbd)
  ;bd = where(xx gt 275 or (xx ge 37 and xx le 53),nbd)
  remove,bd,xx1,yy1
  ; also remove MS region
  ;bd2 = where(xx1 ge 130 and xx1 le 180,nbd2)
  ;remove,bd2,xx1,yy1

  if fname eq 'MSTIP-survey_ses2_s10-42' then begin
    bd = where(xx1 ge 122 and xx1 le 160,nbd)
    remove,bd,xx1,yy1
  endif

  if fname eq 'MSgrid4_588_ses38_s10-13' then begin
    bd = where(xx1 ge 160 and xx1 le 252,nbd)
    remove,bd,xx1,yy1
  endif

  if fname eq 'MSgrid4_597_ses38_s14-29' then begin
    bd = where(xx1 ge 156 and xx1 le 250,nbd)
    remove,bd,xx1,yy1
  endif

  if fname eq 'MSgrid4_588_ses56_s34-49' then begin
    bd = where(xx1 ge 160 and xx1 le 252,nbd)
    remove,bd,xx1,yy1
  endif

  if fname eq 'MSgrid4_607_ses38_s30-45' then begin
    bd = where(xx1 ge 165 and xx1 le 242,nbd)
    remove,bd,xx1,yy1
  endif

  if fname eq 'MSgrid4_588_ses37_s30-41' then begin
    bd = where(xx1 ge 160 and xx1 le 252,nbd)
    remove,bd,xx1,yy1
  endif

  if fname eq 'MSgrid4_597_ses56_s50-65' then begin
    bd = where(xx1 ge 164 and xx1 le 241,nbd)
    remove,bd,xx1,yy1
  endif

  if fname eq 'MSgrid4_626_ses48_s8-10' then begin
    bd = where(xx1 ge 164 and xx1 le 200,nbd)
    remove,bd,xx1,yy1
  endif

  ; MSgrid4_636_ses48_s11-25_all2.dat
  ; This has a hump near the MS vel, but not sure if
  ; it's the baseline or not.
  if fname eq 'MSgrid4_636_ses48_s11-25' then begin
    bd = where(xx1 ge 163 and xx1 le 232,nbd)
    remove,bd,xx1,yy1
  endif

  ;initpars = [0.004d0,159, 44.5, 0.0, 0.0, 0.0]
  ;npar = n_elements(initpars)
  ;parinfo = replicate({value:0.0,fixed:0,limited:[1,1],limits:[0.0,0.0]},npar)
  ;parinfo[0].limits = [0.00,0.04]
  ;parinfo[1].limits = [150,170]
  ;parinfo[2].limits = [0.0,100]
  ;parinfo[3:*].limited = 0
  ;
  ;; func_cos
  ;fpar1 = MPFITFUN('func_cospoly',xx1,yy1,xx1*0+1,initpars,/quiet,parinfo=parinfo,$
  ;                status=status1,niter=niter1)
  ;;if status1 lt 1 then stop,'STATUS1<1'
  ;
  ;swave1 = func_cospoly(xx1,fpar1)
  ;
  ;; do a second cut
  ;diff = yy1-swave1
  ;bd2 = where(diff gt 3*sig,nbd2)
  ;if nbd2 gt 0 then remove,bd2,xx1,yy1
  ;
  ;fpar = MPFITFUN('func_cospoly',xx1,yy1,xx1*0+1,initpars,/quiet,parinfo=parinfo,$
  ;                status=status1,niter=niter1)

  ; Polynomial fit
  plcoef1 = robust_poly_fit(xx1,yy1,5)
  model = poly(xx1,plcoef1)
  diff = yy1-model
  sig = mad(diff)  

  ; Outlier rejection
  bd = where(abs(diff) gt 2.5*sig,nbd,comp=gd,ncomp=ngd)
  plcoef = robust_poly_fit(xx1[gd],yy1[gd],5)

  ; The final model
  xx = findgen(nchan)
  plbase = poly(xx,plcoef)

  ; Plotting
  if keyword_set(pl) then begin
    yr = [-4,4]*stddev(yy1)+median(yy1)
    ;yr = [-0.1,0.1]
    plot,findgen(nchan),tot,ps=1,tit=fname,/xsty,yr=yr,ys=1,xr=[0,275]
    oplot,xx1,yy1,ps=1,co=150
    ;plot,xx1,yy1,ps=1,tit=fname,/xsty,yr=yr,ys=1
    if nbd gt 0 then oplot,[xx1[bd]],[yy1[bd]],ps=1,co=250
    ;swave = func_cospoly(xx,fpar)
    ;oplot,xx,swave,co=250
    oplot,xx,poly(xx,plcoef),co=150
  endif


  ; Now remove it
  base2 = plbase#replicate(1.0,nfinal)
  final.comb_corr -= base2  ; correct comb_corr

  ;stop

end
