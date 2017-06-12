;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; civ_fn.pro               
; Author: Kathy Cooksey                      Date: 17 Sep 2008
; Project: HST CIV survey with Xavier Prochaska
; Description: 
; Input: 
; Optional:
; Output: 
; Example:
; History:
;   17 Sep 2008  created by KLC
;   11 Dec 2008  Use dX and the EW cut from g(z)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@civ_calcewn                    ; need civ_calcewn_ndblt()
@civ_sensitivity                ; need civ_sensitivity_x()
pro civ_fn_plot,psfil, strct_fil, sens_fil, siiv=siiv, $
                fnstrct_fil=fnstrct_fil, $
                csize=csize,psize=psize,lthick=lthick,$
                xrng=xrng,yrng=yrng, log=log, few=few, $
                fit_fil=fit_fil, label=label, _extra=extra
  if (N_params() lt 1) then begin
     print,'Syntax - '+$
           'civ_fn_plot,psfil, strct_fil, sens_fil, few=, fnstrct_fil=,'
     print,'            csize=, psize=psize,lthick=, label='
     print,'            xrng=, yrng=, log=log, fit_fil=, _extra='
     return
  endif

  if keyword_set(fnstrct_fil) then begin
     ;; Read in f(N)
     if size(fnstrct_fil,/type) eq 8 then fnstrct = fnstrct_fil $
     else fnstrct = xmrdfits(fnstrct_fil,1,/silent)
  endif else begin
     ;; Create f(N)
     civ_fn, strct_fil, sens_fil, siiv=siiv, few=few,$
             ostrct_fil=fnstrct,_extra=extra ; for civ_group
  endelse 

  nbin = n_elements(fnstrct.locn)
  bdfn = where(fnstrct.fn le 0.,nbdfn,complement=gdfn)
  if keyword_set(log) then begin
     fnstrct.sigfn[gdfn,0] = fnstrct.sigfn[gdfn,0]/$
                             abs(alog(10)*fnstrct.fn[gdfn])
     fnstrct.sigfn[gdfn,1] = fnstrct.sigfn[gdfn,1]/$
                             abs(alog(10)*fnstrct.fn[gdfn])
     fnstrct.fn[gdfn] = alog10(fnstrct.fn[gdfn])
     if nbdfn ne 0 then begin
        ;; Set 2-sigma upper limit
        fnstrct.fn[bdfn] = alog10(2.*fnstrct.sigfn[bdfn,1])
        fnstrct.sigfn[bdfn,*] = 0.
     endif 
  endif else begin
     if nbdfn ne 0 then begin
        fnstrct.fn[bdfn] = 1.*fnstrct.sigfn[bdfn,1]
        fnstrct.sigfn[bdfn,*] = 0.
     endif 
  endelse 

  ;; Params
  if not keyword_set(csize) then csize = 1.5
  if not keyword_set(psize) then psize = 2.
  if not keyword_set(lthick) then lthick = 2
  if not keyword_set(xrng) then $
     xrng = [fnstrct.locn[0]-fnstrct.siglocn[0,0]-0.1,$
             fnstrct.locn[nbin-1]+fnstrct.siglocn[nbin-1,1]+0.1]

  if not keyword_set(yrng) then begin
     yrng = [min(fnstrct.fn[gdfn])-max(fnstrct.sigfn[gdfn,0]),$
             max(fnstrct.fn[gdfn])+$
             max(fnstrct.sigfn[gdfn,1])] 
     if keyword_set(log) then yrng = yrng + [-0.02,0.02] $
     else yrng = yrng + [-1e-20,1e-15]
  endif

  if keyword_set(few) then begin
     if keyword_set(siiv) then begin
        if keyword_set(log) then ytitle = 'log f(W!Dr,1393!N)'  $
        else ytitle = 'f(W!Dr,1393!N)'
        xtitle = 'log W!Dr,1393!N (m'+STRING("305B) +')'
     endif else begin
        if keyword_set(log) then ytitle = 'log f(W!Dr,1548!N)'  $
        else ytitle = 'f(W!Dr,1548!N)'
        xtitle = 'log W!Dr,1548!N (m'+STRING("305B) +')'
     endelse 
  endif else begin
     if keyword_set(siiv) then begin
        if keyword_set(log) then ytitle = 'log f(N(Si!E+3!N))' $
        else ytitle = 'f(N(Si!E+3!N))'
        xtitle = 'log N(Si!E+3!N)'
     endif else begin
        if keyword_set(log) then ytitle = 'log f(N(C!E+3!N))' $
        else ytitle = 'f(N(C!E+3!N))'
        xtitle = 'log N(C!E+3!N)'
     endelse 
  endelse 

  x_psopen,psfil,/maxs
  !p.multi = [1,1,1]
  !x.margin = [8,3]
  !y.margin = [4,2]

  clr = getcolor(/load)

  plot,xrng,yrng,/nodata,/ystyle,/xstyle,background=clr.white,color=clr.black,$
       ytitle=ytitle,xtitle=xtitle,charsize=csize


  ;; f(N)
  oploterror,fnstrct.locn[gdfn],fnstrct.fn[gdfn],fnstrct.siglocn[gdfn,0],$
             fnstrct.sigfn[gdfn,0],$
             errcolor=clr.black,psym=4,/lobar,color=clr.black,symsize=psize,$
             thick=lthick
  oploterror,fnstrct.locn[gdfn],fnstrct.fn[gdfn],fnstrct.siglocn[gdfn,1],$
             fnstrct.sigfn[gdfn,1],$
             errcolor=clr.black,psym=4,/hibar,color=clr.black,symsize=psize,$
             thick=lthick
  if nbdfn ne 0 then begin
     plotsym,1,4,thick=2.5,color=clr.black ; down arrow, now psym=8
     oplot,fnstrct.locn[bdfn],fnstrct.fn[bdfn],$
           color=clr.black,psym=8 ; arrow
     oploterror,fnstrct.locn[bdfn],fnstrct.fn[bdfn],fnstrct.siglocn[bdfn,0],$
                replicate(0.,nbdfn),/lobar,errcolor=clr.black,$
                color=clr.black,psym=4,symsize=psize,thick=lthick ; point
     oploterror,fnstrct.locn[bdfn],fnstrct.fn[bdfn],fnstrct.siglocn[bdfn,1],$
                replicate(0.,nbdfn),/hibar,errcolor=clr.black,$
                color=clr.black,psym=4,symsize=psize,thick=lthick ; point
  endif 

  ;; Fit
  if keyword_set(fit_fil) then begin
     if size(fit_fil,/type) eq 7 then $
        fitstrct = xmrdfits(fit_fil,1,/silent) $
     else fitstrct = fit_fil

     ;; Linear space first
     xfit = 10.^xrng[0] + dindgen(1000L) * (10.^xrng[1]-10.^xrng[0])/1000L
     yfit = fitstrct.coeff * (xfit/fitstrct.datanorm)^fitstrct.alpha

     if keyword_set(log) then begin
        oplot,alog10(xfit),alog10(yfit),$
              linestyle=0,color=clr.red,thick=lthick 
        oplot,alog10([fitstrct.intlim[1],fitstrct.intlim[1]]), yrng,$
              linestyle=1,color=clr.black,thick=lthick
     endif else begin
        oplot,alog10(xfit),yfit,linestyle=0,color=clr.red,thick=lthick
        oplot,[fitstrct.intlim[1],fitstrct.intlim[1]], yrng,$
               linestyle=1,color=clr.black,thick=lthick
     endelse                    ; log or not

     ;; Legend (upper right)
     if keyword_set(label) then begin
        dx = xrng[1] - xrng[0]
        dy = yrng[1] - yrng[0]
        xyouts, 0.8*dx + xrng[0], 0.95*dy + yrng[0], $
                '!9a!X = '+strtrim(string(fitstrct.alpha,format='(f5.2)'),2),$
                charsize=csize, color=clr.black
        xyouts, 0.8*dx + xrng[0], 0.9*dy + yrng[0], $
                'k = '+strtrim(string(fitstrct.coeff,$
                                      format='(e9.2)'),2),$
                charsize=csize, color=clr.black
        xyouts, 0.8*dx + xrng[0], 0.85*dy + yrng[0], $
                'P!DKS!N = '+strtrim(string(fitstrct.prob_ks,$
                                            format='(f5.3)'),2),$
                charsize=csize, color=clr.black
     endif                      ; /label
  endif                         ; fit_fil=

  x_psclose
  print,'civ_fn_plot: created ',psfil

end                             ; civ_fn_plot


pro civ_fn_plotcdf,psfil,strct_fil,sens_fil, siiv=siiv, few=few, $
                   csize=csize,psize=psize,lthick=lthick,$
                   xrng=xrng,yrng=yrng, fit_fil=fit_fil, label=label, $
                   _extra=extra
  if (N_params() lt 3) then begin
     print,'Syntax - '+$
           'civ_fn_plot,psfil,strct_fil,sens_fil,few=,csize=,lthick=,'+$
           '          xrng=,yrng=, fit_fil=, /label'
     return
  endif

  if keyword_set(siiv) then dblt_name = 'SiIV' $
  else dblt_name = 'CIV'

  if size(sens_fil,/type) eq 7 then sens = xmrdfits(sens_fil,1,/silent) $
  else sens = sens_fil  
  civ_group,strct,strct_fil,nciv=nciv,_extra=extra ;flg_sys=,rating=,zlim=

  if keyword_set(few) then begin
     ncolm = alog10(strct.ew[0])
     signcolm = strct.sigew[0]/abs(alog(10.)*strct.ew[0])
     xpath = civ_sensitivity_x(sens,ew=10.^ncolm) ; 95%, interpolate
     if keyword_set(siiv) then begin
        ytitle = 'f(W!Dr,1393!N) CDF'
        xtitle = 'log W!Dr,1393!N (m'+STRING("305B) +')'
     endif else begin
        ytitle = 'f(W!Dr,1548!N) CDF'
        xtitle = 'log W!Dr,1548!N (m'+STRING("305B) +')'
     endelse 
  endif else begin
     ;; Error-weighted column density
     ncolm = civ_calcewn_ndblt(strct,dblt_name,signcolm=signcolm,/log,/silent)
     xpath = civ_sensitivity_x(sens,ncolm=ncolm) ; 95%, interpolate
     if keyword_set(siiv) then begin
        ytitle = 'f(N(Si!E+3!N)) CDF'
        xtitle = 'log N(Si!E+3!N)'     
     endif else begin
        ytitle = 'f(N(C!E+3!N)) CDF'
        xtitle = 'log N(C!E+3!N)'     
     endelse 
  endelse 
  srt = sort(ncolm)             ; order
  ncolm = ncolm[srt]
  strct = strct[srt]
  xpath = xpath[srt]
  nmin = min(ncolm,max=nmax)
  
  ;; CDF
  cdf = total(1./xpath,/cumulative)/total(1./xpath)

  ;; Params
  if not keyword_set(csize) then csize = 1.5
  if not keyword_set(psize) then psize = 2.
  if not keyword_set(lthick) then lthick = 2
  if not keyword_set(xrng) then $
     xrng = [ncolm[0]-signcolm[0]-0.1,$
             ncolm[nciv-1]+signcolm[nciv-1]+0.1]
  if not keyword_set(yrng) then yrng = [0.,1.01] 

  ;; Plot
  x_psopen,psfil,/maxs
  !p.multi = [1,1,1]
  !x.margin = [8,3]
  !y.margin = [4,2]

  clr = getcolor(/load)

  plot,xrng,yrng,/nodata,/ystyle,/xstyle,background=clr.white,color=clr.black,$
       ytitle=ytitle,xtitle=xtitle,charsize=csize

  ;; CDF
  oplot,[ncolm[0],ncolm],[0,cdf],color=clr.black,psym=10.,thick=lthick

  ;; Fit (normalize to intlim[0] to infinity)
  if keyword_set(fit_fil) then begin
     if size(fit_fil,/type) eq 7 then $
        fitstrct = xmrdfits(fit_fil,1,/silent) $
     else fitstrct = fit_fil

     ;; Linear space first
     xfit = 10^xrng[0] + dindgen(1000L) * (10^xrng[1]-10^xrng[0])/1000L
     yfit = (xfit^(1.+fitstrct.alpha) - $
             fitstrct.datalim[0]^(1.+fitstrct.alpha))/$
            (fitstrct.datalim[1]^(1.+fitstrct.alpha) - $
             fitstrct.datalim[0]^(1.+fitstrct.alpha) )
     oplot,alog10(xfit),yfit, linestyle=0,color=clr.red,thick=lthick 
     ;; Itegration limit
     oplot,alog10([fitstrct.intlim[1],fitstrct.intlim[1]]), yrng,$
           linestyle=1,color=clr.black,thick=lthick

     ;; Legend (upper left)
     if keyword_set(label) then begin
        dx = xrng[1] - xrng[0]
        dy = yrng[1] - yrng[0]
        xyouts, 0.05*dx + xrng[0], 0.95*dy + yrng[0], $
                '!9a!X = '+strtrim(string(fitstrct.alpha,format='(f5.2)'),2),$
                charsize=csize, color=clr.black
        xyouts, 0.05*dx + xrng[0], 0.9*dy + yrng[0], $
                'P!DKS!N = '+strtrim(string(fitstrct.prob_ks,$
                                            format='(f5.3)'),2),$
                charsize=csize, color=clr.black
     endif                      ; /label
  endif                         ; fit_fil=

  x_psclose
  print,'civ_fn_plotcdf: created ',psfil

end                             ; civ_fn_plotcdf



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro civ_fn, strct_fil, sens_fil, siiv=siiv, $
            few=few, psfil=psfil, binsize=binsize, hdf5_fil=hdf5_fil, $
            nlim=nlim, ostrct_fil=ostrct_fil,_extra=extra

  if not keyword_set(binsize) then binsize = 0.2
  if not keyword_set(nlim) then begin
     if keyword_set(few) then nlim = [alog10(20.),alog10(3.e4)] $ ; log(mA)
     else nlim = [13.,19.]                                        ; 14.3
  endif 

  if keyword_set(siiv) then dblt_name = 'SiIV' $
  else dblt_name = 'CIV'

  ;; Trim strct based on both features being 3 sigma
  civ_group,strct,strct_fil,flg_sys=384,nciv=nstrct,$
            _extra=extra        ; zlim=,rating=,/unsat
  if nstrct eq 0 then stop,'civ_fn: no '+dblt_name+$
                           ' with both lines detected at 3 sigma'

  ;; g(z) (unblocked path = sens.zpath; convert to dX)
  if keyword_set(sens_fil) then begin
     if size(sens_fil,/type) eq 7 then sens = xmrdfits(sens_fil,1,/silent) $
     else sens = sens_fil
  endif else begin
     if keyword_set(hdf5_fil) then begin
        if size(hdf5_fil,/type) eq 7 then $ 
           hdf5_sensitivity,hdf5_fil,dblt_name,sens,_extra=extra $ ; cosmology=, zlim=
        else hdf5_sensitivity,strct[0].instr_fil,dblt_name,sens,_extra=extra 
     endif else stop,'civ_fn: sens_fil not set'
;     print,max(sens.cumx_ncolm[*,1])
;     stop
  endelse 

  ;; Determine index
  civ = where(stregex(strct[0].ion,dblt_name,/boolean),nciv)
  if nciv ne 2 then stop,'civ_fn: not a doublet'
  civ = civ[sort(strct[0].wrest[civ])]

  if keyword_set(few) then begin
     ncolm = alog10(strct.ew[civ[0]])
  endif else begin
     ;; Error-weighted average of AODM column density
     ncolm = civ_calcewn_ndblt(strct,dblt_name,flg_colm=flg_colm,/log,/silent)
  endelse    
  histn = histogram(ncolm,loc=locn,binsize=binsize,min=nlim[0])
  locn = locn + 0.5*binsize
  nbin = n_elements(histn)
  siglocn = fltarr(nbin,2)
  siglocn[*,0] = 0.5*binsize
  siglocn[*,1] = 0.5*binsize

  ;; Saturated measurement bin
  gd = where(locn lt nlim[1],ngd,complement=bd,ncomplement=nbd)
  if nbd ne 0 then begin
     ;; Re-sample histogram
     tmp = histn[0:ngd]
     tmp[ngd] = total(histn[bd])
     histn = tmp
     
     ;; Reset bin locations 
     cent = mean(locn[bd])
     tmp = fltarr(ngd+1,2)
     tmp[*,0] = siglocn[0:ngd,0]
     tmp[*,1] = siglocn[0:ngd,1]
     tmp[ngd,1] = locn[nbin-1]+siglocn[nbin-1,1] - cent
     tmp[ngd,0] = cent - (locn[bd[0]]-siglocn[bd[0],0])
     siglocn = tmp
     tmp = locn[0:ngd]
     tmp[ngd] = cent
     locn = tmp

     ;; New bin number
     nbin = ngd + 1
  endif
  locnhi = locn + siglocn[*,1]
  locnlo = locn - siglocn[*,0]

  ;; Instantiate structures
  fn = fltarr(nbin)
  sigfn = fltarr(nbin,2)
  sigdX = dblarr(nbin,2)
  sigdN = fltarr(nbin,2)        ; dN = histn
  if keyword_set(few) then begin
     dX = civ_sensitivity_x(sens,sigx=sigdX,ew=10.^locn,$
                            sigew=siglocn[*,0]*alog(10.)*10.^locn) ; 95% limit, centered
  endif else begin
     dX = civ_sensitivity_x(sens,sigx=sigdX,ncolm=locn,$
                            sign=siglocn[*,0]) ; 95% limit, centered
  endelse 

  for ii=0,nbin-1 do begin
     ;; f(N)
     denom = (10^locnhi[ii]-10^locnlo[ii])*dX[ii]
     fn[ii] = histn[ii]/denom
     p = x_poisscl(histn[ii],sigma=1) 
     ;; Should round number?
     sigdN[ii,0] = histn[ii]-p[1] ; low
     sigdN[ii,1] = p[0]-histn[ii] ; hi
     ;; Errors add in quadrature .r 
     sigfn[ii,0] = sqrt((sigdN[ii,0]/denom)^2 + $
                        (sigdX[ii,1]*fn[ii]/dX[ii])^2)
     sigfn[ii,1] = sqrt((sigdN[ii,1]/denom)^2 + $
                        (sigdX[ii,0]*fn[ii]/dX[ii])^2)
  endfor                        ; loop bins

  fnstrct = {fn:fn, sigfn:sigfn, locn:locn, siglocn:siglocn, $
             dX:dX, sigdX:sigdX, dN:histn, sigdN:sigdN}

  if keyword_set(ostrct_fil) then begin
     if size(ostrct_fil,/type) eq 7 then mwrfits,fnstrct,ostrct_fil,/create $
     else ostrct_fil = fnstrct 
  endif else ostrct_fil = fnstrct

  if keyword_set(psfil) then begin
     civ_fn_plot,psfil,siiv=siiv,fnstrct_fil=fnstrct,_extra=extra ; fit_fil=, /label
  endif                                                           ; psfil=

end
