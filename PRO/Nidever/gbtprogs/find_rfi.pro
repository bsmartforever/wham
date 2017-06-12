pro find_rfi,scans,rfi

nscans = n_elements(scans)

print,'Finding RFI in ',strtrim(nscans,2),' Scans'

; Polarization loop
for i=0,1 do begin

  ; Get the integrations info
  for j=0,nscans-1 do begin

    ; basic argument checks
    argsOK=check_calib_args(scans[j],plnum=i,/quiet,/keepints,ret=ret)
    ;                        ifnum=ifnum,intnum=intnum,plnum=plnum,fdnum=fdnum,sampler=sampler, $
    ;                        eqweight=eqweight,units=units,quiet=quiet,keepints=keepints,useflag=useflag, $
    ;                        skipflag=skipflag,instance=instance,file=file,$
    ;                        timestamp=timestamp,tau=tau,ap_eff=ap_eff,ret=ret)

    ; Get the scan_info for the scan
    info = find_scan_info(scans[j],timestamp=timestamp,instance=instance,file=file)
    ; Get the requested data
    data = get_calib_data(info, ret.ifnum, ret.plnum, ret.fdnum, ret.sampler, count, $
                          intnum=intnum, useflag=useflag, skipflag=skipflag)

    ; This is how get_calib_data loads the data
    ;            data = !g.lineio->get_spectra(count,scan=info.scan,feed=thisfeed,ifnum=thisIF,$
    ;                                          pol=thispol,file=info.file,timestamp=info.timestamp,$
    ;                                          useflag=useflag,skipflag=skipflag)


    if n_elements(alldata) eq 0 then alldata=data else alldata=[alldata,data]
  endfor

  ;stop

endfor

ndata = n_elements(alldata)

;sigind = where(alldata.sig_state eq 1,nsigind)
;refind = where(alldata.sig_state eq 0,nrefind)

freq = dblarr(ndata)
for i=0,ndata-1 do freq[i]=chantofreq(alldata[i],0.0d0)
df = alldata[0].frequency_interval
if df lt 0.0 then shift=(freq-max(freq))/df else $
  shift=(freq-min(freq))/df

nchan = n_elements(*alldata[0].data_ptr)
;data = fltarr(nchan+max(round(shift)),ndata)+!values.f_nan
;for i=0,ndata-1 do data[0,i]=*alldata[i].data_ptr
;
p0ind = where(alldata.polarization_num eq 0,np0ind)
p1ind = where(alldata.polarization_num eq 1,np1ind)

;data = fltarr(nchan+max(round(shift)),ndata)+!values.f_nan
diff = fltarr(nchan+max(round(shift)),ndata)+!values.f_nan
for i=0,ndata-1 do begin
  ;if i mod 500 eq 0 then print,i
  sh = round(shift[i])
  spec = *alldata[i].data_ptr
  cont = median(spec,10,/even)
  cont[0:5]=median(spec[0:5])
  cont[nchan-6:nchan-1]=median(spec[nchan-6:nchan-1])
  sig = mad(spec-cont)
  snr = median(spec)/sig
  scl = (median(spec)/sig)^2 / median(spec)  ; scale to real counts
  sigarr = sqrt((spec>0)/scl) > 1e-5  ; error array
  smsigarr = median(sigarr,4,/even)  ; smooth a bit

  ; residual in terms of sigma
  diff[sh:sh+nchan-1,i] = (spec-cont)/smsigarr
  ;data2[sh:sh+nchan-1,i] = *alldata[i].data_ptr
endfor

; Find RFI for each polarization separately
;  it varies with polarization
f = dindgen(nchan)*df+max(freq)

tot0 = total(diff[*,p0ind],2,/nan)/np0ind
;med0 = median(diff[*,p0ind],dim=2)
tot0med = tot0-median(tot0,3)   ; this removes structure and finds RFI peaks
;sig0 = mad(tot0med,/zero)
mask0 = (tot0med gt 1)
mask0 = convol(mask0,fltarr(5)+1)
mask0 /= mask0>1
lo0 = where(mask0 eq 1 and [0,mask0[0:nchan-2]] eq 0)
hi0 = where(mask0 eq 1 and [mask0[1:nchan-1],0] eq 0)
rfi0 = replicate({plnum:0,lo:0L,hi:0L,flo:0.0d0,fhi:0.0d0},n_elements(lo0))
rfi0.lo = lo0
rfi0.hi = hi0
rfi0.flo = f[lo0]
rfi0.fhi = f[hi0]
print,'Found ',strtrim(n_elements(rfi0),2),' RFI in plnum=0'

tot1 = total(diff[*,p1ind],2,/nan)/np1ind
;med1 = median(diff[*,p1ind],dim=2)
tot1med = tot1-median(tot1,3)   ; this removes structure and finds RFI peaks
;sig1 = mad(tot1med,/zero)
bd1 = where(tot1med gt 1,nbd1)
mask1 = (tot1med gt 1)
mask1 = convol(mask1,fltarr(5)+1)
mask1 /= mask1>1
lo1 = where(mask1 eq 1 and [0,mask1[0:nchan-2]] eq 0)
hi1 = where(mask1 eq 1 and [mask1[1:nchan-1],0] eq 0)
rfi1 = replicate({plnum:1,lo:0L,hi:0L,flo:0.0d0,fhi:0.0d0},n_elements(lo1))
rfi1.lo = lo1
rfi1.hi = hi1
rfi1.flo = f[lo1]
rfi1.fhi = f[hi1]
print,'Found ',strtrim(n_elements(rfi1),2),' RFI in plnum=1'

; Make list of frequency ranges for each polarization
rfi = [rfi0,rfi1]


;; Find RFI by looking at peaks at constant FREQUENCY!!!!!
;shift = (str[0].crval1-str.crval1)/abs(str[0].cdelt1)
;shift = shift-min(shift)   ; make 0 the minimum
;data = fltarr(n+ceil(max(shift)),nstr)+!values.f_nan
;for i=0,nstr-1 do begin
;  if i mod 500 eq 0 then print,i
;  sh = round(shift[i])
;  cont = median(str[i].data,10,/even)
;  data[sh:sh+n-1,i] = str[i].data-cont ; remove the continuum
;  ;data[sh:sh+n-1,i] = str[i].data/cont-1  ; remove the continuum
;end
;
;tot = total(data,2,/nan)
;medall = median(data,dim=2)
;;tot = tot-median(tot)
;;num = total(finite(data),2)
;;tot2 = tot/(num>1)
;;med = median(data,dim=2)
;med = median(tot,8,/even)  ; remove smooth component
;;  if you use an even binsize and /even then it will always
;;  take a mean and med will rarely have an actual value of tot
;;  which screws up the sigma calculation
;diff = tot-med
;sig0 = mad(diff,/zero)
;h = histogram(diff,binsize=sig0/10,min=-sig0*10,max=sig0*10,locations=x)
;yfit = mpfitpeak(x,h,pars,nterms=3,/gaussian,/positive,estimates=[max(h),0.0,sig0])
;sig = pars[2]
;bd = where(diff GT 10*sig,nbd)
;plot,diff,yr=[-1,1]*sig*20
;if nbd gt 0 then oplot,bd,diff[bd],ps=1,co=250

;stop

; Free pointers
data_free,alldata
data_free,data
heap_free,alldata
heap_free,data

;stop

end
