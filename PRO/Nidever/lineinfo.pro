;+
; NAME
;   lineinfo
;
; PURPOSE
;   This program returns information on the lines in a spectrum.
;
; INPUTS:
;   wave        The wavelength array
;   spec        The spectrum array
;   /noplot     Don't plot anything
;   =pause      Amount to pause 
;   /noprint    Don't print anything
;   /nothresh   No threshold 
;
; OUTPUTS:
;   str         Output structure.  Contains w_core,s_core,fwhm,w
;
; PROCEDURES USED:
;   maxmin.pro
;   sm_spec.pro
;
; Written by D.Nidever 2001?
;-


pro sm_spec,wave,spec,spec2,usamp

n=long(n_elements(wave))
usamp=float(round(usamp))

twave=dblarr(n/usamp)
tspec=dblarr(n/usamp)

for i=0,(n/usamp)-1 do begin

  twave(i)=mean(wave(i*usamp:i*usamp+usamp-1))
  tspec(i)=mean(spec(i*usamp:i*usamp+usamp-1))

end

;ind=where(wave ge min(twave) and wave le max(twave))
;spec2=spline(twave,tspec,wave(ind))
spec2=spline(twave,tspec,wave)

;stop

end


;---------------------------------------------------------------------

pro lineinfo,wave,spec,str,noplot=noplot,pause=pause,noprint=noprint,$
             nothresh=nothresh

if not keyword_set(pause) then pause=0

!p.multi=[0,1,2]

;checks the lines in a chunk of spectrum

    ;getting the lines
    maxmin,spec,minarr,maxarr

    ;no lines
    if maxarr(0) eq -1 or minarr(0) eq -1 then begin
      nlin=0
      dum={w_core:0.d,s_core:0.d,fwhm:0.,w:0.}
      str=replicate(dum,1)
      goto,END_BOMB
    end

    nlin=n_elements(minarr)
    npix=n_elements(spec)

    ;checking the density of lines
    dum=nlin/(max(wave)-min(wave))
    str_lrho=strtrim(float(dum),2)

    ;line statistics
    dum=spec(maxarr)-spec(minarr)
    f=histogram(dum,bin=0.01)
    std=SQRT(mean(f/total(f)))

    ;IF THERE ARE TOO MANY SMALL LINES THEN SMOOTH IT
    ;smoothing the data
    if str_lrho gt 8. then begin
      smpix = 10
      if str_lrho gt 10. then smpix=15
      old_spec2=spec
      sm_spec,wave,spec,spec2,smpix
      spec=spec2

      ;getting the lines
      maxmin,spec,minarr,maxarr
      nlin=n_elements(minarr)
      npix=n_elements(spec)
    end

    ;no lines
    if maxarr(0) eq -1 or minarr(0) eq -1 then begin
      nlin=0
      dum={w_core:0.d,s_core:0.d,fwhm:0.,w:0.}
      str=replicate(dum,1)
      goto,END_BOMB
    end

    ;getting the good lines
    if not keyword_set(nothresh) then begin
      old_minarr=minarr
      threshold=median(spec)-stdev(spec)
      gd_ind=where(spec(minarr) lt threshold,ngd)
      if ngd gt 0 then gd_minarr=minarr(gd_ind)
      if ngd eq 0 then begin
	str={w_core:-1.d,s_core:-1.d,fwhm:-1.,w:-1.}
        goto, END_BOMB
      endif
    endif else begin
      ngd=nlin
      gd_minarr=minarr
    endelse

    ;making the structure
    dum={w_core:0.d,s_core:0.d,fwhm:0.,w:0.}
    str=replicate(dum,ngd)

  ;stop

    nline=0
   
    if not keyword_set(noprint) then begin
      print
      print,'    LINE  W_CORE   S_CORE    FWHM       W'
      print,'   -------------------------------------------'
    end

    ;going through the lines
    FOR i=0.,ngd-1 do begin

      ;re-initializing variables
      wcore=0 & score=0 & w=0 & fwhm=0
      maxi1=-1 & mini=-1 & maxi2=-1

      mini=gd_minarr(i)
      dum1=where(maxarr lt mini,ndum1)
      dum2=where(maxarr gt mini,ndum2)	
     ; if ndum1 eq 0 or ndum2 eq 0 then goto,BOMB
      if ndum1 eq 0 then maxi1 = 0      else maxi1=maxarr(first_el(dum1,/last))
      if ndum2 eq 0 then maxi2 = npix-1 else maxi2=maxarr(first_el(dum2))
     ; if ndum1 gt 0 and ndum2 gt 0 then begin
     ;   maxi1=maxarr(first_el(dum1,/last))
     ;   maxi2=maxarr(first_el(dum2))
     ; endif

      ;selecting the best lines
    ;  if maxi1 eq 0 then goto,BOMB
    ;  if maxi2 eq npix-1 then goto,BOMB
    ;  if spec(mini) gt 0.8 then goto,BOMB
    ;  if spec(maxi1) lt 0.95 or spec(maxi2) lt 0.95 then goto,BOMB

      ;if mini eq -1 or maxi eq -1 then goto,BOMB
      ;print,maxarr(mini),j,maxarr(maxi)

      wav=wave(maxi1:maxi2)
      chunk=spec(maxi1:maxi2)

      osamp=4
      upix=n_elements(chunk)
      nwav=dindgen(osamp*(upix-1)+1)/(osamp*(upix-1)+1)*(max(wav)-min(wav))+min(wav)
      nchunk=spline(wav,chunk,nwav)
      upix2=n_elements(nchunk)

      disp=nwav(1)-nwav(0)
      gd=where(nchunk lt 1.0d,ng)
      if ng eq 0 then begin
        ochunk=nchunk
        nchunk=nchunk/max(nchunk)
        gd=where(nchunk lt 1.0d,ng)
      end
      w=total(1-nchunk(gd))*disp

      ;plot,nwav,nchunk,co=300,ps=-8
      ;oplot,nwav,nchunk*0+1,co=300

      corei=first_el(minloc(nchunk))
      score=nchunk(corei)
      wcore=nwav(corei)
      hlfcore=score+(1-score)/2.

      lft_hlf=first_el(minloc(abs(nchunk(0:corei)-hlfcore)))
      rt_hlf=first_el(minloc(abs(nchunk(corei:upix2-1)-hlfcore)))+corei

      ;oplot,nwav,nchunk*0+hlfcore,co=800
      ;oplot,[nwav(lft_hlf),nwav(rt_hlf)],[hlfcore,hlfcore],co=300

      fwhm=nwav(rt_hlf)-nwav(lft_hlf)

  ;stop

      ;PRINTING THE DATA TO THE SCREEN

      if not keyword_set(noprint) then begin
        floormat="(I7,F9.2,F9.4,F9.4,F9.4)"
        print,FORMAT=floormat,strtrim(nline,2),wcore,score,fwhm,w 
      end 

      ;PUTTING DATA IN THE STRUCTURE
      str(nline).w_core=wcore
      str(nline).s_core=score
      str(nline).fwhm=fwhm
      str(nline).w=w

      if not keyword_set(noplot) then begin

      ;PLOTTING
      co1=1500
      co2=300

      ;wset,1
      plot,wave,spec,co=co1
      oplot,wave(maxi1:maxi2),spec(maxi1:maxi2),co=co2

      ;wset,0
      xtit='Wavelength (angstroms)'
      ytit='Normalized Flux'
      tit='Line '+strtrim(nline,2)     ;+' for '+name+' ('+str.sptype+str.spclass+') with lrho '+str_lrho
      plot,nwav,nchunk*0+1,co=co1,yr=[0,1.1],xtit=xtit,ytit=ytit,tit=tit,thick=1.5
      polyfill,[wcore-w/2.,wcore+w/2.,wcore+w/2.,wcore-w/2.],[1,1,0,0],co=800
      oplot,nwav,nchunk,co=500,thick=2
      oplot,nwav,nchunk*0+1,co=co1,thick=1.5
      oplot,[nwav(lft_hlf),nwav(rt_hlf)],[hlfcore,hlfcore],co=co1,thick=1.5
      oplot,[wcore,wcore],[0,1],co=co1,thick=1.5
      oplot,[wcore-w/2.,wcore-w/2.],[0,1],co=co1,thick=1.5
      oplot,[wcore+w/2.,wcore+w/2.],[0,1],co=co1,thick=1.5

      xyouts,min(nwav),1.15,'FWHM   = '+strtrim(float(fwhm),2),co=co1
      xyouts,min(nwav),1.11,'W       = '+strtrim(float(w),2),co=co1
      xyouts,min(nwav),1.07,'W_CORE = '+strtrim(float(wcore),2),co=co1
      xyouts,min(nwav),1.03,'S_CORE = '+strtrim(float(score),2),co=co1

      end	;noplot

      ;if nline eq 29 then goto,END_BOMB		;making sure it doesn't crash
      nline=nline+1

      ;stop
      wait,pause

      BOMB:

    END		;looping through the lines

gdind = where(str.w_core ne 0.0 and str.s_core ne 0.0,ngdind)
str = str(gdind)

END_BOMB:

;stop

if not keyword_set(noprint) then print

!p.multi=0


end


