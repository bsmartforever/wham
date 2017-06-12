;; COS_INSPECT.pro
;;
;; Routine for generating a quick-look summary of a set of COS
;; x1d.fits files.  
;; 
;; INPUT: path - path to data
;;        filelist - list of files, if not supplied, routine will look
;;                   for *x1d.pro
;;
;; OPTIONS: 
;;        xrange, yrange - plotting ranges (optional)
;;        pages - (default=1) number of pages to split plot into
;;
;; REQUIRES: 
;;        unique - apl package
;;
;; CDanforth 10/2/09

pro cos_inspect,path=path,filelist=filelist,xrange=xrange,yrange=yrange,pages=pages

!p.multi=0
!x.style=1

if not keyword_set(xrange) then xrange=[1130,1800]
if not keyword_set(path) then path=''
if not keyword_set(filelist) then filelist=findfile(path+'*x1d.fit*') else filelist=path+filelist

nfiles=n_elements(filelist)
if not keyword_set(pages) then pages=1.

!p.multi=[0,1,ceil(nfiles/pages)]

grating=strarr(nfiles)
cenwave=intarr(nfiles)
exptime=fltarr(nfiles)
dateobs=strarr(nfiles)

for i=0,nfiles-1 do begin
;; read header parameters
    im=mrdfits(filelist[i],0,hdr0,/silent)
    grating[i]=strtrim(sxpar(hdr0,'OPT_ELEM'),2)
    cenwave[i]=sxpar(hdr0,'CENWAVE')
    im=mrdfits(filelist[i],1,hdr1,/silent)
    exptmp=im.exptime
    exptime[i]=exptmp[0]
    dateobs[i]=sxpar(hdr1,'DATE-OBS')

;; plot wave,flux,err
    wavetmp=im.wavelength
    fluxtmp=im.flux
    errtmp=im.error

;; update flux calibration (for old data, should be obsolete now)
;    if strtrim(grating[i],2) ne 'G140L' then begin
;      fluxtmp[*,0]=cos_sens_update(wavetmp[*,0],fluxtmp[*,0],seg='FUVA',grat=grating[i],/lv)
;      fluxtmp[*,1]=cos_sens_update(wavetmp[*,1],fluxtmp[*,1],seg='FUVB',grat=grating[i],/lv)
;      errtmp[*,0]=cos_sens_update(wavetmp[*,0],errtmp[*,0],seg='FUVA',grat=grating[i],/lv)
;      errtmp[*,1]=cos_sens_update(wavetmp[*,1],errtmp[*,1],seg='FUVB',grat=grating[i],/lv)
;    endif

    if not keyword_set(yrange) then yrange=[-.01,1]*1e-13 ;begin 

    plot,wavetmp[*,0],fluxtmp[*,0],yr=yrange,/ysty,nsum=6,xr=xrange,tit=strtrim(sxpar(hdr0,'TARGNAME'),2)+'  '+strtrim(sxpar(hdr0,'ROOTNAME'),2)+' '+dateobs[i]+'  '+grating[i]+string(cenwave[i],exptime[i],'(I5,I6)')+' sec '
    oplot,wavetmp[*,0],errtmp[*,0],nsum=6,color=128
    oplot,wavetmp[*,1],fluxtmp[*,1],nsum=6
    oplot,wavetmp[*,1],errtmp[*,1],nsum=6,color=128

endfor 

;; print summary information
  print,'------------------------------------------------------------'
  print,strtrim(sxpar(hdr0,'TARGNAME'),2)+' Observation Summary'
  g130m=where(grating eq 'G130M') & g160m=where(grating eq 'G160M') & g140l=where(grating eq 'G140L')
  if g130m ne [-1] then begin
    print,'  G130M: total exptime =  '+strtrim(total(exptime[g130m]),2)
    print,'         # exposures          '+strtrim(n_elements(g130m),2)
    print,'         # grating positions  '+strtrim(n_elements(unique(cenwave[g130m])),2)
  endif else print,'  G130M: NONE '
  if g160m ne [-1] then begin
    print,'  G160M: total exptime =  '+strtrim(total(exptime[g160m]),2)
    print,'         # exposures          '+strtrim(n_elements(g160m),2)
    print,'         # grating positions  '+strtrim(n_elements(unique(cenwave[g160m])),2)
  endif else print,'  G160M: NONE '
  if g140l ne [-1] then begin
    print,'  G140L: total exptime =  '+strtrim(total(exptime[g140l]),2)
    print,'         # exposures          '+strtrim(n_elements(g140l),2)
    print,'         # grating positions  '+strtrim(n_elements(unique(cenwave[g140l])),2)
  endif else print,'  G140L: NONE '
  

  print,'------------------------------------------------------------'

!p.multi=0

end
