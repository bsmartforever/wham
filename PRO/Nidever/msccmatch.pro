pro msccmatch,input,usno=usno, faintobj=faintobj,$
          brightobj=brightobj, pos_error=pos_error, mu_error=mu_error,$
          mu_value=mu_value, order=order, reject=reject, maxiter=maxiter,$
          update=update,niter=niter,rms=rms,silent=silent,stp=stp

;+
;
; MSCCMATCH
; 
; PURPOSE:
;  This runs IRAF's msccmatch *non-interactively* on image(s).
;  This is almost identical to the gridred usnob1ctio package.
;
; INPUTS:
;  input       The image filename.  This can be a list.
;  =usno       The USNO-B1 structure of stars.  This is normally
;               obtained by the program with QUERYVIZIER.PRO.
;  =faintobj   Faint magnitude limit for stars to use (default: faintobj=20).
;  =brightobj  Bright magnitude limit for stars to use (default: brightobj=14).
;  =pos_error  Maximum position error (mas) (default: pos_error=100).
;  =mu_error   Maximum proper motion error error (mas/yr) (default: mu_error=100).
;  =mu_value   Maximum proper motion (mas/yr) (default: mu_value=100).
;  =order      Order of astrometric fit (default: order=5).
;  =reject     Sigma rejection for astrometric fit (default: reject=2).
;  =maxiter    Maximum number of rejection iterations (default: maxiter=2).
;  /update     Update the WCS in the FITS header.  The default is to update.
;  =niter      The number of times to run MSCCMATCH (default niter=1)
;  /silent     Don't print out anything
;  /stp        Stop at the end of the program
;
; OUTPUTS:
;  The WCS in the FITS is updated
;  =usno       The USNO-B1 structure of stars.
;  =rms        The final rms in arcsec.
;
; USAGE:
;  IDL>msccmatch,'obj1061.fits'
;
; By D. Nidever    October 2007 (translated from usnob1ctio)
;-

; Important directories
;irafdir = '/net/home/dln5q/iraf/'
irafdir = file_search('~/iraf/',/fully_qualify_path)
CD,current=curdir

; Loading the input
LOADINPUT,input,list
nlist = n_elements(list)

; Not enough inputs
if n_elements(nlist) eq 0 then begin
  print,'Syntax - msccmatch,filename,usno=usno, faintobj=faintobj,'
  print,'          brightobj=brightobj, pos_error=pos_error, mu_error=mu_error,'
  print,'          mu_value=mu_value, order=order, reject=reject, maxiter=maxiter,'
  print,'          update=update,niter=niter,silent=silent,stp=stp'
  return
endif


; Multiple files input
if (nlist gt 1) then begin

  ; Looping through the files 
  for i=0,nlist-1L do begin

    if not keyword_set(silent) then begin
      if i gt 0 then print,''
      print,'-----------------------------------------'
      print,strtrim(i+1,2),'/',strtrim(nlist,2),'  RUNNING MSCCMATCH ON '+list[i]
      print,'-----------------------------------------'
      print,''
    endif

    ; Running msccmatch
    MSCCMATCH,list[i],faintobj=faintobj,$
          brightobj=brightobj, pos_error=pos_error, mu_error=mu_error,$
          mu_value=mu_value, order=order, reject=reject, maxiter=maxiter,$
          update=update,niter=niter,silent=silent,stp=stp
  end

  return

endif


; Single files
image = list[0]


; Default values
if n_elements(faintobj) eq 0 then faintobj = 20.0
if n_elements(brightobj) eq 0 then brightobj = 14.0
if n_elements(pos_error) eq 0 then pos_error = 100.0
if n_elements(mu_error) eq 0 then mu_error = 100.0
if n_elements(mu_value) eq 0 then mu_value = 100.0
if n_elements(order) eq 0 then order = 5
if n_elements(reject) eq 0 then reject = 2.0
if n_elements(maxiter) eq 0 then maxiter = 2
if n_elements(update) eq 0 then update = 1
if (long(update) eq 0) then supdate='no' else supdate='yes'
if n_elements(niter) eq 0 then niter=1
niter = (niter > 1) < 10      ; can't do an unreasonable number of iterations


; Filename
base = file_basename(image,'.fits')
file = base+'.fits'
test = file_test(file)

if (test eq 0) then begin
  print,file,' DOES NOT EXIST'
  return
endif

if not keyword_set(silent) then print,'Running MSCCMATCH on ',file


; Getting USNO-B1 catalog for this frame
if n_elements(usno) eq 0 then begin

  ; Getting central coordinates from the header
  FITS_READ,file,im,head,exten=1
  cen_ra = sxpar(head,'RA')
  cen_dec = sxpar(head,'DEC')
  cen_ra2 = sexig2ten(cen_ra)*15.0  ; degrees
  cen_dec2 = sexig2ten(cen_dec)

  ; Querying the USNO-B1 catalog
  if not keyword_set(silent) then print,'Querying USNO-B1 catalog'
  dis = 40.  ; 
  usno = QUERYVIZIER('USNO-B1',[cen_ra2,cen_dec2],[dis,dis],/canada,/allcolumns)

endif


; Getting average Rmag, Bmag 
rmag = median([[usno.r1mag],[usno.r2mag]],dim=2,/even)
bmag = median([[usno.b1mag],[usno.b2mag]],dim=2,/even)
rmagbd = where(finite(rmag) eq 0,nrmagbd)      ; replace NaNs with 99.9999
if nrmagbd gt 0 then rmag[rmagbd] = 99.9999
bmagbd = where(finite(bmag) eq 0,nbmagbd)
if nbmagbd gt 0 then bmag[bmagbd] = 99.9999

perror = sqrt(usno.e_radeg^2.0 + usno.e_dedeg^2.0)
pm = sqrt(usno.pmra^2.0 and usno.pmde^2.0)
pmerr = sqrt(usno.e_pmra^2.0 and usno.e_pmde^2.0)

; Making the cuts on the USNO-B1 catalog
gd = WHERE(rmag lt faintobj and rmag gt brightobj and $
           perror lt pos_error and pm lt mu_value and $
           pmerr lt mu_error,ngd)


if ngd gt 0 then begin
  if not keyword_set(silent) then $
    print,strtrim(ngd,2),' candidate stars found'

; No good stars found
endif else begin
  print,'No stars fitting the criteria found'
  return
endelse

; The good stars
usno2 = usno[gd]

; Printing star coordinates to ASCII file
star_ra = usno2.raj2000/15.0      ; hours
star_dec = usno2.dej2000          ; degrees
catfile = maketemp('temp','.txt')
WRITECOL,catfile,star_ra,star_dec,fmt='(F12.8,F13.7)'



; From gridred usnob1ctio
;
;real    faintobj   = 20.0 {prompt="Faint magnitude limit for Object frames"}
;real    brightobj  = 14.0 {prompt="Bright magnitude limit for Object frames"}
;real    pos_error  = 100.0  {prompt="Maximum positional error (milliarcseconds"}
;real    mu_error   = 100.0  {prompt="Maximum proper motion error (mas/yr)"}
;real    mu_value   = 100.0  {prompt="Maximum proper motion (mas/yr)"}
;int     order1     = 5 {prompt="Order of astrometric fit"}
;real    reject1    = 1.0 {prompt="Sigma rejection for astrometric fit to USNO-B1"}
;int     maxiter1   = 2 {prompt="Maximum number of rejection iterations"}
;
;# Set values for the fitting done within msctpeak: ORDER, REJECT, MAXITER
;        unlearn tpltsol
;        unlearn ccmap
;        unlearn cctran
;        tpltsol.reject=rej
;        tpltsol.xxorder=ord
;        tpltsol.xyorder=ord
;        tpltsol.yxorder=ord
;        tpltsol.yyorder=ord
;        ccmap.xxorder=ord
;        ccmap.xyorder=ord
;        ccmap.yxorder=ord
;        ccmap.yyorder=ord
;    ccmap.maxiter=maxit
;
;
;msccmatch (img,
;img//".finalcat", yes, outcoords="", usebpm=yes, verbose=yes, nsearch=50,
;search=60., rsearch=4., cbox=11, maxshift=15., csig=1., cfrac=0.5,
;listcoords=yes, nfit=100, rms=1., fitgeometry="general", reject=2.5,
;update=yes, interactive=yes, fit=yes, graphics="stdgraph", cursor="")

; Input strings
srej = strtrim(reject,2)
sorder = strtrim(order,2)
smaxiter = strtrim(maxiter,2)
simage = strtrim(image[0],2)


; Write IRAF script
push,cmd,'cd '+curdir
;push,cmd,'gridred'
push,cmd,'mscred'
push,cmd,'unlearn tpltsol'
push,cmd,'unlearn tpltsol'
push,cmd,'unlearn ccmap'
push,cmd,'unlearn cctran'
push,cmd,'tpltsol.reject='+srej
push,cmd,'tpltsol.xxorder='+sorder
push,cmd,'tpltsol.xyorder='+sorder
push,cmd,'tpltsol.yxorder='+sorder
push,cmd,'tpltsol.yyorder='+sorder
push,cmd,'ccmap.xxorder='+sorder
push,cmd,'ccmap.xyorder='+sorder
push,cmd,'ccmap.yxorder='+sorder
push,cmd,'ccmap.yyorder='+sorder
push,cmd,'ccmap.maxiter='+smaxiter
push,cmd,'msccmatch ("'+simage+'","'+catfile+'", yes, outcoords="", usebpm=yes, verbose=yes,'+$
         'nsearch=50, search=60., rsearch=4., cbox=11, maxshift=15., csig=1., cfrac=0.5,'+$
         'listcoords=no, nfit=100, rms=1., fitgeometry="general", reject=2.5,'+$
         'update='+supdate+', interactive=no, fit=no, graphics="stdgraph", cursor="")'
push,cmd,'logout'
cmdfile = maketemp('temp','.cl')
WRITELINE,cmdfile,cmd

; Goto the IRAF directory
CD,current=curdir
CD,irafdir

; Starting the iterations
for i=0,niter-1 do begin

  if (niter gt 1) and not keyword_set(silent) then begin
    print,''
    print,'# Iteration ',strtrim(long(i+1),2),'/',strtrim(long(niter),2)
  endif

  ; Running IRAF
  undefine,out
  SPAWN,'cl < '+curdir+'/'+cmdfile,out,errout

  ; The output
  lo = first_el(where(stregex(out,'MSCCMATCH:',/boolean) eq 1))  ; where to start output
  lo = lo + 1
  hi = n_elements(out)-2
  out2 = out[lo:hi]
  if not keyword_set(silent) then print,''
  if not keyword_set(silent) then printline,out2

end

; Return to original directory
CD,curdir

; Erasing the temporary files
FILE_DELETE,catfile,cmdfile,/allow,/quiet

; Final RMS
g = where(stregex(out2,'rms',/boolean) eq 1)
srms = out2[g[0]]
p1 = strpos(srms,'(')
p2 = strpos(srms,', ')
p3 = strpos(srms,')')
sra_rms = strmid(srms,p1+1,p2-p1-1)
sdec_rms = strmid(srms,p2+2,p3-p2-2)
ra_rms = float(sra_rms)
dec_rms = float(sdec_rms)
rms = sqrt(ra_rms^2.0 + dec_rms^2.0)
if not keyword_set(silent) then print,''
if not keyword_set(silent) then print,'Final RMS = ',string(rms,format='(F5.3)'),' arcsec'


if keyword_set(stp) then stop

end
