pro catallastrom,datfiles,allusno,allphot,allusno2,allphot2,alltrans,missing,stp=stp,$
                 noprint=noprint

; This loads in all the astrometry DAT files
; and contatenates them

ndatfiles = n_elements(datfiles)

; Not enough inputs
if ndatfiles eq 0 then begin
  print,'Syntax - catallastrom,datfiles,allusno,allphot,allusno2,allphot2,alltrans,missing,stp=stp'
  return
endif

; Initializing all the arrays
undefine,allusno
undefine,allphot
undefine,allusno2
undefine,allphot2
undefine,missing


;#####################################
;# Concatenating all the files
;# Check for missing ones

if not keyword_set(noprint) then begin
  print,''
  print,'RESTORING ASTROMETRY FILES'
  print,''
  print,'STATS ON INDIVIDUAL FRAMES'
endif

; Loop through the DATA files
for i=0,ndatfiles-1 do begin

  ;filebase = file_basename(photfiles[i],'.phot')
  ;dir = file_dirname(photfiles[i])
  ;dir2 = file_expand_path(dir)
  ;
  ;; Restore the DAT save file
  ;datfile = dir2+'/astrom/'+filebase+'_astrom.dat'
  datfile = datfiles[i]
  filebase = file_basename(datfile,'_astrom.dat')

  ; Test the dat file
  test = file_test(datfile)
  if test eq 0 then begin
    push,missing,i
    goto,BOMB2
  endif

  ; Restore the file
  restore,datfile


  ; Concatenate the structures
  ; These are INDIVIDUAL files
  if n_elements(usno) gt 1 then begin

    ; Add a tag to say which chip it is
    add_tag,usno,'CHIP',0,usno
    add_tag,phot,'CHIP',0,phot  

    ; Getting the chip #
    base = file_basename(datfile,'.dat')
    dum = strsplit(base,'_',/extract)
    dum1 = dum[1]
    amp = long(dum1)  ; amp
    chip = ((amp-1)/2)+1

    usno.chip = chip
    phot.chip = chip

    ; Converting coordinates to chip coords
    if i mod 2 eq 1 then phot.x = phot.x+1024L   ; correcting the X values

    push,allusno,usno
    push,allphot,phot
    if n_elements(usno2) gt 1 then begin
      push,allusno2,usno2
      push,allphot2,phot2
      push,missing,i
    endif

  ; These are CHIP files
  endif else begin

    ; Getting the chip #
    base = file_basename(datfile,'.dat')
    dum = strsplit(base,'_',/extract)
    dum1 = dum[1]
    len = strlen(dum1)
    chip = strmid(dum1,4,len-4)
    chip = long(chip)

    ; Add a tag to say which chip it is
    add_tag,chusno,'CHIP',0,chusno
    add_tag,chphot,'CHIP',0,chphot  
    add_tag,chusno3,'CHIP',0,chusno3
    add_tag,chphot3,'CHIP',0,chphot3  
    chusno.chip = chip
    chphot.chip = chip
    chusno3.chip = chip
    chphot3.chip = chip

    ; Add to large structure
    push,allusno,chusno
    push,allphot,chphot
    push,allusno2,chusno3
    push,allphot2,chphot3
    trans = chtrans
  endelse

  ; Making a 4th order transformation structure so
  ; all of the structures will be compatible
  if size(trans,/type) eq 8 then begin

    tags = tag_names(trans)

    nord = 5
    trans2 = {nord:0,nstars:0L,razero:0.0d0,deczero:0.d0,npole:dblarr(2),equator:dblarr(2),xzero:0.0d0,yzero:0.0d0,$
              rpar:dblarr(nord,nord),rsigpar:dblarr(nord,nord),$ 
              rrms:0.0d0,rchi:0.0d0,dpar:dblarr(nord,nord),dsigpar:dblarr(nord,nord),$ 
              drms:0.0d0,dchi:0.0d0,rms:0.0d0}

    trans2.nord = trans.nord
    trans2.nstars = trans.nstars
    trans2.razero = trans.razero
    trans2.deczero = trans.deczero
    g = where(tags eq 'NPOLE',ng)
    if ng gt 0 then trans2.npole = trans.npole
    g = where(tags eq 'EQUATOR',ng)
    if ng gt 0 then trans2.equator = trans.equator
    trans2.xzero = trans.xzero
    trans2.yzero = trans.yzero

    rpar = dblarr(nord,nord)     ; temporary structure to transfer rpar
    rpar[0,0] = trans.rpar
    trans2.rpar = rpar

    rsigpar = dblarr(nord,nord)  ; temporary structure to transfer rsigpar
    rsigpar[0,0] = trans.rsigpar
    trans2.rsigpar = rsigpar

    trans2.rrms = trans.rrms
    trans2.rchi = trans.rchi

    dpar = dblarr(nord,nord)     ; temporary structure to transfer dpar
    dpar[0,0] = trans.dpar
    trans2.dpar = dpar

    dsigpar = dblarr(nord,nord)  ; temporary structure to transfer dsigpar
    dsigpar[0,0] = trans.dsigpar
    trans2.dsigpar = dsigpar

    trans2.drms = trans.drms
    trans2.dchi = trans.dchi
    trans2.rms = trans.rms

    push,alltrans,trans2

    ; Print out RMS
    if not keyword_set(noprint) then begin
      form='(A-14,A-14,A-14)'
      print,format=form,filebase,'  RMS = '+stringize(trans.rms,ndec=3),' Nstars = '+strtrim(trans.nstars,2)
    endif
  endif ; valid trans structure

  BOMB2:

end
print,''

; Getting unique USNO structure
if n_elements(allusno) gt 0 then begin
  ui = uniq(allusno.usno_b1_0,sort(allusno.usno_b1_0))
  allusno_orig = allusno
  allusno = allusno[ui]
endif

if keyword_set(stp) then stop

end

;------------------------------------

pro fit_radec_global,allusno,allphot,allusno2,allphot2,allusno3,allphot3,gtrans,$
                     maxord=maxord,stp=stp

; Do a global fit to the astrometry

nallusno = n_elements(allusno)
nallphot = n_elements(allphot)
nallusno2 = n_elements(allusno2)
nallphot2 = n_elements(allphot2)

; Not enough inputs
if nallusno eq 0 or nallphot eq 0 or nallusno2 eq 0 or nallphot2 eq 0 then begin
  print,'Syntax - fit_radec_global,allusno,allphot,allusno2,allphot2,allusno3,allphot3,ftrans,'
  print,'                          maxord=maxord,stp=stp'
  return
endif

if n_elements(maxord) eq 0 then maxord=4

;####################################
; Finding a common solution

print,''
print,'FINDING GLOBAL SOLUTION'
print,''

; Fitting, NORD=2
FIT_RADEC,allusno2,allphot2,trans2,nord=2
print,'Nord=2  RMS=',stringize(trans2.rms,ndec=3),' arcsec'

rashift = median(allusno2.raj2000-allphot2.ra)
decshift = median(allusno2.dej2000-allphot2.dec)

;; Getting base name
;fbase = file_basename(photfiles[0],'.phot')
;dum = strsplit(fbase,'_',/extract)
;base = dum[0]

; Selecting the best stars
siglim = 21.0

; USNO first
blim = siglim
ugd = where(allusno.e_radeg lt 400 and allusno.e_dedeg lt 400 and abs(allusno.pmra) lt 100 and $
            abs(allusno.pmde) lt 100 and allusno.bmag lt blim,nugd)


; Now the DAOPHOT stars
ilim = siglim
pgd = where(allphot.i lt ilim and abs(allphot.sharp) lt 1.5 and allphot.ierr lt 0.2,npgd)

lim = 2.0/3600.0d0       ; in degrees
temp = allphot[pgd]
temp.ra = temp.ra+rashift
temp.dec = temp.dec+decshift
temp.ra = temp.ra*cos(temp.dec/!radeg)
utemp = allusno[ugd]
utemp.raj2000 = utemp.raj2000*cos(utemp.dej2000/!radeg)
SRCOR,utemp.raj2000,utemp.dej2000,temp.ra,temp.dec,lim,ind1,ind2,option=1
;SRCOR,allusno[ugd].raj2000*cos(allusno,allusno[ugd].dej2000,temp.ra,temp.dec,lim,ind1,ind2,option=1

; NOT enough matches, loosen the matching radius
matchlim = 500.
if n_elements(ind1) lt matchlim then begin
  lim = 2.0/3600.0d0       ; in degrees
  SRCOR,utemp.raj2000,utemp.dej2000,temp.ra,temp.dec,lim,ind1,ind2,option=1
endif

; The matched structures
allusno3 = allusno[ugd[ind1]]
allphot3 = allphot[pgd[ind2]]

; Fitting, NORD=2
FIT_RADEC,allusno3,allphot3,trans2,nord=2
print,'Nord=2  RMS=',stringize(trans2.rms,ndec=3),' arcsec'
gtrans = trans2  ; Initial solution

; Fitting, NORD=3
if maxord ge 3 then begin
  FIT_RADEC,allusno3,allphot3,trans3,nord=3,init=trans2
  print,'Nord=3  RMS=',stringize(trans3.rms,ndec=3),' arcsec'
  if trans3.rms lt gtrans.rms then gtrans=trans3  ; Is this solution better?
endif

; Fitting, NORD=4
if maxord ge 4 then begin
  FIT_RADEC,allusno3,allphot3,trans4,nord=4,init=trans3
  print,'Nord=4  RMS=',stringize(trans4.rms,ndec=3),' arcsec'
  if trans4.rms lt gtrans.rms then gtrans=trans4  ; Is this solution better?
endif

;; Final transformation
;; Use the best solution
;rms = [trans2.rms,trans3.rms,trans4.rms]
;best = first_el(minloc(rms))
;case best of
;  0: gtrans = trans2
;  1: gtrans = trans3
;  2: gtrans = trans4
;endcase


; Getting the transformation just for the good stars
TRANS_RADEC,gtrans,allphot3.ra0,allphot3.dec0,newra,newdec
allphot3.ra = newra
allphot3.dec = newdec

;; Getting the transformation for all the stars
;TRANS_RADEC,gtrans,allphot.ra0,allphot.dec0,newra,newdec
;allphot_orig = allphot
;allphot.ra = newra
;allphot.dec = newdec

if keyword_set(stp) then stop

end

;------------------------------------


pro get_astrom_dir,dirnames,refitall=refitall,refitind=refitind,refitchip=refitchip,$
                   plotresid=plotresid,maxord=maxord,stp=stp,standard=standard,$
                   plotusno=plotusno

; This runs GET_ASTROM.PRO on a whole directory
; of files
; It checks if get_astrom has already been run
; /refitind   Refit the individual FITS files with get_astrom.pro
; /refitchip  Refit the chips
; /refitall   Refit everything
; maxord      maximum order to fit
; /standard    Use the standard chip transformations for this night

; Not enough inputs
if n_elements(dirnames) eq 0 then begin
  print,'Syntax - get_astrom_dir,dirnames,refit=refit,plotresid=plotresid,'
  print,'                        global=global,maxord=maxord,stp=stp'
  return
endif

retry = 1  ; try to refit frames with the global solution
           ; that initially crashed

if n_elements(maxord) eq 0 then maxord=4   ; this is good enough

if keyword_set(refitall) then begin
  refitind = 1
  refitchip = 1
end

; Do global by default to check the fit
if n_elements(global) eq 0 then global=1   

; A list file was input
if strmid(dirnames[0],0,1) eq '@' then begin

  inp = strmid(dirnames[0],1)

  ; Checking that the file exists
  test = file_test(inp)
  if test eq 0 then begin
    print,'FILE ',inp,' DOES NOT EXIST'
    return
  endif

  ; Loading the files
  readcol,inp,dirs,format='A',/silent,comment='#'
  ndirs = n_elements(dirs)

endif else begin

  ; Probably an array of filenames
  if n_elements(dirnames) gt 1 then begin
    dirs = dirnames
    ndirs = n_elements(dirs)

  ; A globbed list or only one file
  endif else begin
    dirs = file_search(dirnames)
    ndirs = n_elements(dirs)
  endelse

endelse

if ndirs gt 1 then begin
  print,''
  print,'RUNNING GET_ASTROM_DIR on ',strtrim(ndirs,2),' DIRECTORIES'
  print,''
endif

; Initial directory
cd,current=curdir

; LOOPING OVER THE DIRECTORIES
FOR d=0,ndirs-1 do begin

  ; Always start at the initial directory
  cd,curdir

  dir = dirs[d]
  dir2 = file_expand_path(dir)

  ; Check that this directory exists
  test = file_test(dir2,/directory)
  if test eq 0 then begin
    print,'DIRECTORY ',dir2,' DOES NOT EXIST'
    return
  endif

  field = file_basename(dir2)

  ; Print directory
  print,'------------------------------------------'
  print,dir2
  print,'------------------------------------------'
  print,''

  ; Getting the phot files
  photfiles = file_search(dir2+'/*.phot')
  nphotfiles = n_elements(photfiles)

  ; NO PHOT files
  if nphotfiles eq 0 then begin
    print,'NO PHOT FILES'
    return
  endif

  t0 = systime(1)

  cd,current=curdir

  ;########################################
  ;# STEP 1.  INDIVIDUAL SOLUTIONS
  ;########################################

  ; Looping through the files
  for i=0,nphotfiles-1 do begin

    ; Make sure we're in the right directory
    cd,curdir

    dir1 = file_dirname(photfiles[i])
    filebase = file_basename(photfiles[i],'.phot')

    ; Check for the FITS and ALS files
    fitsfile = dir2+'/'+filebase+'.fits'
    ftest = file_test(fitsfile)
    if ftest eq 0 then begin
      print,'NO FITS FILE FOR ',photfiles[i]
      goto,BOMB
    endif

    alsfile = dir2+'/'+filebase+'.als'
    atest = file_test(alsfile)
    if atest eq 0 then begin
      print,'NOT ALS FILE FOR ',photfiles[i]
      goto,BOMB
    endif

    ; Check for DATA to see if get_astrom has already
    ; been run previously
    datfile = dir2+'/astrom/'+filebase+'_astrom.dat'
    dattest = file_test(datfile)
    if dattest eq 1 then begin
      print,'Solution already exists for ',filebase
    endif

    ; First solution or refitting
    if dattest eq 0 or keyword_set(refitind) then begin

      if dattest eq 1 and keyword_set(refitind) then print,'REFITTING'

      ; Run get_astrom
      GET_ASTROM,fitsfile,maxord=maxord,plotresid=plotresid

      print,''

    endif

    BOMB:

  end ; phot files

  ; Print out how long it took
  dt = systime(1)-t0
  print,'Time = ',stringize(dt,ndec=2),' seconds'
  print,''


  ;#####################################
  ;# Concatenating all the files
  photfiles2 = file_basename(photfiles,'.phot')
  datfiles = dir2+'/astrom/'+photfiles2+'_astrom.dat'
  CATALLASTROM,datfiles,allusno,allphot,allusno2,allphot2,alltrans,missing

  ; Plot allphot on the sky
  if keyword_set(plotresid) then begin
    xr = [max(allphot.ra)+0.1,min(allphot.ra)-0.1]
    yr = [min(allphot.dec)-0.1,max(allphot.dec)+0.1]
    plot,allphot.ra,allphot.dec,ps=3,xr=xr,yr=yr,xs=1,ys=1,xtit='RA',ytit='DEC',title=field+' Individual Solutions'
    wait,1
  endif


  ;##############################################################
  ;# STEP 2.  SOLUTIONS FOR EACH CHIP
  ;##############################################################
  nchip = 8
  ; Loop over the chips
  for i=0,nchip-1 do begin

    chip = i+1
    schip = strtrim(long(chip),2)

    fbase = file_basename(photfiles[0],'.phot')
    dum = strsplit(fbase,'_',/extract)
    base = dum[0]

    ;; Using the standard chip transformation
    ;if keyword_set(standard) then begin
    ;  ; Creating the filename
    ;  lo = stregex(dir2,'/n[1-9]/')
    ;  snight = strmid(dir2,lo+2,1)
    ;  nightdir = strmid(dir2,0,lo+3)
    ;  fil = nightdir+'/n'+snight+'chip'+schip+'_trans.dat'
    ;
    ;  test = file_test(fil)
    ;  if test eq 0 then begin
    ;    print,'NO STANDARD TRANSFORMATION FOR NIGHT ',snight,' CHIP ',schip
    ;  endif
    ;
    ;  ; Restore the transformation
    ;  restore,fil
    ;  stdtrans = trans
    ;endif

    ; We need 4th order at least
    GET_ASTROM_CHIP,dir2+'/'+base,chip,refit=refitchip,maxord=4,plotresid=plotresid,$
                    standard=standard,stp=stp

  end ; chip loop


  ;#####################################
  ;# Concatenating all the files
  ;# Check for missing ones
  datfiles = dir2+'/astrom/'+base+'_chip'+strtrim(lindgen(nchip)+1,2)+'_astrom.dat'
  CATALLASTROM,datfiles,challusno,challphot,challusno2,challphot2,challtrans,chmissing
  if n_elements(challusno) eq 0 then goto,BOMB_END

  if keyword_set(plotresid) then $   
    PLOT_ASTROM_RESID,challusno2.raj2000,challusno2.dej2000,challphot2.ra,challphot2.dec,ps=3,title=field,/noprint

  wait,1

  ;########################################
  ;# Plot the final positions on the sky
  if keyword_set(plotresid) then begin
    wset,1
    xr = [max(challphot.ra)+0.1,min(challphot.ra)-0.1]
    yr = [min(challphot.dec)-0.1,max(challphot.dec)+0.1]
    plot,challphot.ra,challphot.dec,ps=3,xr=xr,yr=yr,xs=1,ys=1,xtit='RA',ytit='DEC',title=field+' Chip Solution'
  end


  ;##########################################
  ;# Comparing with USNO-B1
  if keyword_set(plotusno) then begin
    maglim = 23.
  
    ; USNO first
    blim = maglim
    errlim = 400.  
    pmlim = 100.
    ugd = where(challusno.e_radeg lt errlim and challusno.e_dedeg lt errlim and abs(challusno.pmra) lt pmlim and $
               abs(challusno.pmde) lt pmlim and challusno.bmag lt blim,nugd)
  
    ; Now the DAOPHOT stars
    ilim = maglim
    sharplim = 1.5
    photlim = 0.2
    pgd = where(challphot.i lt ilim and abs(challphot.sharp) lt sharplim and challphot.ierr lt photlim,npgd)
  
    utemp = challusno[ugd]
    ptemp = challphot[pgd]
  
    ; Finding matches
    SRCOR,utemp.raj2000,utemp.dej2000,ptemp.ra,ptemp.dec,3.0,ind1,ind2,opt=1,sph=2
  
    ; Comparing them
    COMPARE_ASTROM,utemp[ind1].raj2000,utemp[ind1].dej2000,ptemp[ind2].ra,ptemp[ind2].dec
  
    ; Plotting the residuals
    if keyword_set(plotresid) then $
      PLOT_ASTROM_RESID,utemp[ind1].raj2000,utemp[ind1].dej2000,ptemp[ind2].ra,ptemp[ind2].dec,$
        ps=3,title=field,/noprint
    wait,1
  endif ; /plotusno

  finalrms = mean(challtrans.rms)
  ;finalrms = mean(alltrans.rms)

  print,''
  print,'##########################'
  print,'FINAL RMS = ',stringize(finalrms,ndec=3),' arcsec'
  print,'##########################'
  print,''

  ; Checking that this solution is satisfactory
  rmslim = 0.30   ; arcsec
  if finalrms gt rmslim then begin
    print,'SOLUTION IS NOT GOOD ENOUGH'
  endif

  wset,0

  BOMB_END:

END  ; directory loop

print,''
print,'GET_ASTROM_DIR FINISHED'

if keyword_set(stp) then stop

end
