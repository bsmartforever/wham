pro get_astrom_chip,filebase,chip,maxord=maxord,refit=refit,plotresid=plotresid,stp=stp,$
                    standard=standard,nosector=nosector,secmaglim=secmaglim,$
                    secerrlim=secerrlim,secpmlim=secpmlim,secsharplim=secsharplim,$
                    secphotlim=secphotlim

; This does the astrometry fit on one chip
; filebase   direcory+base, i.e /net/halo/dln5q/ctio4m/n1/130L134a/obj1110
; standard   Use this standard chip transformation
; /nosector  Don't do the sector check

; Not enough inputs
if n_elements(filebase) eq 0 or n_elements(chip) eq 0 then begin
  print,'Syntax - get_astrom_chip,filebase,chip,maxord=maxord,refit=refit,plotresid=plotresid,stp=stp'
  return
endif

; Error handling
CATCH, error_status

if error_status ne 0 then begin
  print, 'Error index: ', error_status
  print, 'Error message: ', !ERROR_STATE.MSG
  goto,BOMB
  CATCH, /CANCEL
endif

dir = file_dirname(filebase)
dir2 = file_expand_path(dir)
base = file_basename(filebase)

ichip = long(chip)
schip = strtrim(ichip,2)

; Are we using a standard solution
if keyword_set(standard) then begin
  ; Creating the filename
  lo = stregex(dir2,'/n[1-9]/')
  snight = strmid(dir2,lo+2,1)
  nightdir = strmid(dir2,0,lo+3)
  fil = nightdir+'/n'+snight+'chip'+schip+'_trans.dat'

  test = file_test(fil)
  if test eq 0 then begin
    print,'NO STANDARD TRANSFORMATION FOR NIGHT ',snight,' CHIP ',schip
    return
  endif

  ; Restore the transformation
  restore,fil
  stdtrans = trans
endif

if n_elements(maxord) eq 0 then maxord=4

; Checking to see if a solution exists for this chip
datfile = dir+'/astrom/'+base+'_chip'+schip+'_astrom.dat'
dattest = file_test(datfile)

; Solution already exists
if dattest eq 1 then begin
  print,'Solution already exists for ',base+'_chip'+schip
endif

; First solution or refitting
if dattest eq 0 or keyword_set(refit) or keyword_set(standard) then begin

  ; Refitting
  if dattest eq 1 and keyword_set(refit) then print,'REFITTING'

  print,''
  print,'FINDING SOLUTION FOR CHIP ',schip
  print,''


  ; The indices for the two frames
  num1 = ichip*2-1
  num2 = ichip*2
  snum1 = strtrim(num1,2)
  snum2 = strtrim(num2,2)

  ; FITS filenames
  fitsfile1 = dir+'/'+base+'_'+snum1+'.fits'
  fitsfile2 = dir+'/'+base+'_'+snum2+'.fits'

  ; Check the FITS files
  fits1test = file_test(fitsfile1)
  fits2test = file_test(fitsfile2)
  if fits1test eq 0 then begin
    print,'FILE ',fitsfile1,' DOES NOT EXIST
    goto,BOMB
  endif
  if fits2test eq 0 then begin
    print,'FILE ',fitsfile2,' DOES NOT EXIST
    goto,BOMB
  endif
  head1 = headfits(fitsfile1)
  head2 = headfits(fitsfile2)

  nx1 = sxpar(head1,'NAXIS1')
  ny1 = sxpar(head1,'NAXIS2')
  nx = 2*nx1   ; total NX,NY
  ny = ny1

  ; Check the individual DAT files

  ; DAT filenames
  datfile1 = dir+'/astrom/'+base+'_'+snum1+'_astrom.dat'
  datfile2 = dir+'/astrom/'+base+'_'+snum2+'_astrom.dat'

  ; Check the DAT files
  dat1test = file_test(datfile1)
  dat2test = file_test(datfile2)

  ; No DAT file, get USNO and PHOT files
  if dat1test eq 0 then begin
    ; Get USNO info
    GET_USNO,head1,usno
    
    ; Load the DAOPHOT file
    photname = dir+'/'+base+'_'+snum1+'.phot'
    LOAD_PHOT,photname,head1,phot
    print,'No solution for ',base+'_'+snum1
  endif else begin
    restore,datfile1
  endelse

  ; GET_ASTROM didn't crash
  if n_elements(usno2) gt 1 then begin
    usno1 = usno
    phot1 = phot
    usno12 = usno2
    phot12 = phot2
    trans1 = trans
  endif else begin
    usno1 = usno
    phot1 = phot
  endelse


  ; GETTING THE SECOND FILE
  if dat2test eq 0 then begin
    ; Get USNO info
    GET_USNO,head2,usno
    
    ; Load the DAOPHOT file
    photname = dir+'/'+base+'_'+snum2+'.phot'
    LOAD_PHOT,photname,head2,phot
    print,'No solution for ',base+'_'+snum2
  endif else begin
    restore,datfile2
  endelse

  ; GET_ASTROM didn't crash
  if n_elements(usno2) gt 1 then begin
    usno22 = usno2
    phot22 = phot2
    usno2 = usno
    phot2 = phot
    trans2 = trans
  endif else begin
    usno2 = usno
    phot2 = phot
  endelse

  phot2_orig = phot2
  phot2.x = phot2.x+nx1    ; need to shift the X positions
  usno2_orig = usno2
  usno2.x = usno2.x+nx1    ; need to shift the X positions

  if n_elements(usno22) gt 0 then begin
    usno22_orig = usno22
    usno22.x = usno22.x+nx1
    phot22_orig = phot22
    phot22.x = phot22.x+nx1
  endif

  ; Concatenating
  chusno = [usno1,usno2]
  ui = uniq(chusno.usno_b1_0,sort(chusno.usno_b1_0))
  chusno = chusno[ui]
  chphot = [phot1,phot2]
  if n_elements(usno12) gt 0 then push,chusno2,usno12
  if n_elements(usno22) gt 0 then push,chusno2,usno22
  if n_elements(phot12) gt 0 then push,chphot2,phot12
  if n_elements(phot22) gt 0 then push,chphot2,phot22
  ;chusno2 = [usno12,usno22]
  ;chphot2 = [phot12,phot22]

  ; No initial matches, use standard DAOMATCH/MASTER to get initial matches
  if n_elements(chusno2) eq 0 then begin

    print,''
    print,'USING DAOMATCH/DAOMASTER TO GET INITIAL MATCHES'

    ; ALS filename
    alsfile = dir+'/'+base+'_'+snum1+'.als'

    ; Load the ALS header
    alshead = LOAD_DAOHEAD(alsfile)


    maglim = 23.
  
    ; USNO first
    usnotemp = dir+'/astrom/'+base+'_chip'+schip+'_usno.tempcoo'
    blim = maglim
    errlim = 400.
    pmlim = 100.
    ugd = where(chusno.x ge 0. and chusno.x le nx-1 and chusno.y ge 0 and chusno.y le ny-1 and $
               chusno.e_radeg lt errlim and chusno.e_dedeg lt errlim and abs(chusno.pmra) lt pmlim and $
               abs(chusno.pmde) lt pmlim and chusno.bmag lt blim,nugd)
    WRITE_PHOT,chusno[ugd],usnotemp,daohead=alshead

    ; Now the DAOPHOT stars
    daotemp = dir+'/astrom/'+base+'_chip'+schip+'_dao.tempcoo'
    ilim = maglim
    sharplim = 1.5
    photlim = 0.2
    pgd = where(chphot.x ge 0 and chphot.x le nx-1 and chphot.y ge 0 and chphot.y le ny-1 and $
               chphot.i lt ilim and abs(chphot.sharp) lt sharplim and chphot.ierr lt photlim,npgd)

    WRITE_PHOT,chphot[pgd],daotemp,daohead=alshead


    ; Run DAOMATCH and DAOMASTER on the files
    RUN_DAOMATCH,[usnotemp,daotemp],trans1a
    RUN_DAOMASTER,usnotemp+'.mch',trans1b,siglim=siglim,ndegree=4,$
                  ind1=usno_ind,ind2=phot_ind

    chusno2 = chusno[ugd[usno_ind]]
    chphot2 = chphot[pgd[phot_ind]]

    print,'DAOMATCH Nmatch = ',strtrim(n_elements(chusno2),2)
    print,''    

  endif ; not initial matches


  ; GETTING AN INITIAL SOLUTION
  ; Use the stars matched from the INDIVIDUAL frames

  ; Fitting, NORD=2
  FIT_RADEC,chusno2,chphot2,trans2,nord=2,xzero=nx/2,yzero=ny/2
  if size(trans2,/type) ne 8 then begin
    print,'BAD FIT - BOMBING'
    goto,BOMB
  endif
  print,'Nord=2  RMS=',stringize(trans2.rms,ndec=3),' arcsec'

  ; Get new and improved coordinates
  TRANS_RADEC,trans2,chphot.x,chphot.y,newra,newdec
  chphot.ra = newra  
  chphot.dec = newdec
  rms = trans2.rms

  ; Bad fit, redo if possible with 3rd order
  if trans2.rms gt 0.6 and n_elements(chusno2) ge 10 then begin
    ; Fitting, NORD=3
    FIT_RADEC,chusno2,chphot2,trans3,nord=3,xzero=nx/2,yzero=ny/2
    if size(trans3,/type) ne 8 then begin
      print,'BAD FIT - BOMBING'
      goto,BOMB
    endif
    print,'Nord=3  RMS=',stringize(trans3.rms,ndec=3),' arcsec'

    ; Get new and improved coordinates
    TRANS_RADEC,trans3,chphot.x,chphot.y,newra,newdec
    chphot.ra = newra  
    chphot.dec = newdec
    rms = trans3.rms
  endif



  ; ---Now try to match more---

  maglim = 23.

  ; USNO first
  ; I'm not sure BMAG cut is helping much
  ; The e_radeg and e_dedeg cuts are important
  blim = maglim
  errlim = 400.  
  pmlim = 100.
  ugd = where(chusno.x ge 0. and chusno.x le nx-1 and chusno.y ge 0 and chusno.y le ny-1 and $
             chusno.e_radeg lt errlim and chusno.e_dedeg lt errlim and abs(chusno.pmra) lt pmlim and $
             abs(chusno.pmde) lt pmlim and chusno.bmag lt blim,nugd)
  
  ; Now the DAOPHOT stars
  ilim = maglim
  sharplim = 1.5
  photlim = 0.2
  pgd = where(chphot.x ge 0 and chphot.x le nx-1 and chphot.y ge 0 and chphot.y le ny-1 and $
             chphot.i lt ilim and abs(chphot.sharp) lt sharplim and chphot.ierr lt photlim,npgd)

  ; Try to match all stars at first
  ; use a very loose constraint in case the global solution is bad for this chip
  lim = 2.0 > 2.0*rms   ; arcsec
  SRCOR,chusno[ugd].raj2000,chusno[ugd].dej2000,chphot[pgd].ra,chphot[pgd].dec,lim,ind1,ind2,option=1,sph=2
  print,'Nmatch = ',strtrim(n_elements(ind1),2)

  chusno3 = chusno[ugd[ind1]]
  chphot3 = chphot[pgd[ind2]]

  ; Fitting, NORD=3
  FIT_RADEC,chusno3,chphot3,trans3,nord=3,xzero=nx/2,yzero=ny/2
  if size(trans3,/type) ne 8 then begin
    print,'BAD FIT - BOMBING'
    goto,BOMB
  endif
  print,'Nord=3  RMS=',stringize(trans3.rms,ndec=3),' arcsec'

  ; Get new and improved coordinates
  TRANS_RADEC,trans3,chphot3.x,chphot3.y,newra,newdec
  chphot3.ra = newra  
  chphot3.dec = newdec
  rms = trans3.rms

  ;stop

  ; ---Tighten the matching radius---

  ; Try to match all stars at first
  ; use a very loose constraint in case the global solution is bad for this chip
  lim = 0.5 > 2.0*rms   ; arcsec   
  SRCOR,chusno3.raj2000,chusno3.dej2000,chphot3.ra,chphot3.dec,lim,ind1,ind2,option=1,sph=2
  print,'Nmatch = ',strtrim(n_elements(ind1),2)
  
  chusno3 = chusno3[ind1]
  chphot3 = chphot3[ind2]

  ; Fitting, NORD=3
  FIT_RADEC,chusno3,chphot3,trans3,nord=3,xzero=nx/2,yzer=ny/2
  if size(trans3,/type) ne 8 then begin
    print,'BAD FIT - BOMBING'
    goto,BOMB
  endif
  print,'Nord=3  RMS=',stringize(trans3.rms,ndec=3),' arcsec'

  ; Get new and improved coordinates
  TRANS_RADEC,trans3,chphot3.x,chphot3.y,newra,newdec
  chphot3.ra = newra
  chphot3.dec = newdec


  ; REMOVE OUTLIERS

  ; Throw out bad stars
  chphot3_orig = chphot3
  chusno2_orig = chusno3
  flag = 0
  count = 1  
  niter = 5
  ntoss = 0
  while (flag ne 1) do begin
    ; Getting the residuals and rms
    COMPARE_ASTROM,chusno3.raj2000,chusno3.dej2000,chphot3.ra,chphot3.dec,raresid,decresid,resid,rms=rms,/noprint
    usnoerr = sqrt( (chusno3.e_radeg/1d3)^2. + (chusno3.e_dedeg/1d3)^2. )

    ; Using a 3 sigma cut
    lim = (3.0*rms) > 0.25
    bad = where( resid gt lim OR usnoerr gt lim ,nbad)
    if nbad gt 0 then remove,bad,chphot3,chusno3

    ; Is this the end?
    if nbad eq 0 or count ge niter then flag=1
    ntoss = ntoss+nbad

    ; Okay this is the last one
    ; make sure the cut was low enough
    if (flag eq 1) then begin
      COMPARE_ASTROM,chusno3.raj2000,chusno3.dej2000,chphot3.ra,chphot3.dec,raresid,decresid,resid,rms=rms,/noprint
      usnoerr = sqrt( (chusno3.e_radeg/1d3)^2. + (chusno3.e_dedeg/1d3)^2. )

      bad = where( resid gt 0.40 OR usnoerr gt 0.40 ,nbad)
      if nbad gt 0 then remove,bad,chphot3,chusno3
      ntoss = ntoss+nbad
    endif
  
    count = count+1
  endwhile
  print,'Tossing out ',strtrim(ntoss,2),' Bad stars'
  print,'Nmatch = ',strtrim(n_elements(chphot3),2)


  ;####################################################
  ;# DOUBLE-CHECK THAT WE HAVE STARS IN ALL SECTORS
  if not keyword_set(nosector) then begin
    print,''
    print,'MAKING SURE THERE ARE STARS IN EACH SECTOR'
    newstars = 0
    nseccols = (nx/512) 
    nsecrows = (ny/512)

    ; Loop through the columns
    for i=0,nseccols-1 do begin
      xlo = i*512+1L
      xhi = xlo+511L

      ; Loop through the rows
      for j=0,nsecrows-1 do begin
        ylo = j*512+1L
        yhi = ylo+511L
        gg = where(chphot3.x ge xlo and chphot3.x le xhi and chphot3.y ge ylo and chphot3.y le yhi,ngg)

        ; Not enough stars in this sector
        seclim = 5
        if ngg lt seclim then begin

          ; Remove the stars that are already matched in this sector
          if ngg gt 0 then remove,gg,chusno3,chphot3

          ; Get stars in this sector
          if n_elements(secmaglim) eq 0 then secmaglim = 100.   ; no limit
  
          ; USNO
          ; I'm not sure BMAG cut is helping much
          ; The e_radeg and e_dedeg cuts are important
          blim = secmaglim
          if n_elements(secerrlim) eq 0 then secerrlim = 1100.
          if n_elements(secpmlim) eq 0 then secpmlim = 200.
          ugg = where(chusno.x ge xlo and chusno.x le xhi and chusno.y ge ylo and chusno.y le yhi and $
                      chusno.e_radeg lt secerrlim and chusno.e_dedeg lt secerrlim and $
                      abs(chusno.pmra) lt secpmlim and abs(chusno.pmde) lt secpmlim and $
                      chusno.bmag lt blim,nugg)
          utemp = chusno[ugg]

          ; DAOPHOT stars
          secilim = secmaglim
          if n_elements(secsharplim) eq 0 then secsharplim = 10.0
          if n_elements(secphotlim) eq 0 then secphotlim = 10.0
          pgg = where(chphot.x ge xlo and chphot.x le xhi and chphot.y ge ylo and chphot.y le yhi and $
                      chphot.i lt secilim and abs(chphot.sharp) lt secsharplim and $
                      chphot.ierr lt secphotlim,npgg)
          ptemp = chphot[pgg]

          ;ugg = where(chusno.x ge xlo and chusno.x le xhi and chusno.y ge ylo and chusno.y le yhi,nugg)
          ;utemp = chusno[ugg]
          ;pgg = where(chphot.x ge xlo and chphot.x le xhi and chphot.y ge ylo and chphot.y le yhi,ngg)
          ;ptemp = chphot[pgg]

          ; Try to match them
          lim = 2.0   ; arcsec
          SRCOR,utemp.raj2000,utemp.dej2000,ptemp.ra,ptemp.dec,lim,ind1,ind2,option=1,sph=2

          ; No matches enlarge the matching radius
          if ind1[0] eq -1 then begin        
            lim = 4.0   ; arcsec
            SRCOR,utemp.raj2000,utemp.dej2000,ptemp.ra,ptemp.dec,lim,ind1,ind2,option=1,sph=2          
          endif

          ; We have matches, add to structure
          if ind1[0] ne -1 then begin
            push,chusno3,utemp[ind1]
            push,chphot3,ptemp[ind2]
            newstars = 1                  ; we have added some new stars
          endif else begin
            print,'NO MATCHES FOUND FOR SECTOR ',strtrim(i,2),',',strtrim(j,2)
          endelse

        end  ; not enough stars in sector
 
    end  ; row loop
    end  ; column loop

    ; ##########################################
    ; # NEW STARS
    ; Find solution and update ra/dec
    ; Do one outlier rejection to get rid of terrible stars.
    if (newstars eq 1) then begin

      ; Some of these stars still have old coordinates
      TRANS_RADEC,trans3,chphot3.x,chphot3.y,newra,newdec
      chphot3.ra = newra
      chphot3.dec = newdec

      ; Fitting again with the NEW stars as well
      FIT_RADEC,chusno3,chphot3,trans3,nord=3,xzero=nx/2,yzer=ny/2
      if size(trans3,/type) ne 8 then begin
        print,'BAD FIT - BOMBING'
        goto,BOMB
      endif
      print,'Nord=3  RMS=',stringize(trans3.rms,ndec=3),' arcsec'
    
      ; Get new and improved coordinates
      TRANS_RADEC,trans3,chphot3.x,chphot3.y,newra,newdec
      chphot3.ra = newra
      chphot3.dec = newdec

      ; Do ONE outlier rejection to get the REALLY BAD stars
      COMPARE_ASTROM,chusno3.raj2000,chusno3.dej2000,chphot3.ra,chphot3.dec,raresid,decresid,$
                     resid,rms=rms,/noprint

      lim = (3.0 * trans3.rms > 0.6) < 1.5
      bad = where( resid gt lim,nbad)
      if nbad gt 0 then remove,bad,chphot3,chusno3
      print,'Tossing out ',strtrim(nbad,2),' Bad stars'
      print,'Nmatch = ',strtrim(n_elements(chphot3),2)

    endif ; new stars

  endif ; check sector

  ;###########################################
  ;# ARE WE USING A STANDARD TRANSFORMATION??
  if n_elements(stdtrans) ne 0 then begin
    trans = trans2
    if n_elements(trans3) ne 0 then trans=trans3


    ; Getting DEC at the center of the field
    TRANS_RADEC,trans,stdtrans.xzero,stdtrans.yzero,cen_ra,cen_dec

    ; Calculating the field rotation terms
    expr = 'P(0)+P(1)*tan(X/!radeg)'
    rpar10 = mpevalexpr(expr,cen_dec,stdtrans.rafpar)
    dpar01 = mpevalexpr(expr,cen_dec,stdtrans.decfpar)

    ; Making the input transformation
    init = stdtrans
    init.rpar[1,0] = rpar10
    init.dpar[0,1] = dpar01
    init.razero = cen_ra
    init.deczero = cen_dec

    ; MAYBE rpar[0,1] and dpar[1,0] need to be "rotated" as well?
    ;stop

    ; Making Npole and equator
    npole = [cen_ra,90.0d0+cen_dec]
    if cen_dec gt 0.0 then begin
      ranpole = cen_ra-180.0d0
      if ranpole lt 0.0 then ranpole=ranpole+360.0d0
      npole = [ranpole,90.0d0-cen_dec]
    endif
    init.npole = npole
    init.equator = [cen_ra,cen_dec]

    ; Fitting
    FIT_RADEC,chusno3,chphot3,chtrans,nord=4,xzero=init.xzero,yzero=init.yzero,$
              init=init,/fitrot

    ; Getting new coordinates
    TRANS_RADEC,chtrans,chphot3.x,chphot3.y,newra,newdec
    chphot3.ra = newra  
    chphot3.dec = newdec

    ; Getting mean offset
    COMPARE_ASTROM,chusno3.raj2000,chusno3.dej2000,chphot3.ra,chphot3.dec,raresid,decresid,/noprint

    cen_ra = chtrans.razero+median(raresid)
    cen_dec = chtrans.deczero+median(decresid)

    ; Shifting the constant terms
    chtrans.rpar[0,0] = chtrans.rpar[0,0]+median(raresid)
    chtrans.dpar[0,0] = chtrans.dpar[0,0]+median(decresid)

    ; Getting new coordinates
    TRANS_RADEC,chtrans,chphot3.x,chphot3.y,newra,newdec
    chphot3.ra = newra
    chphot3.dec = newdec

    ; Getting new RMS values
    COMPARE_ASTROM,chusno3.raj2000,chusno3.dej2000,chphot3.ra,chphot3.dec,raresid,decresid,$
                   rms=rms,/noprint
    chtrans.rrms = sqrt(mean(raresid^2.0))
    chtrans.drms = sqrt(mean(decresid^2.0))
    chtrans.rms = rms
    
    ;; 2nd fit
    ;init2 = chtrans
    ;FIT_RADEC,chusno3,chphot3,chtrans,nord=4,xzero=init2.xzero,yzero=init2.yzero,$
    ;          init=init2,/fitrot,razero=init2.razero,deczero=init2.deczero
    ;
    ;; Getting new coordinates
    ;TRANS_RADEC,chtrans,chphot3.x,chphot3.y,newra,newdec
    ;chphot3.ra = newra
    ;chphot3.dec = newdec

    ; Plotting
    ;plot_astrom_resid,chusno3.raj2000,chusno3.dej2000,newra,newdec

    ;stop

  endif ; standard chip solution



  ; ---Using higher order fits---
  ; If not using a standard solution
  if not keyword_set(standard) then begin


    ; Fitting, NORD=2
    FIT_RADEC,chusno3,chphot3,trans3,nord=2,xzero=nx/2,yzero=ny/2
    if size(trans2,/type) ne 8 then begin
      print,'BAD FIT - BOMBING'
      goto,BOMB
    endif
    print,'Nord=2  RMS=',stringize(trans2.rms,ndec=3),' arcsec'
    chtrans = trans2

    ; Fitting, NORD=3
    if maxord ge 3 and n_elements(chusno3) ge 9 then begin
      FIT_RADEC,chusno3,chphot3,trans3,nord=3,xzero=nx/2,yzero=ny/2
      if size(trans3,/type) ne 8 then begin
        print,'BAD FIT - BOMBING'
        goto,BOMB
      endif
      print,'Nord=3  RMS=',stringize(trans3.rms,ndec=3),' arcsec'
      if trans3.rms lt chtrans.rms then chtrans=trans3      ; Is this solution better?
    endif

    ; Fitting, NORD=4
    if maxord ge 4 and n_elements(chusno3) ge 16 then begin
      FIT_RADEC,chusno3,chphot3,trans4,nord=4,xzero=nx/2,yzero=ny/2
      if size(trans4,/type) ne 8 then begin
        print,'BAD FIT - BOMBING'
        goto,BOMB
      endif
      print,'Nord=4  RMS=',stringize(trans4.rms,ndec=3),' arcsec'
      if trans4.rms lt chtrans.rms then chtrans=trans4      ; Is this solution better?
    endif

    ; Fitting, NORD=5
    if maxord ge 5 and n_elements(chusno3) ge 25 then begin
      FIT_RADEC,chusno3,chphot3,trans5,nord=5,xzero=nx/2,yzero=ny/2
      if size(trans5,/type) ne 8 then begin
        print,'BAD FIT - BOMBING'
        goto,BOMB
      endif
      print,'Nord=5  RMS=',stringize(trans5.rms,ndec=3),' arcsec'
      if trans5.rms lt chtrans.rms then chtrans=trans5      ; Is this solution better?
    endif

    ; Fitting, NORD=6
    if maxord ge 6 and n_elements(chusno3) ge 36 then begin
      FIT_RADEC,chusno3,chphot3,trans6,nord=6,xzero=nx/2,yzero=ny/2
      if size(trans6,/type) ne 8 then begin
        print,'BAD FIT - BOMBING'
       goto,BOMB
      endif
      print,'Nord=6  RMS=',stringize(trans6.rms,ndec=3),' arcsec'
      if trans6.rms lt chtrans.rms then chtrans=trans6      ; Is this solution better?
    endif

    ; If we have few stars then keep it to a low order
    nusno2 = n_elements(chusno3)
    if nusno2 lt 50. then chtrans=trans3                           ; at maximum third order
    if nusno2 lt 50. and trans2.rms lt 0.25 then chtrans=trans2    ; 2nd order if good enough

    ; Get new coordinates using this transformation for the BEST stars
    TRANS_RADEC,chtrans,chphot3.x,chphot3.y,newra,newdec
    chphot3.ra = newra
    chphot3.dec = newdec 

  endif ; not standard solution


  ; Final rms
  print,'FINAL RMS = ',stringize(chtrans.rms,ndec=3)

  ; Get new coordinates using this transformation for all stars
  TRANS_RADEC,chtrans,chphot.x,chphot.y,newra,newdec
  chphot.ra = newra
  chphot.dec = newdec 


  ; Final transformations
  TRANS_RADEC,chtrans,phot1.x,phot1.y,newra,newdec
  phot1.ra = newra
  phot1.dec = newdec

  TRANS_RADEC,chtrans,phot2.x,phot2.y,newra,newdec
  phot2.ra = newra
  phot2.dec = newdec

  ; Saving the CHIP file
  datfile = dir+'/astrom/'+base+'_chip'+schip+'_astrom.dat'
  print,'DAT File = ','astrom/'+base+'_chip'+schip+'_astrom.dat'
  save,chusno,chphot,chusno3,chphot3,chtrans,phot1,phot2,file=datfile

  ; Write the final output file.
  outfile1 = dir+'/'+base+'_'+snum1+'.ast'
  print,'FINAL Photometry File = ',base+'_'+snum1+'.ast'
  WRITE_PHOT,phot1,outfile1,/colhead,/both

  outfile2 = dir+'/'+base+'_'+snum2+'.ast'
  print,'FINAL Photometry File = ',base+'_'+snum2+'.ast'
  WRITE_PHOT,phot2,outfile2,/colhead,/both

  ; Plotting residuals
  if keyword_set(plotresid) then begin

    ; Getting boundary for image 1
    nx1 = sxpar(head1,'NAXIS1')
    ny1 = sxpar(head1,'NAXIS2')
    xb1 = [0.,2*nx1-1.,2*nx1-1.,0.]
    yb1 = [0.,0.,ny1-1.,ny1-1.]
    TRANS_RADEC,chtrans,xb1,yb1,rab1,decb1

    xr = minmax(rab1)
    xr = reverse(xr)
    yr = minmax(decb1)

    PLOT_ASTROM_RESID,chusno3.raj2000,chusno3.dej2000,chphot3.ra,chphot3.dec,xr=xr,yr=yr,$
                      title=base+' Chip '+schip
  endif else begin

    ; Printing out the stats
    COMPARE_ASTROM,chusno3.raj2000,chusno3.dej2000,chphot3.ra,chphot3.dec

  endelse

  ; MAYBE COMPARE THE ORIGINAL RA0/DEC0 COORDINATES TO THE NEW ONES (MINUS A CONSTANT OFFSET)


endif else begin  ; first time, or refitting

  ; Restore the DATA file and plot the residuals
  if keyword_set(plotresid) then begin

    ; Restore the DATA file
    restore,datfile

    ; Plotting residuals
    xr = minmax(chusno3.raj2000)
    xr = reverse(xr)
    yr = minmax(chusno3.dej2000)

    PLOT_ASTROM_RESID,chusno3.raj2000,chusno3.dej2000,chphot3.ra,chphot3.dec,xr=xr,yr=yr,$
                      title=base+' Chip '+schip

  endif

endelse

BOMB:

if keyword_set(stp) then stop

end
