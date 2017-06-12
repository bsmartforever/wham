;+
;
; NAME:
;  GET_ASTROM_GLOBAL
;
; PURPOSE:
;  This program gets a global astrometric solution for MOSAIC data
;
; INPUTS:
;  fdir     Main directory of the data
;  /noplot  Don't plot anything
;  /stp     Stop at the end of the program
;
; OUTPUTS:
;  par      The 40 parameters of the global astrometric solution
;  fcenter  The RA/DEC coordinates of the center of the field
;
; By D. Nidever   Feb-April 2007
;-


function trans_coord,xpin,ypin,chip,par,x=x,y=y,stp=stp,debug=debug

; Apply the transformation from x,y to zeta,eta

radeg = 180.0d0/!dpi

dx = par[0:7]   ; 8 params 
dy = par[8:15]   ; 8 params
theta = par[16:23]  ; 8 params
a = par[24:31]  ; 8 pararms
b = par[32:39]  ; 8 params
;a = par[24:39]  ; 16 pararms
;b = par[40:55]  ; 16 params

xp = double(xpin)
yp = double(ypin)

; Chip constants
; cxi,cyi is the center of chip i
; dxi,dyi represent the variable part of the chip constants
;dx = [10.43, 58.73, 109.88, 153.60, 1.60, 52.76, 111.58, 159.01]   ; or KPNO
;dy = [1.15, 2.99, 3.37, -1.50, 32.23, 40.20, 36.93, 32.76]
;cxi = dxi + 2048*(i-1)  ; for i=1-4
;cxi = dxi + 2048*(i-5)  ; for i=5-8
cx = dblarr(8)
cx[0:3] = dx[0:3] + 2048.0*lindgen(4)      ; for i=1-4
cx[4:7] = dx[4:7] + 2048.0*lindgen(4)      ; for i=5-8

;cyi = dyi               ; for i=1-4
;cyi = dyi + 4096        ; for i=5-8
cy = dblarr(8)
cy[0:3] = dy[0:3]                 ; for i=1-4
cy[4:7] = dy[4:7] + 4096.0        ; for i=5-8


; thetai is the rotation of the chip around its center
; Xp,Yp are the positions of the stars in the chip
;xn = (Xp-1024)*cos(thetai) - (Yp-2048)*sin(thetai) + 1024
;yn = (XP-1024)*sin(thetai) + (Yp-2048)*cos(thetai) + 2048
; THETA is in arcmin
xn = (Xp-1024.0)*cos(theta[chip-1L]/60.0/radeg) - (Yp-2048.0)*sin(theta[chip-1L]/60.0/radeg) + 1024.0
yn = (XP-1024.0)*sin(theta[chip-1L]/60.0/radeg) + (Yp-2048.0)*cos(theta[chip-1L]/60.0/radeg) + 2048.0

; xc,yc are the coordinates for the center of the frame
xc = 4112.0
yc = 4116.0
; x, y are the positions of the stars in the frame, relative to the field center
; global pixel coordinate system
;x = xc - (xn+cx)
;y = yc - (yn+cy)

;x = xc - (xn + cx[chip-1L])
;y = yc - (yn + cy[chip-1L])
x = (xn + cx[chip-1L]) - xc
y = (yn + cy[chip-1L]) - yc

; Distortions
; Add the large-pixel pixels terms to a and b
; zeta is proportional to +Y
; eta is proportional to +X
; The pixel scale is 0.27340"
;a[1] = a[1] + 0.27340/3600.0d0
;b[2] = b[2] + 0.27340/3600.0d0
;;a[2] = a[2] + 0.27340/3600.0d0
;;b[1] = b[1] + 0.27340/3600.0d0
;;a[4] = a[4] + 0.27340/3600.0d0
;;b[1] = b[1] + 0.27340/3600.0d0

eta = a[0]+a[1]*x+a[2]*y+a[3]*x^2.+a[4]*x*y+a[5]*y^2.+a[6]*x*(x^2.+y^2.)+a[7]*x*(x^2.0+y^2.)^2.0
zeta = b[0]+b[1]*x+b[2]*y+b[3]*x^2.+b[4]*x*y+b[5]*y^2.+b[6]*y*(x^2.+y^2.)+b[7]*y*(x^2.0+y^2.)^2.0


; Checking for bad numbers
bad = where(finite(zeta) eq 0,nbad)
if nbad gt 0 then zeta[bad] = 999999.
bad = where(finite(eta) eq 0,nbad)
if nbad gt 0 then eta[bad] = 999999.

nstars = n_elements(zeta)
out = dblarr(2,nstars)
out[0,*] = zeta
out[1,*] = eta

if keyword_set(debug) then begin
  print,min(zeta),max(zeta)
  print,min(eta),max(eta)
  print,dx
  print,dy
  print,theta
  print,a
  print,b
endif

if keyword_set(stp) then stop

return,out

end

;--------------------

function trans_coord_dev,par,x=x,y=y,chip=chip,zeta=zeta,eta=eta,err=err,stp=stp,debug=debug

; X = ra
; Y = dec
; Z = theoretical ra or dec

out = trans_coord(x,y,chip,par,debug=debug)

nzeta = reform(out[0,*])
neta = reform(out[1,*])

diff = sqrt( (nzeta-zeta)^2.0d0 + (neta-eta)^2.0d0 )*3600.d0 ; in arcsec

dev = diff/err

dum = where(finite(dev) eq 0,nbad)
if nbad gt 0 then stop

if keyword_set(debug) then print,min(dev),max(dev)

if keyword_set(stp) then stop

return,dev

end

;-------------------------

pro get_astrom_global,fdir,par,fcenter,outusno,outphot,inpusno=inpusno,inpphot=inpphot,$
                      saveplot=saveplot,psfile=psfile,noplot=noplot,stp=stp

;+
;
; NAME:
;  GET_ASTROM_GLOBAL
;
; PURPOSE:
;  This program gets a global astrometric solution for MOSAIC data
;
; INPUTS:
;  fdir       Main directory of the data
;  inpusno=   Input matched USNO catalog (with inpphot)
;  inpphot=   Input matched PHOT catalog (with inpusno)
;  /saveplot  Save the final plot of residuals
;  psfile=    Filename of final residuals
;  /noplot    Don't plot anything
;  /stp       Stop at the end of the program
;
; OUTPUTS:
;  par        The 40 parameters of the global astrometric solution
;  fcenter    The RA/DEC coordinates of the center of the field
;  outusno    Matched USNO stars
;  outphot    Matched stars from the phot catalog
;
; By D. Nidever   Feb-April 2007
;-

dir = '/net/halo/dln5q/ctio4m/'

undefine,par,fcenter,usno2,phot2

; Not enough inputs
if n_elements(fdir) eq 0 then begin
  print,'Syntax - get_astrom_global,fdir,par,fcenter,usno2,phot2,inpusno=inpusno,inpphot=inpphot,'
  print,'                      saveplot=saveplot,psfile=psfile,noplot=noplot,stp=stp'
  return
endif

;field = '130L134a'
;if n_elements(field) eq 0 then field = '255L032'
;fdir = dir+'n1/'+field+'/'

field = file_basename(fdir)

print,''
print,'  FINDING GLOBAL ASTROMETRIC SOLUTION FOR FIELD ',field
print,''

get_astrom_dir
get_astrom

; Restore the chip files
chfiles = file_search(fdir+'/astrom/*chip*astrom.dat',count=nchfiles)

if field eq '205L150' then remove,7,chfiles     ; This chip has BAD matches
if field eq '270L034' then remove,2,chfiles     ; This chip has BAD matches
if field eq '190L161a' then remove,3,chfiles     ; This chip has BAD matches
if field eq '209L152' then remove,2,chfiles     ; This chip has BAD matches

; We've got some chip files
if nchfiles gt 0 then begin
  catallastrom,chfiles,allusno1,allphot1,allusno2,allphot2,alltrans,missing

  ; Restore ALL the USNO and PHOT data from individual files
  files1 = file_search(fdir+'/astrom/*_?_astrom.dat')
  files2 = file_search(fdir+'/astrom/*_??_astrom.dat')
  files = [files1,files2]
  catallastrom,files,allusno,allphot

; No chip files, use INDIVIDUAL FILES
endif else begin
  ; Restore ALL the USNO and PHOT data from individual files
  files1 = file_search(fdir+'/astrom/*_?_astrom.dat')
  files2 = file_search(fdir+'/astrom/*_??_astrom.dat')
  files = [files1,files2]
  catallastrom,files,allusno,allphot,allusno2,allphot2

endelse

; Were MATCHED structures input?
ninpusno = n_elements(inpusno)
ninpphot = n_elements(inpphot)
if ninpusno gt 0 and ninpusno eq ninpphot then begin
  print,''
  print,'**** USING INPUT MATCHED CATALOGS ****'
  print,''
  allusno2 = inpusno
  allphot2 = inpphot
end

;if field eq '190L182b' then undefine,allphot2,allusno2

base = file_basename(files[0])
base = first_el(strsplit(base,'_',/extract))

; Getting just the "good" USNO stars 
gd = where(allusno2.e_radeg lt 400. and allusno2.e_dedeg lt 400.,ngd)
usno = allusno2[gd]
phot = allphot2[gd]

; Getting data for my stars, chip coordinates
x = phot.x   
y = phot.y
chip = phot.chip

; Getting data for the USNO stars
; Converting to gnomic system
mnra = mean(usno.raj2000)
mndec = mean(usno.dej2000)
ROTSPHCEN,usno.raj2000,usno.dej2000,mnra,mndec,zeta,eta,/gnomic

err = sqrt( (usno.e_dedeg/1000.0)^2.0 + (usno.e_radeg/1000.0)^2.0 )  ; in arcsec
err = err > 0.05   ; make sure ERR is not zero
;err = err > 0.01   ; make sure ERR is not zero


;----------------------------------------------------------
; ---- FITTING ----


; Initializing the parameters
dx = dblarr(8)
dy = dblarr(8)
theta = dblarr(8)
a = dblarr(8)
b = dblarr(8)

; Starting parameters
dx = [ -75.0d0, -25., 25., 75., -75, -25., 25., 75.  ]
dy = [ -15.0d0, -15.0, -15.0, -15.0, 15.0, 15.0, 15.0, 15.0] 
theta = dblarr(8)
a = [ 0.0d0 ,  7.4921890e-05 ,  3.1164492e-07 ,  3.4215511e-11,  -1.0261240e-11 ,  1.2030707e-11,  -4.6752405e-14 , -3.8975044e-23 ]
b = [ 0.0d0 , -3.0312892e-07,   7.4925892e-05,   4.3452297e-13 ,  1.7530428e-11,  -6.1183455e-12 , -4.3734693e-14 , -9.7695053e-23 ]

; Remember these
origdx = dx
origdy = dy

; Iterate,  each iteration run mpfit only fitting distortion, then
; with that distortion solution run mpfit only fitting the chip constants
; use the same mpfit function, but just hold different parameters fixed.


;####################################################
; 1.)  Fit tangent point, field center  
;      Hold everything else fixed  
;####################################################


;---------------------------------------------
; STEP 1:  find global x/y offset, theta=0
;---------------------------------------------

; Getting new fitted values
par = [ dx, dy, theta, a, b]
out = trans_coord(x,y,chip,par)
nzeta = reform(out[0,*])
neta = reform(out[1,*])

zetaoff = median(zeta-nzeta)
etaoff = median(eta-neta)
print,'Global offsets:'
print,'ZETAoff = ',zetaoff
print,'ETAoff = ',etaoff
print,''

a[0] = a[0]+etaoff
b[0] = b[0]+zetaoff
  
;---------------------------------------------
; STEP 2:  redo the gnomic conversion with a new field center
;---------------------------------------------
mnra2 = mnra+b[0]/cos(mndec/!radeg)
mndec2 = mndec+a[0]
  
ROTSPHCEN,usno.raj2000,usno.dej2000,mnra2,mndec2,zeta,eta,/gnomic
a[0] = 0.0
b[0] = 0.0


; ****** STARTING THE FITTING ITERATIONS *******

oldrms = 99.99
oldpar = par

rmsarr = fltarr(50)
pararr = fltarr(50,40)

flag = 0
count = 0
WHILE (flag ne 1) do begin

  print,'########################'
  print,'#  ITERATION = ',strtrim(count+1,2)
  print,'########################'
  print,''

  par = [ dx, dy, theta, a, b]
  origpar = par

  ;####################################################
  ; 2.)  Fit chip constants individually
  ;      Hold plate constants and tangent point fixed
  ;####################################################

  nbadchip = 0
  maxrms = 0.0
  maxchi = 0.0

  ; Loop through the chips
  for i=1,8 do begin

    ind = where(chip eq i,nind)

    if nind lt 40. then nbadchip++

    ; Are there any matched stars in this chip
    if (nind ge 20) then begin

      ; Now run MPFIT.PRO to get the best fit
      nstars = nind
      npar = 40

      ; Making the constraints structure
      parinfo = replicate({limited:[1,1],limits:[-0.5,0.5],fixed:0},npar)

      ; Chip constants terms
      ; dx,dy pixel constants
      parinfo[0:15].limits = [-200,200]     ; pixels
      ; theta
      parinfo[16:23].limits = [-200.0,200.0]  ; arcmin
      ; ALLOW CHIP CONSTANTS TO FLOAT  -  FOR THIS CHIP ONLY!!!!
      parinfo[0:23].fixed = 1
      parinfo[[i-1,i+7,i+15]].fixed = 0   ; the three terms for this chip
      parinfo[i-1].limits = [-15.,15.]+dx[i-1]
      parinfo[i+7].limits = [-15.,15.]+dy[i-1]

      ; Distortion terms
      ; a, zeta terms
      parinfo[24:31].limits = [-0.5,0.5]
      ; b, eta terms
      parinfo[32:39].limits = [-0.5,0.5]
      ;parinfo[24:39].fixed = 0
      parinfo[24:39].fixed = 1

      ; Initial guess
      ;par = dblarr(npar)
      par = [ dx, dy, theta, a, b]

      ; Bad parameters
      bad = where(par lt parinfo.limits[0],nbad)
      if nbad gt 0 then par[bad] = parinfo[bad].limits[0]
      bad = where(par gt parinfo.limits[1],nbad)
      if nbad gt 0 then par[bad] = parinfo[bad].limits[1]

      ; Other inputs
      ; Get all stars for this chip
      ind = where(chip eq i,nind)
      ftol = 1d-10   ;1d-10
      fa = {x:x[ind],y:y[ind],chip:chip[ind],zeta:zeta[ind],eta:eta[ind],err:err[ind]}

      ; FITTING
      func = 'trans_coord_dev'
      maxiter = 50L
      fpar = mpfit(func, par, functargs=fa, perror=perror,niter=iter,status=status,$
                   bestnorm=chisq, parinfo=parinfo, dof=dof, ftol=ftol, maxiter=maxiter, /quiet)

      ; Getting new fitted values
      out = trans_coord(x[ind],y[ind],chip[ind],fpar)
      nzeta = reform(out[0,*])
      neta = reform(out[1,*])

      ; Error information
      resid = sqrt( (zeta[ind]-nzeta)^2.0 + (eta[ind]-neta)^2.0 )*3600.0d0  ; in arcsec
      ;rms = stdev(resid)
      rms = sqrt(mean(resid^2.0))
      sigpar = perror * sqrt(chisq/dof)
      chi = sqrt(chisq/dof)

      ; New parameter values
      dx = fpar[0:7]
      dy = fpar[8:15]
      theta = fpar[16:23]
      ;a = fpar[24:31]   ; These are fixed for this part
      ;b = fpar[32:39]

      ; MPFIT ERROR
      if (status lt 1) then begin
        print,'MPFIT CHIP ',strtrim(i,2),' PROBLEM - Status=',strtrim(status,2)
        ;stop
      endif

      ; Printing
      print,'Chip=',strtrim(i,2),' Chi=',stringize(chi,ndec=1),' RMS=',stringize(rms,ndec=2),$
            '    dx=',stringize(dx[i-1],ndec=2,len=7),' dy=',stringize(dy[i-1],ndec=2,len=7),$ 
            ' theta=',stringize(theta[i-1],ndec=2,len=7),'  Nstars=',strtrim(nind,2)

      maxrms = maxrms > rms
      maxchi = maxchi > chi

    ; Not enough stars in the this chip
    endif else begin ; stars in this chip

      ; Keep the original values
      dx[i-1] = origdx[i-1]
      dy[i-1] = origdy[i-1]
      theta[i-1] = 0.0

      print,'Chip=',strtrim(i,2),' Chi=9.9',' RMS=9.99',$
            '    dx=',stringize(dx[i-1],ndec=2,len=7),' dy=',stringize(dy[i-1],ndec=2,len=7),$ 
            ' theta=',stringize(theta[i-1],ndec=2,len=7),'  Nstars=',strtrim(nind,2),'    TOO FEW STARS'

      maxrms = 9.99
      maxchi = 9.99

    endelse

  end ; for i

  ;stop

  ; ############################
  ; Find more matches
  ; NOT the FIRST time
  ;
  ; MISSING CHIPS:
  ; If there are missing chips then don't try to find matches for them until iteration=5
  ; We need a good solution first.  Make sure the matching radius is large when first
  ; trying to match the missing chips.
  ; ############################
  if (count ge 1) then begin

    print,''
    print,'Finding more matches'

    ; Get good USNO stars and transform to zeta/eta
    errlim = 300.  ;400.
    agd = where(allusno.e_radeg lt errlim and allusno.e_dedeg lt errlim,nagd)
    usno2 = allusno[agd]
    ROTSPHCEN,usno2.raj2000,usno2.dej2000,mnra2,mndec2,zeta2,eta2,/gnomic  

    ; Get good PHOT stars and transform to zeta/eta
    pgd = where(allphot.ierr lt 0.1 and allphot.merr lt 0.1 and allphot.chi lt 1.5 and abs(allphot.sharp) lt 1.5,npgd)
    phot2 = allphot[pgd]

    ; Do we have any missing chips
    ; WAIT UNTIL WE HAVE A DECENT FIT FIRST
    ; Don't include them until Iteration 4
    ui = uniq(chip,sort(chip))
    chp = chip[ui]

    ; Remove stars in missing chips
    if n_elements(chp) lt 8 and count lt 4 then begin
      print,'LEAVING OUT THE MISSING CHIPS'

      missing = lindgen(8)+1
      remove,chp-1,missing
      nmissing = n_elements(missing)
      for m=0,nmissing-1 do begin
        bd = where(phot2.chip eq missing[m],nbd)
        if nbd gt 0 then remove,bd,phot2
      end
    endif
    if n_elements(chp) lt 8 and count ge 4 then print,'ADDING IN THE MISSING CHIPS'

    par = [ dx, dy, theta, a, b]
    out = trans_coord(phot2.x,phot2.y,phot2.chip,par) 
    nzeta2 = reform(out[0,*])
    neta2 = reform(out[1,*])

    ; Match them
    lim = 1.0     ; in arcsec
    if (count ge 3) then lim = 0.5 > 2.0*rms                  ; at later times lower the matching radius
    if n_elements(chp) lt 8 and count ge 4 then lim = lim > 4.0  ; Missing chips, use a large matching radius
    if nbadchip gt 0 then lim = lim > 4.0                     ; chips with few matched stars, use a large radius
    if maxrms gt 1.0 then lim = lim > 3.0                     ; bad rms
    if maxchi gt 6.0 then lim = lim > 3.0                     ; really bad chi
    if (maxchi gt 4.0 and maxrms gt 0.7) then lim = lim > 2.0    ; bad chi
    lim = lim < 5.0                                           ; 5.0 maximum
    if (count lt 4 and maxchi lt 3.5) then lim = lim < 1.5    ; 1.5 maximum for initial iterations
    
    print,'Matching radius = ',stringize(lim,ndec=2),' arcsec'
    SRCMATCH,zeta2,eta2,nzeta2,neta2,lim,ind1,ind2,/sph

    ; Matched structures
    usno3 = usno2[ind1]
    phot3 = phot2[ind2]

    ; Make new variables
    zeta = zeta2[ind1]
    eta = eta2[ind1]
    x  = phot2[ind2].x
    y = phot2[ind2].y
    chip = phot2[ind2].chip

    err = sqrt( (usno2[ind1].e_dedeg/1000.0)^2.0 + (usno2[ind1].e_radeg/1000.0)^2.0 )  ; in arcsec
    err = err > 0.05   ; make sure ERR is not zero
    ;err = err > 0.01   ; make sure ERR is not zero

    print,strtrim(n_elements(ind1),2),' Matches found'

    ;stop

  end


  ;####################################################
  ; 3.)  Fit plate constants (distortion)
  ;      Hold everything else fixed
  ;####################################################

  print,''

  ; Now run MPFIT.PRO to get the best fit
  nstars = n_elements(x)
  npar = 40
  ; Making the constraints structure
  parinfo = replicate({limited:[1,1],limits:[-0.5,0.5],fixed:0},npar)

  ; Chip constants terms
  ; dx,dy pixel constants
  parinfo[0:15].limits = [-100,100]     ; pixels
  ; theta
  parinfo[16:23].limits = [-200.0,200.0]  ; arcmin
  ; Hold all chip constants fixed
  parinfo[0:23].fixed = 1

  ; Distortion terms
  ; a, zeta terms
  parinfo[24:31].limits = [-0.5,0.5]
  ; b, eta terms
  parinfo[32:39].limits = [-0.5,0.5]
  ; DISTORTION terms are allowed to float
  parinfo[24:39].fixed = 0

  ; AFTER THE FIRST PASS ALLOW ONLY LINEAR SHIFTS
  ; the 1st three terms are allowed to float
  ;if (count ge 1) then begin
  ;  parinfo[24:26].fixed = 0   ; 1st three terms can float
  ;  parinfo[27:31].fixed = 1   ; higher orders are fixed
  ;  parinfo[32:34].fixed = 0   ; 1st three terms can float
  ;  parinfo[35:39].fixed = 1   ; higher orders are fixed
  ;endif

  ; Initial guess
  par = [ dx, dy, theta, a, b]

  ; Bad parameters
  bad = where(par lt parinfo.limits[0],nbad)
  if nbad gt 0 then par[bad] = parinfo[bad].limits[0]
  bad = where(par gt parinfo.limits[1],nbad)
  if nbad gt 0 then par[bad] = parinfo[bad].limits[1]

  ; Other inputs
  ftol = 1d-10  ;1d-10
  fa = {x:x,y:y,chip:chip,zeta:zeta,eta:eta,err:err}

  ; FITTING
  print,'Fitting global distortion terms'
  func = 'trans_coord_dev'
  maxiter = 50L
  ;maxiter = 300L
  fpar = mpfit(func, par, functargs=fa, perror=perror,niter=iter,status=status,$
               bestnorm=chisq, parinfo=parinfo, dof=dof, ftol=ftol, maxiter=maxiter, /quiet)

  ; Getting new fitted values
  out = trans_coord(x,y,chip,fpar)
  nzeta = reform(out[0,*])
  neta = reform(out[1,*])

  ; Error information
  resid = sqrt( (zeta-nzeta)^2.0 + (eta-neta)^2.0 )*3600.0d0  ; in arcsec
  ;rms = stdev(resid)
  rms = sqrt(mean(resid^2.0))
  sigpar = perror * sqrt(chisq/dof)
  chi = sqrt(chisq/dof)

  ; Plot the final residuals
  if n_elements(zeta) gt 5000. then ps=3 else ps=1
  if not keyword_set(noplot) then $
    plot_astrom_resid,zeta,eta,nzeta,neta,tit=field+'  RMS='+stringize(rms,ndec=2),ps=ps

  ; New parameter values
  ;dx = fpar[0:7]         ; held fixed in this part
  ;dy = fpar[8:15]
  ;theta = fpar[16:23]
  a = fpar[24:31]
  b = fpar[32:39]


  ; MPFIT ERROR
  if (status lt 1) then begin
    print,'MPFIT CHIP DISTORTION PROBLEM - Status=',strtrim(status,2)
    ;stop
  endif

  print,'Distortion   Chi=',stringize(chi,ndec=1),' RMS=',stringize(rms,ndec=2)
  print,'dx   =',dx
  print,'dy   =',dy
  print,'theta=',theta
  print,'a    =',a
  print,'b    =',b

 
  ; Fix chip constants again, but allow only linear changes
  ; to the plate constants.


  ;#########################################
  ; If good enough then end iteration
  ;#########################################
  ; iterate 2 or 3 times
  ; compare to previous solution or
  ; change in gap size is <0.01 pixels or change in rotation angle is <1"

  delx = abs(origpar[0:7]-dx)
  dely = abs(origpar[8:15]-dy)
  delth = abs(origpar[16:31]-theta)

  maxdelx = max(delx)
  maxdely = max(dely)
  maxdelth = max(delth)

  print,''
  print,'CHANGES'
  print,'MAX(delx)  = ',strtrim(maxdelx,2),' pixels'
  print,'MAX(dely)  = ',strtrim(maxdely,2),' pixels'
  print,'MAX(delth) = ',strtrim(maxdelth,2),' arcmin'
  print,''

  ; Final solution for this iteration
  par = [ dx, dy, theta, a, b]

  ; Once max(delx) < 0.05 pixels  ;0.01 pixels
  ;  and max(dely) < 0.05 pixels  ;0.01 pixels
  ;  and max(delth) < 5"          ;1"
  ; Then STOP

  if maxdelx lt 0.05 and maxdely lt 0.05 and maxdelth le (5./60.) and count gt 2 then flag=1

  ; Putting solution into array
  pararr[count,*] = par
  rmsarr[count] = rms

  ;; Saving solution
  ;oldrms = rms
  ;oldpar = par

  ; Iterate
  count = count + 1

  ;stop

END  ; while

; Field center coordinates
fcenter = [ mnra2, mndec2]

;; Finding the best solutin
pararr = pararr[0:count-1,*]
rmsarr = rmsarr[0:count-1]
;best = first_el(minloc(rmsarr))
;par = reform(pararr[best,*])
;
;; Getting final values
;out = trans_coord(x,y,chip,par)
;nzeta = reform(out[0,*])
;neta = reform(out[1,*])

; MAYBE I SHOULD SAVE THE MATCHED STARS TO A FILE.
outusno = usno3
outphot = phot3

; Plot the final residuals
if n_elements(zeta) gt 5000. then ps=3 else ps=1
if not keyword_set(noplot) then $
  plot_astrom_resid,zeta,eta,nzeta,neta,title=field+'  RMS='+stringize(rms,ndec=2),ps=ps

; Save the plot
if keyword_set(saveplot) then begin
  if keyword_set(psfile) then file=psfile else file = field+'_resid'
  ps_open,file,/color,thick=3
  plot_astrom_resid,zeta,eta,nzeta,neta,title=field+'  RMS='+stringize(rms,ndec=2)
  ps_close
endif

;stop

if keyword_set(stp) then stop

end
