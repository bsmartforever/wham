;+
;
; GRAB_STARS
;
; This function can be used to get star positions like
; IMEXAMINE in IRAF
;
; INPUTS:
;  filein  The input FITS file
;  fileout The name of the output file of X/Y coordinates
;  listin=listin  The name of a list of stars to select coordinates for
;
; OUTPUTS:
;  The X/Y coordinates of the flux-centered stars are printed
;  to the output file
;
; Written by D.Nidever  Dec. 2006
;-


function get_subim,im,xind,yind,hwidth,sky

; This function returns a subimage
;  hwidth is the half-width.  Total width = 2*hwidth+1

xind2 = round(xind)
yind2 = round(yind)

; Getting the image size
sz = size(im)
nx = sz(1)
ny = sz(2)

; Setting the sky background
if n_elements(sky) eq 0 then sky=0.0

; Initializing the subimage
subim = fltarr(2*hwidth+1,2*hwidth+1) + sky

; Checking that we're getting any part of the image
; The center can be off the edge
if ( (xind2+hwidth) ge 0 ) and ( (xind2-hwidth) le (nx-1) ) $
   and ( (yind2+hwidth) ge 0 ) and ( (yind2-hwidth) le (ny-1) ) then begin

  ; Indices for the big image
  xbg0 = (xind2-hwidth) > 0
  xbg1 = (xind2+hwidth) < (nx-1)
  ybg0 = (yind2-hwidth) > 0
  ybg1 = (yind2+hwidth) < (ny-1)

  ; Indices for the subimage
  xsm0 = hwidth+xbg0-xind2
  xsm1 = hwidth+xbg1-xind2
  ysm0 = hwidth+ybg0-yind2
  ysm1 = hwidth+ybg1-yind2

  ; Getting part of the image
  subim(xsm0:xsm1,ysm0:ysm1) = im(xbg0:xbg1,ybg0:ybg1)

end

;stop

return,subim

end

;--------------------------------------------------------

pro get_fluxcenter,subim,xind,yind

; Calculate the flux-weighted center        

; Center-of-Mass like centroid.  Weight by flux
; Add up all position along columns since they will have the same
; X value. Similar for Y
sz = size(subim)
nx = sz(1)
ny = sz(2)
mask = float(subim ge 0.0)     ; mask out negative pixels
xind = total( total(subim*mask,2)*findgen(nx) )/total(subim*mask)     
yind = total( total(subim*mask,1)*findgen(ny) )/total(subim*mask)
 
end

;--------------------------------------------------------

pro grab_stars,filein,fileout,listin=listin,log=log

; Use this to get star positions in an image

; No inputs
if n_params() eq 0 then begin
  print,'Syntax - grab_stars,filein,fileout,listin=listin'
  return
endif

; No filein given
if n_elements(filein) eq 0 then begin
  filein=''
  print,'ENTER INPUT FILENAME:'
  read,filein
endif


; A list file was input
if strmid(filein[0],0,1) eq '@' then begin

  inp = strmid(filein[0],1)

  ; Loading the files
  readcol,inp,list,format='A',/silent
  nlist = n_elements(list)

endif else begin

  ; Probably an array of filenames
  if n_elements(filein) gt 1 then begin
    list = filein
    nlist = n_elements(list)

  ; A globbed list or only one file
  endif else begin
    list = file_search(filein)
    nlist = n_elements(list)
  endelse

endelse

; More than 1 file input
if nlist gt 1 then begin

  print,'#################################################'
  print,'LIST INPUT: RUNNING GRAB_STARS.PRO ON ',strtrim(nlist,2),' FRAMES'
  print,'#################################################'

  ; fileout input
  if n_elements(fileout) gt 0 then $
    for i=0,nlist-1 do grab_stars,list[i],fileout,listin=listin

  ; No fileout input
  if n_elements(fileout) eq 0 then $
    for i=0,nlist-1 do grab_stars,list[i],listin=listin

  return

endif


; No fileout given
if n_elements(fileout) eq 0 then begin
  file = file_basename(filein,'.fits')
  fileout=file+'.coord'
  ;fileout='grab_stars.out'
endif


; Check the filein
filein2 = filein
test = file_test(filein2)

; Try adding a .fits extension
if test eq 0 then filein2=filein2+'.fits'
test = file_test(filein2)

if test eq 0 then begin
  print,'FILE ',filein,' DOES NOT EXIST'
  return
endif

; Print the file name
print,''
print,'FILE = ',filein2
print,''

; Reading the file
fits_read,filein2,im,head

sz = size(im)
nx = sz(1)
ny = sz(2)

; Image statistics
med = median(im)
std = stdev(im)
resistant_mean,im,3.0,mean,stdmn
std = stdmn*sqrt(n_elements(im))


; Open a window with the correct size
ratio = ny/float(nx)
; NX > NY
if ratio le 1.0 then begin
  xsize = 1000.; 640.
  ysize = round(xsize*ratio)
; NY > NX
endif else begin
  ysize = 1000. ;640.
  xsize = round(ysize/ratio)*1.2   ; correct for plot being a bit smooshed in the x-axis
endelse
window,10,xsize=xsize,ysize=ysize,xpos=1000


; DISPLAY the image
if !d.name eq 'X' then device,decomposed=0
loadct,0,/silent
display,im,min=med,max=med+5.0*std,log=log,xtit='X',ytit='Y',tit=filein


; USE THE INPUT LIST OF STARS
loadct,39,/silent
IF keyword_set(listin) THEN BEGIN

  ; Get the star coordinates using the header

  test = file_test(listin)
  if test eq 0 then begin
    print,'FILE ',listin,' DOES NOT EXIST'
    return
  endif

  ; Load the list
  readcol,listin,name,ra,dec,format='A,A,A',comment='#',/silent

  name2 = strtrim(name,2)
  ra2 = sexig2ten(ra)       ; sexig2ten can deal with regular numbers as well
  dec2 = sexig2ten(dec)

  ; Getting the X/Y values
  ctype1 = sxpar(head,'CTYPE1')
  tnx = stregex(ctype1,'TNX',/boolean,/fold_case)

  ; WCS is not TNX
  if tnx eq 0 then begin
    adxy,head,ra2*15.,dec2,xout,yout        ; Needs ra/dec in DEGREES

  ; WCS IS TNX
  endif else begin
    ra3 = ra2*15.0d0/!radeg
    dec3 = dec2/!radeg
    wcs = hdr2wcstnx(head)
    wcstnx_rd2xy, ra3, dec3, wcs, xout, yout  ; Needs ra/dec in RADIANS
  endelse

  ; Print message
  print,'If a star is off the frame or you do not want to include it'
  print,'then click outside the boundary and its coordinates will'
  print,'be set to (X,Y)=(9999.99,9999.99)'
  print,''

  ; Initializing the position arrays
  nxout = n_elements(xout)
  xarr = fltarr(nxout)
  yarr = fltarr(nxout)
  xarr2 = fltarr(nxout)
  yarr2 = fltarr(nxout)

  phi = findgen(100)/99.*2.0*!dpi

  ; Now prompt to click on each star in the right order
  for i=0,nxout-1 do begin
   
    size = 30.0

    ; Overplot all the star outlines in GREEN
    for j=0,nxout-1 do begin
      xx = size*sin(phi)
      yy = size*cos(phi)
      oplot,xout[j]+xx,yout[j]+yy,co=150
      xyouts,xout[j],yout[j]+size+20,name2[j],charsize=1.5,co=150,align=0.5
    end

    ; Print the message
    print,'PLEASE CLICK ON STAR: ',name2[i]

    ; Overplot the current star in RED
    oplot,xout[i]+xx,yout[i]+yy,co=250
    xyouts,xout[i],yout[i]+size+20,name2[i],charsize=1.5,co=250,align=0.5

    ; Get the cursor click
    cursor,x,y
    wait,0.2
    xarr[i] = x
    yarr[i] = y

    ; Print the clicked cursor position
    print,'     X        Y'
    print,format='(F10.3,F10.3,A25)',xout[i],yout[i],' ESTIMATED POSITION'
    print,format='(F10.3,F10.3,A25)',x,y,' CLICKED POSITION'

    ; Overplot the click
    oplot,[x],[y],ps=7,sym=2

    ; INSIDE the boundary
    if (x ge 0 and x le (nx-1) and y ge 0 and y le (ny-1)) then begin

      ; Getting the flux-weighted centroided position
      off = 15.0
      subim = get_subim(im,xarr[i],yarr[i],off,med)
      get_fluxcenter,subim,xind,yind   ; relative to bottom-left
      xoff = xind-off
      yoff = yind-off
      xarr2[i] = xarr[i]+xoff
      yarr2[i] = yarr[i]+yoff

      ; Printing the centroided position
      print,format='(F10.3,F10.3,A25)',xarr2[i],yarr2[i],' CENTROIDED POSITION'
      print,format='(F10.3,F10.3,A25)',xoff,yoff,' OFFSET'
      print,''

    ; OUTSIDE the boundary
    ; If outside the bounds then set x,y = 9999.99, 9999.99
    endif else begin

      xarr2[i] = 9999.99
      yarr2[i] = 9999.99
    
      ; Printing the centroided position  
      print,format='(F10.3,F10.3,A25)',xarr2[i],yarr2[i],' OFF THE IMAGE'
      print,''

    endelse  ; inside/outside the boundary

  end ; loop through each list star

  ; Write the file
  ; Print to a file
  print,'PRINTING TO ',fileout
  openw,unit,/get_lun,fileout

  namelen = max(strlen(name2))
  ralen = max(strlen(ra))
  declen = max(strlen(dec))
  format = '(A'+strtrim(namelen+2,2)+',F10.3,F10.3)'
  ;format = '(A'+strtrim(namelen+2,2)+',A'+strtrim(ralen+2,2)+',A'+strtrim(declen+2,2)+',F10.3,F10.3)'

  for i=0,nxout-1 do begin
    printf,unit,format=format,name2[i],xarr2[i],yarr2[i]
    ;printf,unit,format=format,name2[i],ra[i],dec[i],xarr2[i],yarr2[i]
  end
  
  close,unit
  free_lun,unit

ENDIF ELSE BEGIN


  print,'CLICK OUTSIDE THE BOUNDARY TO END'

  ; Select the stars
  psym8
  clicker,xarr,yarr,/overplot

  ; Get the maxima around the stars
  nxarr = n_elements(xarr)

  xarr2 = xarr*0.
  yarr2 = yarr*0.

  ; Loop through the stars
  for i=0,nxarr-1 do begin

    x = xarr[i]
    y = yarr[i]

    ; INSIDE the boundary
    if (x ge 0 and x le (nx-1) and y ge 0 and y le (ny-1)) then begin

      off = 10.0
      subim = get_subim(im,xarr[i],yarr[i],off,med)
      get_fluxcenter,subim,xind,yind   ; relative to bottom-left
      xarr2[i] = xarr[i]-off+xind
      yarr2[i] = yarr[i]-off+yind

    ; OUTSIDE the boundary
    endif else begin

      xarr2[i] = 9999.99
      yarr2[i] = 9999.99

    endelse ; inside/outside the boundary

  end ; loop through the stars

  ; Print to a file
  print,'PRINTING TO ',fileout
  openw,unit,/get_lun,fileout

  print,''
  print,'CENTROIDED POSITIONS'
  print,'----------------------------'
  print,'   NUM      X          Y     '
  print,'----------------------------'

  for i=0,nxarr-1 do begin
    printf,unit,format='(I5,F10.3,F10.3)',i+1,xarr2[i],yarr2[i]
    print,format='(I5,F10.3,F10.3)',i+1,xarr2[i],yarr2[i]
  end

  print,'----------------------------'
  
  close,unit
  free_lun,unit

ENDELSE

if keyword_set(stp) then stop

end
