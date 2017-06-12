pro head_adxy,head,a,d,x,y,degree=degree

; This program uses the WCS in a FITS header to convert
; RA/DEC values (in hours and degrees) to X/Y pixels values.
; Vectors can be input.

; Not enough inputs
if n_elements(head) eq 0 then begin
  print,'Syntax - head_adxy,head,a,d,x,y,degree=degree'
  return
endif

ctype1 = sxpar(head,'CTYPE1')
tnx = stregex(ctype1,'TNX',/boolean,/fold_case)

; WCS is not TNX
if tnx eq 0 then begin
  adxy,head,a,d,x,y        ; Needs ra/dec in degrees

; WCS IS TNX
endif else begin
  if keyword_set(degree) then a2=a/!radeg else a2=a*15.0d0/!radeg
  d2 = d/!radeg   
  wcs = hdr2wcstnx(head)
  wcstnx_rd2xy, a2, d2, wcs, x, y    ; Needs ra/dec in RADIANS
endelse

; Return scalars if only one element
if n_elements(x) eq 1 then x=x[0]
if n_elements(y) eq 1 then y=y[0]

end

;---------------------------

pro head_xyad,head,x,y,a,d,degree=degree

; This program uses the WCS in a FITS header to convert
; X/Y pixel values to RA/DEC values (in hours and degrees).
; Vectors can be input.

; Not enough inputs
if n_elements(head) eq 0 then begin
  print,'Syntax - head_xyad,head,x,y,a,d,degree=degree'
  return
endif

ctype1 = sxpar(head,'CTYPE1')
tnx = stregex(ctype1,'TNX',/boolean,/fold_case)

; WCS is not TNX
if tnx eq 0 then begin
  xyad,head,x,y,a,d

; WCS IS TNX
endif else begin
  wcs = hdr2wcstnx(head)
  wcstnx_xy2rd, x, y, wcs, a2, d2     ; returns RA/DEC in radians!
  a = a2*!radeg
  d = d2*!radeg
endelse

if not keyword_set(degree) then a=a/15.0

; Return scalars if only one element
if n_elements(a) eq 1 then a=a[0]
if n_elements(d) eq 1 then d=d[0]

end

;---------------------------

pro get_usno,head,usno,stp=stp

; This gets USNO data for the region covering
; the image given the header

; Not enough inputs
if n_elements(head) eq 0 then begin
  print,'Syntax - get_usno,head,usno,stp=stp'
  return
endif

nx = sxpar(head,'NAXIS1')
ny = sxpar(head,'NAXIS2')

xb = [0.,nx-1.,nx-1.,0.]
yb = [0.,0.,ny-1.,ny-1.]
     
; Center coordinates
head_xyad,head,nx/2.,ny/2.,cen_ra,cen_dec,/degree

; Boundary coordinates
head_xyad,head,xb,yb,rab,decb,/degree

off = sphdist(cen_ra,cen_dec,rab,decb,/degree)
dis = 60.*max(off)+10.         ; add a margin of 10 arcmin

; Get the USNO-B1 stars within this boundary (+some margin)
; There is a /allcolumn option to download more data
usno = queryvizier('USNO-B1',[cen_ra,cen_dec],dis,/canada)

; There was an error
if n_elements(usno) lt 2 then begin
  ntags = n_tags(usno)
  if ntags eq 0 then begin
    print,'NO USNO SOURCES'
    return
  endif
endif

; Empose a rectangular boundary
rar = [min(rab),max(rab)]
decr = [min(decb),max(decb)]
decoff = 0.10
raoff = decoff/cos(cen_dec/!radeg)
gd = where(usno.raj2000 ge rar[0]-raoff and usno.raj2000 le rar[1]+raoff and $
           usno.dej2000 ge decr[0]-decoff and usno.dej2000 le decr[1]+decoff,ngd)
usno_orig = usno
usno = usno[gd]

; Convert RA/DEC coordinates for USNO to X/Y pixel values
head_adxy,head,usno.raj2000,usno.dej2000,x,y,/degree
add_tag,usno,'X',0.0,usno
add_tag,usno,'Y',0.0,usno
usno.x = x
usno.y = y

; Adding ID tag
add_tag,usno,'ID',0L,usno
nusno = n_elements(usno)
usno.id = findgen(nusno)+1.0   ; start at 1

; Adding fake CHI and SHARP tags
add_tag,usno,'CHI',0.0,usno
add_tag,usno,'SHARP',0.0,usno
usno.chi = 1.0
usno.sharp = 0.0

; Getting average Rmag, Bmag 
rmag = median([[usno.r1mag],[usno.r2mag]],dim=2,/even)
bmag = median([[usno.b1mag],[usno.b2mag]],dim=2,/even)
rmagbd = where(finite(rmag) eq 0,nrmagbd)      ; replace NaNs with 99.9999
if nrmagbd gt 0 then rmag[rmagbd] = 99.9999
bmagbd = where(finite(bmag) eq 0,nbmagbd)
if nbmagbd gt 0 then bmag[bmagbd] = 99.9999

; Getting Err, Berr
rerr = abs(usno.r1mag-usno.r2mag)
berr = abs(usno.b1mag-usno.b2mag)
rerrbd = where(finite(rerr) eq 0,nrerrbd)      ; replace NaNs with 0.2
if nrerrbd gt 0 then rerr[rerrbd] = 0.2
if nrmagbd gt 0 then rerr[rmagbd] = 9.9999     ; bad mag -> bad error
berrbd = where(finite(berr) eq 0,nberrbd)
if nberrbd gt 0 then berr[berrbd] = 0.2
if nbmagbd gt 0 then berr[bmagbd] = 9.9999     ; bad mag -> bad error

; Adding the BMAG, RMAG, BERR, RERR tags
add_tag,usno,'BMAG',0.0,usno
add_tag,usno,'RMAG',0.0,usno
add_tag,usno,'BERR',0.0,usno
add_tag,usno,'RERR',0.0,usno
usno.bmag = bmag
usno.rmag = rmag
usno.berr = berr
usno.rerr = rerr

if keyword_set(stp) then stop

end

;---------------------------

pro load_phot,filename,head,phot,stp=stp

; This loads the DAOPHOT photometry file

; Not enough inputs
if n_elements(filename) eq 0 or n_elements(head) eq 0 then begin
  print,'Syntax - load_phot,filename,head,phot,stp=stp'
  return
endif

test = file_test(filename)
if test eq 0 then begin
  print,'FILE ',filename,' DOES NOT EXIST'
  phot=-1
  return
endif

; Is this an ALS or PHOT file
line1='' & line2='' & line3='' & line4=''
openr,unit,/get_lun,filename
readf,unit,line1
readf,unit,line2
readf,unit,line3
readf,unit,line4
close,unit
free_lun,unit

; This is an ALS file 
arr1 = strsplit(line1,' ',/extract)
if arr1[0] eq 'NL' and strtrim(line3,2) eq '' then begin

  arr4 = strsplit(line4,' ',/extract)
  narr4 = n_elements(arr4)
  nmag = (narr4-5)/2

  fieldtypes = [3,lonarr(narr4-1)+4]
  fieldnames = ['ID','X','Y']
  for i=1,nmag do fieldnames = [fieldnames,'MAG'+strtrim(i,2),'MERR'+strtrim(i,2)]
  fieldnames = [fieldnames,'CHI','SHARP']

  ; Read the photometry file
  phot = importascii(filename,skip=3,fieldnames=fieldnames,fieldtypes=fieldtypes,/noprint)

; This is a PHOT file
endif else begin

  ; Read the photometry file
  phot = importascii(filename,/header,/noprint)

endelse


; Add RA/DEC tags
add_tag,phot,'RA0',0.0d0,phot
add_tag,phot,'DEC0',0.0d0,phot
add_tag,phot,'RA',0.0d0,phot
add_tag,phot,'DEC',0.0d0,phot
   
; Convert X/Y coordinates for our DAOPHOT photometry stars
; to RA/DEC using the WCS in the FITS header
head_xyad,head,phot.x,phot.y,ra,dec,/degree
phot.ra0 = ra      ; Initial coordinates
phot.dec0 = dec
phot.ra = ra       ; Current best and final coordinates
phot.dec = dec

if keyword_set(stp) then stop

end


;---------------------------

function load_daohead,file,stp=stp

; This returns the header from a DAPHOT style ASCII file
; i.e. the first two lines

; Not enough inputs
if n_elements(file) eq 0 then begin
  print,'Syntax - head = load_daohead(filename,stp=stp)'
  return,-1
endif

; Checking that the file exists
test = file_test(file)
if test eq 0 then begin
  print,'FILE ',file,' DOES NOT EXIST'
  return,-1
end  

nlines = file_lines(file)

; Not enough lines
if nlines lt 2 then begin
  print,'FILE ',file,' DOES NOT HAVE ENOUGH LINES'
  return,-1
endif

; Loading the header
line1='' & line2=''
openr,unit,/get_lun,file
readf,unit,line1
readf,unit,line2
close,unit
free_lun,unit

head = strarr(2)
head[0] = line1
head[1] = line2

if keyword_set(stp) then stop

return,head

end

;---------------------------

pro write_phot,str,outfile,radec=radec,nohead=nohead,$
    daohead=daohead,colhead=colhead,both=both,stp=stp

; This writes a photometry structure (USNO or DAOPHOT) to
; a DAOPHOT like output file
; The default is to print out X/Y, but if /radec is set
; then the RA/DEC are output.
;
; Allow three header options: (1) no header, (2) header of 
; column names (for importascii), (3) DAOPHOT-style header
; /nohead   No header at all
; /colhead  Header with column nmaes
; =daohead  DAOPHOT-style header.  daohead must contain the
;           two-element string header otherwise blanks will
;           be written
; The default is /colhead
; /both print pixels and ra/dec coordinates

; Not enough inputs
if n_elements(str) eq 0 or n_elements(outfile) eq 0 then begin
  print,'Syntax - write_phot,str,outfile,radec=radec,nohead=nohead,'
  print,'                    daohead=daohead,colhead=colhead,stp=stp'
  return
endif

if n_elements(daohead) eq 0 and n_elements(nohead) eq 0 then colhead=1
if keyword_set(colhead) then headcase=0
if keyword_set(daohead) then headcase=1
if keyword_set(nohead) then headcase=2

; Opening the file
openw,unit,/get_lun,outfile


; USNO or DAOPHOT structure??
tags = tag_names(str)
us = where(tags eq 'RAJ2000',nus)

if keyword_set(radec) then both=0

;------------------------------
; USNO structure
;------------------------------
if (nus gt 0) then begin

  ; Header
  case headcase of
         
    ; COLHEAD   
    0: begin

         ; RA/DEC coordinates
         if keyword_set(radec) then begin
           fields = ['ID','RA','DEC','BMAG','BERR','RMAG','RERR','CHI','SHARP']
           printf,unit,format='(2X,A5,2A12,6A9)',fields
         endif

         ; BOTH X/Y and RA/DEC coordinates  
         if keyword_set(both) then begin
           fields = ['ID','X','Y','RA','DEC','BMAG','BERR','RMAG','RERR','CHI','SHARP']
           printf,unit,format='(2X,A5,2A9,2A12,6A9)',fields
         endif

         ; X/Y pixel coordinates
         if not keyword_set(radec) and not keyword_set(both) then begin
           fields = ['ID','X','Y','BMAG','BERR','RMAG','RERR','CHI','SHARP']
           printf,unit,format='(2X,A5,2A9,6A9)',fields
         endif

       end

    ; DAOPHOT-style header
    1: begin
         nhead = n_elements(daohead)

         ; Regular header input
         if nhead eq 2 then begin
           printf,unit,daohead[0]
           printf,unit,daohead[1]
           printf,unit,''
         ; Non-regular header, printing blanks
         endif else begin
           print,'WRITE_PHOT.PRO ERROR - Non-regular DAOPHOT header input. Printing blanks.'
           printf,unit,'' 
           printf,unit,''
           printf,unit,'' 
         endelse
       end

    ; NO HEADER
    2: begin
         ; Don't need to do anything
       end
        
  endcase  ; header

  nstr = n_elements(str)

  ; Printing RA/DEC coordinates
  if keyword_set(radec) then begin

    ; Print the data
    for i=0.,nstr-1 do begin
      printf,unit,format='(2X,I5,2F12.6,6F9.4)',str[i].id,str[i].raj2000,str[i].dej2000,$
             str[i].bmag,str[i].berr,str[i].rmag,str[i].rerr,str[i].chi,str[i].sharp
    end

  endif ; RA/DEC coordinates

  ; Printing BOTH X/Y and RA/DEC coordinates
  if keyword_set(both) then begin

    ; Print the data
    for i=0.,nstr-1 do begin
      printf,unit,format='(2X,I5,2F9.3,2F12.6,6F9.4)',str[i].id,str[i].x,str[i].y,$
             str[i].raj2000,str[i].dej2000,str[i].bmag,str[i].berr,str[i].rmag,$
             str[i].rerr,str[i].chi,str[i].sharp
    end

  endif ; both X/Y and RA/DEC coordinates  

  ; Printing X/Y pixels coordinates
  if not keyword_set(radec) and not keyword_set(both) then begin

    ; Print the data
    for i=0.,nstr-1 do begin
      printf,unit,format='(2X,I5,2F9.3,6F9.4)',str[i].id,str[i].x,str[i].y,$
             str[i].bmag,str[i].berr,str[i].rmag,str[i].rerr,str[i].chi,str[i].sharp
    end

  endif ; X/Y coordinates


;------------------------------
; DAOPHOT structure
;------------------------------
endif else begin

  nstr = n_elements(str)
          
  ; Figure out which tags to print
  tags = tag_names(str)
  ntags = n_tags(str)
  ; Assuming that RA,DEC are first tags after CHI/SHARP
  hi = where(stregex(tags,'RA',/boolean,/fold_case) eq 1,nhi)
  hi = hi[0]
  if nhi eq 0 then hi=ntags
  nphotags = hi-5

  ; Header
  case headcase of

    ; COLHEAD
    0: begin

         ; RA/DEC coordinates
         if keyword_set(radec) then begin
           fields = ['ID','RA','DEC']
           fields = [fields,tags[3:nphotags+2]]
           fields = [fields,'CHI','SHARP']
           format = '(2X,A5,2A12,'+strtrim(nphotags+2,2)+'A9)'
           printf,unit,format=format,fields
         endif

         ; BOTH X/Y and RA/DEC coordinates
         if keyword_set(both) then begin
           fields = ['ID','X','Y','RA','DEC']
           fields = [fields,tags[3:nphotags+2]]
           fields = [fields,'CHI','SHARP']
           format = '(2X,A5,2A9,2A12,'+strtrim(nphotags+2,2)+'A9)'
           printf,unit,format=format,fields
         endif 
        
         ; X/Y pixel coordinates
         if not keyword_set(radec) and not keyword_set(both) then begin
           fields = ['ID','X','Y']
           fields = [fields,tags[3:nphotags+2]]
           fields = [fields,'CHI','SHARP']
           format = '(2X,A5,2A9,'+strtrim(nphotags+2,2)+'A9)'
           printf,unit,format=format,fields
         endif

       end ; /colhead

    ; DAOPHOT-style header
    1: begin
         nhead = n_elements(daohead)

         ; Regular header input
         if nhead eq 2 then begin
           printf,unit,daohead[0]
           printf,unit,daohead[1]
           printf,unit,''
         ; Non-regular header, printing blanks
         endif else begin
           print,'WRITE_PHOT.PRO ERROR - Non-regular DAOPHOT header input. Printing blanks.'
           printf,unit,''
           printf,unit,''
           printf,unit,''
         endelse
       end ; /daophot-style header
          
    ; NO HEADER
    2: begin
         ; Don't need to do anything
       end
         
  endcase  ; header


  ; Printing RA/DEC coordinates
  if keyword_set(radec) then begin

    ; Setting up the command
    cmd = "printf,unit,format='(2X,I5,2F12.6,"+strtrim(nphotags+2,2)+"F9.4)',str[i].id,"
    cmd = cmd+"str[i].ra,str[i].dec,"
    for i=0,nphotags-1 do cmd=cmd+"str[i].("+strtrim(i+3,2)+"),"
    cmd = cmd+"str[i].chi,str[i].sharp"

    ; Print the data
    for i=0.,nstr-1 do begin
      dum = execute(cmd)
      ;printf,unit,format='(2X,I5,2F11.5,6F9.4)',str[i].id,str[i].ra,str[i].dec,$
      ;       str[i].bmag,str[i].berr,str[i].rmag,str[i].rerr,str[i].chi,str[i].sharp
    end

  endif ; RA/DEC coordinates

  ; Printing BOTH X/Y and RA/DEC coordinates
  if keyword_set(both) then begin
  
    ; Setting up the command
    cmd = "printf,unit,format='(2X,I5,2F9.3,2F12.6,"+strtrim(nphotags+2,2)+"F9.4)',str[i].id,"
    cmd = cmd+"str[i].x,str[i].y,str[i].ra,str[i].dec,"
    for i=0,nphotags-1 do cmd=cmd+"str[i].("+strtrim(i+3,2)+"),"
    cmd = cmd+"str[i].chi,str[i].sharp"

    ; Print the data
    for i=0.,nstr-1 do dum = execute(cmd)
           
  endif ; both X/Y and RA/DEC coordinates

  ; Printing X/Y pixels coordinates
  if not keyword_set(radec) and not keyword_set(both) then begin

    ; Setting up the command
    cmd = "printf,unit,format='(2X,I5,2F9.3,"+strtrim(nphotags+2,2)+"F9.4)',str[i].id,"
    cmd = cmd+"str[i].x,str[i].y,"
    for i=0,nphotags-1 do cmd=cmd+"str[i].("+strtrim(i+3,2)+"),"
    cmd = cmd+"str[i].chi,str[i].sharp"

    ; Print the data
    for i=0.,nstr-1 do begin
      dum = execute(cmd)
      ;printf,unit,format='(2X,I5,2F9.5,6F9.4)',str[i].id,str[i].x,str[i].y,$
      ;       str[i].bmag,str[i].berr,str[i].rmag,str[i].rerr,str[i].chi,str[i].sharp
    end

  endif

endelse

; Closing the file
close,unit
free_lun,unit

if keyword_set(stp) then stop

end

;---------------------------

pro run_daomatch,files,trans,stp=stp

; This program runs DAOMATCH

; Not enough inputs
if n_elements(files) eq 0 then begin
  print,'Syntax - run_daomatch,files,stp=stp'
  return
endif

; Checking that all the files exist
nfiles = n_elements(files)
for i=0,nfiles-1 do begin
  test = file_test(files[i])
  if test eq 0 then begin
    print,'FILE ',files[i],'DOES NOT EXIST'
    return
  endif
end

; DAOMATCH needs to be run in the directory of the files
cd,current=origdir

dirs = file_dirname(files)
ui = uniq(dirs,sort(dirs))
nui = n_elements(ui)

; The directories are not all the same
if (nui ne 1) then begin
  print,'THE FILES ARE NOT ALL IN THE SAME DIRECTORY'
  return
endif

; CD to the directory
dir = dirs[0]
cd,dir

; Getting base filenames
files2 = file_basename(files)

; Delete MCH file if any
mchfile = files2[0]+'.mch'
file_delete,mchfile,/allow_nonexistent

; Running daomatch.sh
cmd = '/home/dln5q/bin/daomatch.sh '
nmatch = nfiles < 3
for i=0,nmatch-1 do cmd=cmd+' '+files2[i]
spawn,cmd,out,errout

; Checking for problems
nlines = file_lines(files2[0]+'.mch')
if nlines lt nmatch then begin
  print,'Number of lines in MCH is only ',strtrim(nlines,2),'  EXPECTED ',strtrim(nmatch,2)
  print,'CHECK THIS BY HAND!!!'
endif

if (errout[0] ne '') then begin
  print,'DAOMATCH.SH ERROR'
  print,errout
  return
endif

; Loading the transformation from the MCH file
mchfile = files2[0]+'.mch'

; Checking for the MCH file
test = file_test(mchfile)
if test eq 0 then begin
  print,'ERROR.  NO MCH FILE'
  return
endif

openr,unit,/get_lun,mchfile

while(~EOF(unit)) do begin
  line=''
  readf,unit,line
  push,lines,line
endwhile

close,unit
free_lun,unit

; Creating the trans array
lines2 = repstr(lines,"'")
nlines = n_elements(lines)
arr = strsplit(lines2[0],' ',/extract)
ntrans = n_elements(arr)-3    ; first line is the samee, last two are mag offsets

; Initializing the array
trans = fltarr(nlines,ntrans)
; Filling the aray
for i=0,nlines-1 do begin
  arr = strsplit(lines2[i],' ',/extract)
  trans[i,*] = arr[1:ntrans]
end

; CD back to the original directory
cd,origdir

if keyword_set(stp) then stop

end

;---------------------------

pro run_daomaster,mchfile,trans,siglim=siglim,ndegree=ndegree,$
                  ind1=ind1,ind2=ind2,stp=stp

; This programs runs DAOMASTER
; siglim is the sigma limit for DAOMASTER.  Default: siglim=99.
; ndegree is the desired degrees of freedom to use in DAOMASTER
;                  Desired degrees of freedom ---
;
;                  2:  Translations only
;                  4:  Translations, rotation, and scale
;                  6:  Six-constant linear transformation
;                 12:  Quadratic transformation
;                 20:  Cubic transformation
; Default ndegree=4
; ind1 is the array of USNO indices for matched stars
; ind2 is the array of DAO indices for matched stars

; Not enough inputs
if n_elements(mchfile) eq 0 then begin
  print,'Syntax - run_daomaster,mchfile,stp=stp'
  return
endif

; Checking that all the files exist
test = file_test(mchfile)
if test eq 0 then begin
  print,'FILE ',mchfile,'DOES NOT EXIST'
  return
endif

; DAOMATCH needs to be run in the directory of the files
cd,current=origdir
  
; CD to the directory
dir = file_dirname(mchfile)
cd,dir
  
; Getting base filename
mchfile2 = file_basename(mchfile,'.mch')

; Deleting files
mchfile3 = file_basename(mchfile2,'.tempcoo')
file_delete,mchfile3+'.tfr',/allow_nonexistent
file_delete,mchfile3+'.raw',/allow_nonexistent
file_delete,mchfile3+'.mch',/allow_nonexistent

; DAOMASTER settings
if n_elements(siglim) eq 0 then siglim=99
if n_elements(ndegree) eq 0 then ndegree=4
degrees = [2,4,6,12,20]     ; making sure it's an acceptable value
dum = closest(ndegree,degrees,ind=ind)
ndeg = degrees[ind]

; Running daomaster_astrom.sh
cmd = '/home/dln5q/bin/daomaster_astrom.sh '+mchfile2
cmd = cmd+' '+strtrim(siglim,2)+' '+strtrim(ndeg,2)
spawn,cmd,out,errout

if (errout[0] ne '') then begin
  print,'DAOMASTER_ASTROM.SH ERROR'
  print,errout
  return
endif
  
; Loading the transformation from the MCH file
mchfile = mchfile3+'.mch'

; Checking for the MCH file
test = file_test(mchfile)
if test eq 0 then begin      
  print,'ERROR.  NO MCH FILE'
  return
endif

openr,unit,/get_lun,mchfile
  
while(~EOF(unit)) do begin  
  line=''
  readf,unit,line
  push,lines,line
endwhile  

close,unit
free_lun,unit

; Creating the trans array  
lines2 = repstr(lines,"'")
nlines = n_elements(lines)
arr = strsplit(lines2[0],' ',/extract)
ntrans = n_elements(arr)-3    ; first line is the samee, last two are mag offsets
      
; Initializing the array
trans = fltarr(nlines,ntrans)
; Filling the aray
for i=0,nlines-1 do begin
  arr = strsplit(lines2[i],' ',/extract)
  trans[i,*] = arr[1:ntrans]
end


; GET THE MATCHING INDICES
; Load the TFR file and figure out which stars match
tfrfile = mchfile3+'.tfr'

tfr = importascii(tfrfile,fieldnames=['ID','X','Y','IND1','IND2'],$
                  fieldtypes=[3,4,4,3,3],skip=3,/noprint)

; Matches, index starts from 1
mind = where(tfr.ind1 gt 0 and tfr.ind2 gt 0,nmind)

; There is a bug in the way DAOMASTER outputs the TFR files
; it's only correct for the stars that pass the sigma criteria
; but as long as we have the same sigma limit we should be okay.
ind1 = tfr[mind].ind1-1    ; USNO matching indices
ind2 = tfr[mind].ind2-1    ; DAO matching indices

; CD back to the original directory   
cd,origdir

if keyword_set(stp) then stop

end

;-----------------------------

function matmult,x,y,par

; This is the function that calculates the transformation
; from observed to real coordinates (RA or DEC, one at a time)
; given the appropriate input parameters

; par is a norder x norder matrix of coefficients
; 1st index is for X, 2nd is for Y
; par(0,0) = is the constant offset 
; par(1,0) = X
; par(2,0) = X^2
; par(0,1) = Y
; par(0,2) = Y^2
; par(1,2) = X*Y^2

sum = 0.0d0

sz = size(par)
nord = sz[1]
nstars = n_elements(x)

; Loop through the matrix
for i=0.,nord-1 do begin
  for j=0.,nord-1 do begin
    sum = sum + par[i,j]*(x^i)*(y^j)
  end
end

return,sum

end

;----------------------------

function matmultdev,par,x=x,y=y,z=z,err=err

; X = ra
; Y = dec
; Z = theoretical ra or dec

new = matmult(x,y,par)
dev = (z-new)/err

return,dev

end

;--------------------------
         
pro trans_radec,trans,x,y,newra,newdec,stp=stp

; This function uses the TRANS structure to transform
; RA/DEC from the original system to the correct system.

; Remove RA/DECZERO
; Remove cos(dec) from RA
; Convert to arcsec, multiply by 3600

radeg = 180.0d0/!dpi

;ra1 = ra-trans.razero  
;ra1 = ra1*cos(trans.deczero/radeg)
;dec1 = dec-trans.deczero
;ra1 = ra1*3600.0d0
;dec1 = dec1*3600.0d0
x1 = x-trans.xzero
y1 = y-trans.yzero

rnew = matmult(x1,y1,trans.rpar)
dnew = matmult(x1,y1,trans.dpar)
   
;; Reconstruct the final version
;; Convert back:
;; (1) Convert back to degrees, divide by 3600
;; (2) Add small correction with matmult
;; (3) Divide by cos(deczero)
;; (4) Add ra/deczero
;; MUST BE IN THIS ORDER!!!
;rnew = rnew/3600.0d0
;dnew = dnew/3600.0d0
;newdec = dnew + trans.deczero
;;newra = rnew/cos(trans.deczero/radeg)
;newra = rnew/cos(newdec/radeg)
;newra = newra + trans.razero

; Convert back
dec = dnew/3600.d0   
ra = rnew/3600.d0/cos(dec/radeg)
rotate_lb,ra,dec,trans.npole,trans.equator,newra,newdec,/noprint,/contin,/reverse

if keyword_set(stp) then stop

end

;--------------------------

pro fit_radec,usno,phot,trans,nord=nord,init=init,stp=stp,xzero=xzero,yzero=yzero,$
              razero=razero,deczero=deczero,allfixed=allfixed,fitrot=fitrot,ndown=ndown

; This gets the transformation equation to get
; the right RA/DEC for stars.
; init - You can input a previous transformation structure
;  which will be used as the initial estimate for MPFIT
; /allfixed - hold all the coefficients fixed
; /fitrot -  allow it to rotate only

;#####################################################
; Now we do the fitting ourselves
; using the RA/DEC coordinates of the matched stars
;#####################################################

; Not enough inputs
if n_elements(usno) eq 0 or n_elements(phot) eq 0 then begin
  print,'Syntax - fit_radec,usno,phot,trans,nord=nord,init=init,stp=stp,xzero=xzero,yzero=yzero'
  return
endif

if n_elements(ndown) eq 0 then ndown=1 else ndown=ndown+1  ; how many levels down are we

radeg = 180.0d0/!dpi

if n_elements(nord) eq 0 then nord=2    ; use order=2 by default

if nord lt 1 then begin
  print,'NORD MUST be >=1'
  return
endif

x0 = phot.x              ; always use the INITIAL coordinates
y0 = phot.y  
rref = usno.raj2000
dref = usno.dej2000
;rdiff = rref-r0
;ddiff = dref-d0
err = sqrt( (usno.e_radeg/3.6d6)^2. + (usno.e_dedeg/3.6d6)^2. )
err = err > 1d-10    ; can't have err=0

; It's best to remove the a constant from everything
; so that we're only working with small numbers
; Also taking out the cos(dec) effect on RA

; Costant offset
if n_elements(razero) eq 0 then razero = median(rref)
if n_elements(deczero) eq 0 then deczero = median(dref)
if n_elements(xzero) eq 0 then xzero = median(x0)
if n_elements(yzero) eq 0 then yzero = median(y0)

x1 = x0-xzero
y1 = y0-yzero


; Rotate to new coordinates system which has the origin
; at (razero,deczero)
npole = [razero,90.0d0+deczero]
if deczero gt 0.0 then begin
  ranpole = razero-180.0d0
  if ranpole lt 0.0 then ranpole=ranpole+360.0d0
  npole = [ranpole,90.0d0-deczero]
endif
;great_circle,[razero,deczero],[razero+30.0d0,deczero],dum1,dum2,npole,dum3
equator = [razero,deczero]
rotate_lb,rref,dref,npole,equator,newra,newdec,/noprint,/contin

rref1 = newra*3600.0d0*cos(newdec/radeg)   ; The cos(dec) probably has little effect
dref1 = newdec*3600.0d0

;rref1 = rref-razero
;;rref1 = rref1*cos(deczero/radeg)
;rref1 = rref1*cos(dref/radeg)        ; use each stars' dec, takes care of 
;dref1 = dref-deczero                 ; changes in dec across the chip
;
;; Convert to arcsec
;;r1 = r1 * 3600.0d0
;;d1 = d1 * 3600.0d0
;rref1 = rref1 * 3600.0d0
;dref1 = dref1 * 3600.0d0
err1 = err*3600.0d0

rdiff1 = rref1-x1
ddiff1 = dref1-y1


;---------------------------------
; RIGHT ASCENSION
;---------------------------------

rpar = dblarr(nord>2,nord>2)  ; we need at least a 2x2 matrix

; Figure out our own initial estimates
if n_elements(init) eq 0 then begin

  ; Getting initial RA estimates
  coefrr = poly_fit(x1,rdiff1,1)
  coefdr = poly_fit(y1,rdiff1,1)

  rpar[0,0] = median(rdiff1)
  if nord gt 1 then begin
    rpar[1,0] = 1.0+coefrr[1]
    rpar[0,1] = coefdr[1]
    rpar[1,1] = 0.00
  endif else begin
    rpar[1,0] = 1.0
  endelse

; Using the initial estimate input
endif else begin

  rpar[0,0] = init.rpar   ; it will fill it in correctly

endelse

; Now run MPFIT.PRO to get the best fit
nstars = n_elements(x1)
npar = n_elements(rpar)
; Making the constraints structure
if (nord gt 1) then begin
  parinfo = replicate({limited:[1,1],limits:[-0.5,0.5],fixed:0},npar)
  parinfo[0].limits = [-5000,5000]
  parinfo[1].limits = [-5.,5]
  parinfo[nord].limits = [-5.,5]
endif else begin
  parinfo = replicate({limited:[0,0],limits:[0.,0.],fixed:1},4)
  parinfo[0].limited = [1,1]
  parinfo[0].limits = [-500,500]
  parinfo[0].fixed = 0
endelse
; Rotation ONLY allowed
if keyword_set(fitrot) then begin
  parinfo.fixed = 1         ; all fixed
  parinfo[0].fixed = 0      ; constant term
  parinfo[1].fixed = 0      ; rpar[0,1]
  parinfo[nord].fixed = 0   ; rpar[1,0]
  parinfo[nord+1].fixed = 0   ; rpar[1,1]
endif
ftol = 1d-10
fa = {x:x1,y:y1,z:rref1,err:err1}

; FITTING
if not keyword_set(allfixed) then begin
  func = 'matmultdev'
  frpar = mpfit(func, rpar, functargs=fa, perror=perror,niter=iter,status=status,$
            bestnorm=chisq, parinfo=parinfo, dof=dof, ftol=ftol, /quiet) 
endif else begin
  frpar = rpar
  status = 1
  dof = 1.0
  chisq = 1.0
  perror = dblarr(npar)
endelse

; MPFIT ERROR
if (status lt 1) then begin
  print,'MPFIT RA PROBLEM - Status=',strtrim(status,2)
  trans=-1
  goto,BOMB
endif

; New RA coordinates
rnew = matmult(x1,y1,frpar)

; RA error information
rresid = rref1-rnew
rrms = stdev(rresid)
rsigpar = perror * sqrt(chisq/dof)
rchi = sqrt(chisq/dof)

;---------------------------------
; DECLINATION
;---------------------------------

dpar = dblarr(nord>2,nord>2)    ; we need at least a 2x2 matrix

; Figure out our own initial estimates 
if n_elements(init) eq 0 then begin

  ; Getting initial DEC estimates
  coefrd = poly_fit(x1,ddiff1,1)
  coefdd = poly_fit(y1,ddiff1,1)

  dpar[0,0] = median(ddiff1)
  if nord gt 1 then begin
    dpar[1,0] = coefrd[1]
    dpar[0,1] = 1.0+coefdd[1]
    dpar[1,1] = 0.00
  endif else begin
    dpar[0,1] = 1.0
  endelse

; Using the initial estimate input
endif else begin

  dpar[0,0] = init.dpar

endelse

; Now run MPFIT.PRO to get the best fit
nstars = n_elements(y1)
npar = n_elements(dpar)
; Making the constraints structure
if (nord gt 1) then begin
  parinfo = replicate({limited:[1,1],limits:[-0.5,0.5],fixed:0},npar)
  parinfo[0].limits = [-5000,5000]
  parinfo[1].limits = [-5.,5.]
  parinfo[nord].limits = [-5.,5.]   ; [0,1] -> nord
endif else begin
  parinfo = replicate({limited:[0,0],limits:[0.,0.],fixed:1},npar)
  parinfo[0].limited = [1,1]
  parinfo[0].limits = [-1000,1000]
  parinfo[0].fixed = 0
endelse
; Rotation ONLY allowed
if keyword_set(fitrot) then begin
  parinfo.fixed = 1         ; all fixed
  parinfo[0].fixed = 0      ; constant term
  parinfo[1].fixed = 0      ; dpar[0,1]
  parinfo[nord].fixed = 0   ; dpar[1,0]
  parinfo[nord+1].fixed = 0   ; dpar[1,1]
endif
;if keyword_set(allfixed) then parinfo.fixed=1
ftol = 1d-10
fa = {x:x1,y:y1,z:dref1,err:err1}

; FITTING
if not keyword_set(allfixed) then begin
  func = 'matmultdev'
  fdpar = mpfit(func, dpar, functargs=fa, perror=perror,niter=iter,status=status,$
            bestnorm=chisq, parinfo=parinfo, dof=dof, ftol=ftol, /quiet)
endif else begin
  fdpar = dpar
  status = 1
  dof = 1.0
  chisq = 1.0
  perror = dblarr(npar)
endelse


; MPFIT ERROR
if (status lt 1) then begin
  print,'MPFIT DEC PROBLEM - Status=',strtrim(status,2)
  trans=-1
  goto,BOMB
endif

; New DEC coordinates
dnew = matmult(x1,y1,fdpar)

; DEC error information
dresid = dref1-dnew
drms = stdev(dresid)
dsigpar = perror * sqrt(chisq/dof)
dchi = sqrt(chisq/dof)

; Final RMS
;rms = stdev(sqrt(rresid^2.0 + dresid^2.))
resid = sqrt( rresid^2.0 + dresid^2.0 )
rms = sqrt( mean( resid^2.0 ) )

; Put all the transformation equation into a structure
trans = {nord:0,nstars:0L,razero:0.0d0,deczero:0.d0,npole:dblarr(2),equator:dblarr(2),xzero:0.0d0,$
         yzero:0.0d0,rpar:dblarr(nord>2,nord>2),$
         rsigpar:dblarr(nord>2,nord>2),rrms:0.0d0,rchi:0.0d0,dpar:dblarr(nord>2,nord>2),$
         dsigpar:dblarr(nord>2,nord>2),drms:0.0d0,dchi:0.0d0,rms:0.0d0}

trans.nord = nord
trans.nstars = nstars
trans.razero = razero
trans.deczero = deczero
trans.npole = npole
trans.equator = equator
trans.xzero = xzero
trans.yzero = yzero
trans.rpar = frpar
trans.rsigpar = rsigpar
trans.rrms = rrms
trans.rchi = rchi
trans.dpar = fdpar
trans.dsigpar = dsigpar
trans.drms = drms
trans.dchi = dchi
trans.rms = rms


; Get RA/DEC at the center of the image
TRANS_RADEC,trans,xzero,yzero,cen_ra,cen_dec
cen_ra = cen_ra[0]
cen_dec = cen_dec[0]
diff = sphtrigdist(razero,deczero,cen_ra,cen_dec)*3600.0d0

;##########################
; Now rerun FIT_RADEC.PRO with the RA/DEC of the center of the image
; as razero,deczero
;if (diff gt 0.00001) then begin
if (diff gt 0.001) and ndown lt 5 then begin

  undefine,trans
  FIT_RADEC,usno,phot,trans,nord=nord,razero=cen_ra,deczero=cen_dec,xzero=xzero,yzero=yzero,$
            allfixed=allfixed,fitrot=fitrot,init=init,ndown=ndown

endif

;print,trans.rpar[0],trans.dpar[0]

BOMB:

if keyword_set(stp) then stop

end

;---------------------------

pro compare_astrom,ra1,dec1,ra2,dec2,raresid,decresid,resid,rms=rms,noprint=noprint,stp=stp

; This program compares two sets of coordinates

nra1 = n_elements(ra1)
ndec1 = n_elements(dec1)
nra2 = n_elements(ra2)
ndec2 = n_elements(dec2)

; Not enough inputs
if nra1 eq 0 or ndec1 eq 0 or nra2 eq 0 or ndec2 eq 0 then begin
  print,'Syntax - compare_astrom,ra1,dec1,ra2,dec2,raresid,decresid,resid,rms=rms,'
  print,'                        noprint=noprint,stp=stp'
  return
endif

; Checking that their dimensions are equal
nra1 = n_elements(ra1)
nra2 = n_elements(ra2)
if nra1 ne nra2 then begin
  print,'RA1 and RA2 do not have equal elements'
  return
endif

if nra1 ne ndec1 then begin
  print,'RA1 and DEC1 do not have equal elements'
  return
end

if nra1 ne ndec1 then begin
  print,'RA1 and DEC1 do not have equal elements'
  return
end

; Calculating residuals
raresid = (ra1-ra2)*cos(dec1/!radeg)*3600.
decresid = (dec1-dec2)*3600.
resid = sqrt( raresid^2.0 + decresid^2.0 )
rms =  sqrt( mean( resid^2. ) )
;rms = stdev( sqrt( raresid^2. + decresid^2.) )

; Printing statistics 
if not keyword_set(noprint) then begin
  form = '(A-15,A7,A8)'   
  print,''
  print,'** FIT STATISTICS **'
  print,'RA:'
  print,format=form,'Mean Resid',stringize(mean(raresid),ndec=3),' arcsec'
  print,format=form,'Stdev Resid',stringize(stdev(raresid),ndec=3),' arcsec'
  print,format=form,'Max Abs(Resid)',stringize(max(abs(raresid)),ndec=3),' arcsec'
  print,'DEC:'
  print,format=form,'Mean Resid',stringize(mean(decresid),ndec=3),' arcsec'
  print,format=form,'Stdev Resid',stringize(stdev(decresid),ndec=3),' arcsec'
  print,format=form,'Max Abs(Resid)',stringize(max(abs(decresid)),ndec=3),' arcsec'
  print,'
  print,format=form,'Total RMS',stringize(rms,ndec=3),' arcsec'
  print,''
endif ; not /noprint

maxresid = max([raresid,decresid])
maxresid2 = ceil(maxresid*10.)/10.

if keyword_set(stp) then stop

end

;---------------------------

pro plot_astrom_resid,ra1,dec1,ra2,dec2,ps=ps,xr=xr,yr=yr,title=title,rms=rms,stp=stp,noprint=noprint

; This plots the astrometry residuals

nra1 = n_elements(ra1)
ndec1 = n_elements(dec1)
nra2 = n_elements(ra2)
ndec2 = n_elements(dec2)

; Not enough inputs
if nra1 eq 0 or ndec1 eq 0 or nra2 eq 0 or ndec2 eq 0 then begin
  print,'Syntax - plot_astrom_resid,ra1,dec1,ra2,dec2,ps=ps,xr=xr,yr=yr,title=title,stp=stp,noprint=noprint'
  return
endif

; Checking that their dimensions are equal
nra1 = n_elements(ra1)
nra2 = n_elements(ra2)
if nra1 ne nra2 then begin
  print,'RA1 and RA2 do not have equal elements'
  return
endif

if nra1 ne ndec1 then begin 
  print,'RA1 and DEC1 do not have equal elements'
  return
end

if nra1 ne ndec1 then begin
  print,'RA1 and DEC1 do not have equal elements'
  return
end


; Getting the residuals
COMPARE_ASTROM,ra1,dec1,ra2,dec2,raresid,decresid,resid,rms=rms,noprint=noprint

maxresid = max([raresid,decresid])
maxresid2 = ceil(maxresid*10.)/10.

; Plotting the residuals
if not keyword_set(xr) then xr = reverse(minmax(ra2))
if not keyword_set(yr) then yr = minmax(dec2)

if not keyword_set(ps) then ps=1

;!p.multi=[0,2,1]
mult = 125./maxresid2  ; 250.
colors = raresid*mult+125.
offrange = 125./mult
plot,ra1,dec1,ps=ps,xtit='RA',ytit='DEC',tit='RA Residuals',xr=xr,yr=yr,xs=1,ys=1,$
     position=[0.09,0.08,0.48,0.88]
plots,ra1,dec1,color=colors,ps=ps,/data
colorbar,ncolors=255,bottom=0,maxrange=-offrange,minrange=offrange,position=[0.09,0.96,0.48,0.98],$
         format='(F5.2)'

;!p.multi=[1,2,1]
colors = decresid*mult+125.
plot,ra1,dec1,ps=ps,xtit='RA',ytit='DEC',tit='DEC Residuals',xr=xr,yr=yr,xs=1,ys=1,$
     position=[0.58,0.08,0.97,0.88],/noerase
plots,ra1,dec1,color=colors,ps=ps,/data
colorbar,ncolors=255,bottom=0,maxrange=-offrange,minrange=offrange,position=[0.58,0.96,0.97,0.98],$
         format='(F5.2)'

; Overplot rms and chi
;xyouts,0.5,0.90,'RMS='+stringize(rms,ndec=2)+'"',charsize=1.0,align=0.5,/normal

if keyword_set(title) then xyouts,0.52,0.90,title,charsize=1.2,align=0.4,/normal
;if keyword_set(title) then xyouts,0.52,0.89,title,charsize=1.2,align=0.4,/normal

;!p.multi=0

if keyword_set(stp) then stop

end

;---------------------------

pro get_astrom,filename,trans,maxord=maxord,init=init,plotresid=plotresid,stp=stp

; This gets the astrometry for stars using the USNO-B1 catalog
; init  is an initial transformation to use
; maxord   you can't use an order higher than this

radeg = 180.0d0/!dpi
if n_elements(maxord) eq 0 then maxord=4

; Not enough inputs
if n_params() lt 1 then begin
  print,'Syntax - get_astrom,filename'
  return
endif

; Checking that the image exists
test = file_test(filename)
if test eq 0 then begin
  print,'FILE ',filename,' DOES NOT EXIST'
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

; File information
dir = file_dirname(filename)
filebase = file_basename(filename,'.fits')

; Print the filename
print,filebase

; Create the astrom/ directory if it doesn't exist yet
if file_test(dir+'/astrom',/directory) eq 0 then file_mkdir,dir+'/astrom'

; Starting the LOG file
logfile = dir+'/astrom/'+filebase+'_astrom.log'
file_delete,logfile,/allow_nonexistent
journal,logfile

;##########################
;# LOAD the data
;##########################

; Get the header
head = headfits(filename)
nx = sxpar(head,'NAXIS1')
ny = sxpar(head,'NAXIS2')

; Get USNO info
GET_USNO,head,usno

; Load the DAOPHOT file
photname = dir+'/'+filebase+'.phot'
LOAD_PHOT,photname,head,phot


;#################################################
; USING DAOMATCH/DAOMASTER to get the first match
; and an initial estimate
;#################################################
if not keyword_set(init) then begin


  ;#######################################
  ;# Write both sets of stars to text files
  ; USE PIXELS VALUES TO MATCH
  ; DAOMATCH/MASTER works better this way

  ; ALS filename
  alsfile = dir+'/'+filebase+'.als'

  ; Load the ALS header
  alshead = LOAD_DAOHEAD(alsfile)


  ; ---------------------------------
  ; Use only the BEST stars at first, for an initial solution
  siglim = 20.

  ; USNO first
  usnotemp = dir+'/astrom/'+filebase+'_usno.tempcoo'
  blim = siglim
  errlim = 400.
  pmlim = 100.
  ugd = where(usno.x ge 0. and usno.x le nx-1 and usno.y ge 0 and usno.y le ny-1 and $
             usno.e_radeg lt errlim and usno.e_dedeg lt errlim and abs(usno.pmra) lt pmlim and $
             abs(usno.pmde) lt pmlim and usno.bmag lt blim,nugd)
  WRITE_PHOT,usno[ugd],usnotemp,daohead=alshead


  ; Now the DAOPHOT stars
  daotemp = dir+'/astrom/'+filebase+'_dao.tempcoo'
  ilim = siglim
  sharplim = 1.5
  photlim = 0.2
  pgd = where(phot.x ge 0 and phot.x le nx-1 and phot.y ge 0 and phot.y le ny-1 and $
             phot.i lt ilim and abs(phot.sharp) lt sharplim and phot.ierr lt photlim,npgd)
  WRITE_PHOT,phot[pgd],daotemp,daohead=alshead


  ; Run DAOMATCH and DAOMASTER on the files
  RUN_DAOMATCH,[usnotemp,daotemp],trans1a
  RUN_DAOMASTER,usnotemp+'.mch',trans1b,siglim=siglim,ndegree=4,$
                ind1=usno_ind,ind2=phot_ind

  ;xshift = trans1b[1,0]  ; xref = x + xshift
  ;yshift = trans1b[1,1]  ; yref = y + yshift
  ;xshift = median( usno[ugd[usno_ind]].x - phot[pgd[phot_ind]].x )
  ;yshift = median( usno[ugd[usno_ind]].y - phot[pgd[phot_ind]].y )

  usno1 = usno[ugd[usno_ind]]
  phot1 = phot[pgd[phot_ind]]
  xshift = median( usno1.x - phot1.x )
  yshift = median( usno1.y - phot1.y )

  print,'DAOMASTER Nmatch = ',strtrim(n_elements(usno1),2)


;###################################
; USING AN INITIAL TRANSFORMATION
;###################################
endif else begin

  maglim = 20.0

  ; USNO first
  blim = maglim
  errlim = 400.
  pmlim = 100.
  ugd = where(usno.e_radeg lt errlim and usno.e_dedeg lt errlim and abs(usno.pmra) lt pmlim and $
             abs(usno.pmde) lt pmlim and usno.bmag lt blim,nugd)

  ; Now the DAOPHOT stars
  ilim = maglim
  sharplim = 1.5
  photlim = 0.2
  pgd = where(phot.i lt ilim and abs(phot.sharp) lt sharplim and phot.ierr lt photlim,npgd)

  temp = phot[pgd]
  ; Getting new coordinates with the initial solution
  TRANS_RADEC,init,temp.x,temp.y,newra,newdec
  temp.ra = newra  
  temp.dec = newdec

  lim = 2.0   ; in arcsec
  SRCOR,usno[ugd].raj2000,usno[ugd].dej2000,temp.ra,temp.dec,lim,ind1,ind2,$
        option=1,spherical=2

  usno1 = usno[ugd[ind1]]
  phot1 = phot[pgd[ind2]]

  xshift = median( usno1.x - phot1.x )
  yshift = median( usno1.y - phot1.y )
  
  print,'INITIAL Nmatch = ',strtrim(n_elements(usno1),2)

endelse ; initial transformation


;#####################################################
; GET AN INITIAL SOLUTION
;#####################################################

; Not very many matches, use Nord=2
if n_elements(usno1) lt 10 then begin

  ; Fitting, NORD=2
  FIT_RADEC,usno1,phot1,trans2,nord=2 
  if size(trans2,/type) ne 8 then begin
    print,'BAD FIT - BOMBING'
    goto,BOMB
  endif
  print,'Nord=2  RMS=',stringize(trans2.rms,ndec=3),' arcsec'

  ; Getting new coordinates with the initial solution
  TRANS_RADEC,trans2,phot.x,phot.y,newra,newdec
  phot.ra = newra
  phot.dec = newdec
  rms = trans2.rms

; Enough matches, use Nord=3
endif else begin

  ; Fitting, NORD=3
  FIT_RADEC,usno1,phot1,trans3,nord=3
  if size(trans3,/type) ne 8 then begin
    print,'BAD FIT - BOMBING'
    goto,BOMB   
  endif
  print,'Nord=3  RMS=',stringize(trans3.rms,ndec=3),' arcsec'

  ; Getting new coordinates with the initial solution
  TRANS_RADEC,trans3,phot.x,phot.y,newra,newdec
  phot.ra = newra
  phot.dec = newdec
  rms = trans3.rms

endelse


;################################################################
; LOOSEN THE CONSTRAINTS AND USE SRCOR.PRO TO FIND MORE MATCHES
;################################################################

maglim = 21.0

blim = maglim
errlim = 400.
pmlim = 100.
ugd2 = where(usno.x-xshift ge 0. and usno.x-xshift le nx-1 and usno.y-yshift ge 0 and $
           usno.y-yshift le ny-1 and usno.e_radeg lt errlim and usno.e_dedeg lt errlim and $
           abs(usno.pmra) lt pmlim and abs(usno.pmde) lt pmlim and usno.bmag lt blim,nugd2)
ilim = maglim
sharplim = 5.0
photlim = 0.2
pgd2 = where(phot.x ge 0 and phot.x le nx-1 and phot.y ge 0 and phot.y le ny-1 and $  
           phot.i lt ilim and abs(phot.sharp) lt sharplim and phot.ierr lt photlim,npgd2)

temp = phot[pgd2]
;; Getting new coordinates with the initial solution
;TRANS_RADEC,trans3,temp.x,temp.y,newra,newdec
;temp.ra = newra
;temp.dec = newdec

lim = 0.5 > 2.0*rms   ; in arcsec
SRCOR,usno[ugd2].raj2000,usno[ugd2].dej2000,temp.ra,temp.dec,lim,ind1,ind2,$
      option=1,spherical=2

usno2 = usno[ugd2[ind1]]
phot2 = phot[pgd2[ind2]]


;#####################################################
; Now we do the fitting ourselves
; using the RA/DEC coordinates of the matched stars
;#####################################################

; Fitting, NORD=2
FIT_RADEC,usno2,phot2,trans2,nord=2
if size(trans2,/type) ne 8 then begin 
  print,'BAD FIT - BOMBING'
  goto,BOMB
endif
print,'Nord=2  RMS=',stringize(trans2.rms,ndec=3),' arcsec'

; Get new coordinates
TRANS_RADEC,trans2,phot2.x,phot2.y,newra,newdec
phot2.ra = newra
phot2.dec = newdec


; Bad fit, try 3rd order if possible
if trans2.rms gt 1.0 and maxord ge 3 and n_elements(usno2) ge 10 then begin
  FIT_RADEC,usno2,phot2,trans3,nord=3
  if size(trans3,/type) ne 8 then begin 
    print,'BAD FIT - BOMBING'
    goto,BOMB
  endif
  print,'Nord=3  RMS=',stringize(trans3.rms,ndec=3),' arcsec'

  ; Get new coordinates
  TRANS_RADEC,trans3,phot2.x,phot2.y,newra,newdec
  phot2.ra = newra
  phot2.dec = newdec

endif


; ---Throw out bad stars---
phot2_orig = phot2
usno2_orig = usno2
flag = 0
count = 1
niter = 5
ntoss = 0
while (flag ne 1) do begin
  ; Getting the residuals and rms
  COMPARE_ASTROM,usno2.raj2000,usno2.dej2000,phot2.ra,phot2.dec,raresid,decresid,resid,rms=rms,/noprint
  usnoerr = sqrt( (usno2.e_radeg/1d3)^2. + (usno2.e_dedeg/1d3)^2. )

  ; Using a 3 sigma cut
  lim = (3.0*rms) > 0.25
  bad = where( resid gt lim OR usnoerr gt lim ,nbad)
  if nbad gt 0 then remove,bad,phot2,usno2

  if nbad eq 0 or count ge niter then flag=1
  ntoss = ntoss+nbad

  ; Okay this is the last one
  ; make sure the cut was low enough
  if (flag eq 1) then begin
    COMPARE_ASTROM,usno2.raj2000,usno2.dej2000,phot2.ra,phot2.dec,raresid,decresid,resid,rms=rms,/noprint
    usnoerr = sqrt( (usno2.e_radeg/1d3)^2. + (usno2.e_dedeg/1d3)^2. )

    bad = where( resid gt 0.40 OR usnoerr gt 0.40 ,nbad)
    if nbad gt 0 then remove,bad,phot2,usno2
    ntoss = ntoss+nbad
  endif

  count = count+1
endwhile
print,'Tossing out ',strtrim(ntoss,2),' Bad stars'
print,'Nmatch = ',strtrim(n_elements(phot2),2)

; Could also use the magnitudes to do a cut
;mndiff = mean(usno2.rmag-phot2.i)

;; Fitting, NORD=1
;FIT_RADEC,usno2,phot2,trans1,nord=1
;if size(trans1,/type) ne 8 then begin 
;  print,'BAD FIT - BOMBING'
;  goto,BOMB
;endif
;print,'Nord=1  RMS=',stringize(trans1.rms,ndec=3),' arcsec'
;trans = trans1

; Fitting, NORD=2
FIT_RADEC,usno2,phot2,trans2,nord=2
if size(trans2,/type) ne 8 then begin 
  print,'BAD FIT - BOMBING'
  goto,BOMB
endif
print,'Nord=2  RMS=',stringize(trans2.rms,ndec=3),' arcsec'
trans = trans2

; Fitting, NORD=3
if maxord ge 3 and n_elements(usno2) gt 9 then begin

  FIT_RADEC,usno2,phot2,trans3,nord=3,init=trans2
  if size(trans3,/type) ne 8 then begin 
    print,'BAD FIT - BOMBING'
    goto,BOMB
  endif
  print,'Nord=3  RMS=',stringize(trans3.rms,ndec=3),' arcsec'

  ; Is this a better solution?
  if trans3.rms lt trans.rms then trans=trans3
endif

; Fitting, NORD=4
if maxord ge 4 and n_elements(usno2) gt 16 then begin

  FIT_RADEC,usno2,phot2,trans4,nord=4,init=trans3
  if size(trans4,/type) ne 8 then begin 
    print,'BAD FIT - BOMBING'
    goto,BOMB
  endif
  print,'Nord=4  RMS=',stringize(trans4.rms,ndec=3),' arcsec'

  ; Is this a better solution?
  if trans4.rms lt trans.rms then trans=trans4
endif

; If we have few stars then keep it to a low order
nusno2 = n_elements(usno2)
if nusno2 lt 50. then trans=trans3                           ; at maximum third order
if nusno2 lt 50. and trans2.rms lt 0.25 then trans=trans2    ; 2nd order if good enough

print,'Final RMS = ',stringize(trans.rms,ndec=3),' arcsec'

; Checking that this solution is satisfactory
rmslim = 0.40   ; arcsec
if trans.rms gt rmslim then begin
  print,'SOLUTION IS NOT GOOD ENOUGH'
endif


; Convert the coordinates for the best stars
TRANS_RADEC,trans,phot2.x,phot2.y,newra,newdec
phot2.ra = newra
phot2.dec = newdec

; Convert the coordinates for the entire PHOT structure
TRANS_RADEC,trans,phot.x,phot.y,newra,newdec
phot.ra = newra            ; leave it in degrees
phot.dec = newdec

print,''

; Write the final output file.
outfile = dir+'/'+filebase+'.ast'
print,'FINAL Photometry File = ',filebase+'.ast'
WRITE_PHOT,phot,outfile,/colhead,/both

; Plotting residuals
if keyword_set(plotresid) then begin
  ; Get the boundary information
  xb = [0.,nx-1.,nx-1.,0.]
  yb = [0.,0.,ny-1.,ny-1.]
  
  ; Boundary coordinates
  ;HEAD_XYAD,head,xb,yb,rab,decb,/degree

  ; Transform to the new coordinates
  TRANS_RADEC,trans,xb,yb,rab2,decb2
  xr = reverse(minmax(rab2))
  yr = minmax(decb2)

  ; Plot the residuals
  plot_astrom_resid,usno2.raj2000,usno2.dej2000,phot2.ra,phot2.dec,xr=xr,yr=yr,title=filebase
endif

BOMB:

; Saving the structures
datfile = dir+'/astrom/'+filebase+'_astrom.dat'
print,'DAT File = ','astrom/'+filebase+'_astrom.dat'
if n_elements(usno) eq 0 then usno=-1
if n_elements(phot) eq 0 then phot=-1
if n_elements(usno2) eq 0 then usno2=-1
if n_elements(phot2) eq 0 then phot2=-1
if n_elements(ugd2) eq 0 then ugd2=-1
if n_elements(pgd2) eq 0 then pgd2=-1
if n_elements(trans) eq 0 then trans=-1
save,usno,phot,usno2,phot2,ugd2,pgd2,trans,file=datfile

; Close the logfile
journal
print,'LOG File = ','astrom/'+filebase+'_astrom.log'

if keyword_set(stp) then stop

end
