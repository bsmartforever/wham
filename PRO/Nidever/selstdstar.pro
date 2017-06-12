pro selstdstar,apfile,coordfile,lstfile,outfile

; This is an IDL version of Tony's selstdstar.f FORTRAN program
; This program gets the aperture photometry for standard stars

if n_elements(apfile) eq 0 then begin
  apfile=''
  print,'Aperture photometry file ? (.ap)'
  read,apfile
endif

if n_elements(coordfile) eq 0 then begin
  coordfile=''
  print,'File with the coordinates of standard stars ?'
  read,coordfile
endif

if n_elements(lstfile) eq 0 then begin
  lstfile=''
  print,'Output file from PICKPSF task ? (.lst)'
  read,lstfile
endif

if n_elements(outfile) eq 0 then begin
  outfile=''
  print,'Output file ?'
  read,outfile
endif


; Load the apfile
test1 = file_test(apfile)

if test1 eq 0 then begin
  print,apfile,' DOES NOT EXIST'
  return
end

; Making the aperture data arrays
n = 10000.
id = fltarr(n)
x = fltarr(n)
y = fltarr(n)
mag = fltarr(n)
sky = fltarr(n)
err = fltarr(n)

line1='' & line2='' & dum=''
id1=0L & x1=0. & y1=0. & mag1=0.
sky1=0. & stdev=0. & skew=0. & err1=10.

openr,unit,/get_lun,apfile
readf,unit,line1
readf,unit,line2
readf,unit,dum

count = 0
while (~EOF(unit)) do begin
  readf,unit,dum
  readf,unit,format='',id1,x1,y1,mag1
  readf,unit,format='',sky1,stdev,skew,err1
  id(count) = id1
  x(count) = x1
  y(count) = y1
  mag(count) = mag1
  sky(count) = sky1
  err(count) = err1

  count = count+1
end

close,unit
free_lun,unit

; Chopping off the end
id = id[0:count-1]
x = x[0:count-1]
y = y[0:count-1]
mag = mag[0:count-1]
sky = sky[0:count-1]
err = err[0:count-1]


; Load the coordfile
test2 = file_test(coordfile)

if test2 eq 0 then begin
  print,coordfile,' DOES NOT EXIST'
  return
end
readcol,coordfile,num,cx,cy,format='I,F,F',/silent


; Match the standard stars to the photometry
nstd = n_elements(cx)
cid = intarr(nstd)
rmin = fltarr(nstd)

; Loop through the standard stars
for i=0,nstd-1 do begin
  r = sqrt( (cx[i]-x)^2. + (cy[i]-y)^2. )
  best = where(r eq min(r),nbest)
  rmin[i] = min(r)
  cid[i] = id[best[0]]
end


; Print out the data
if n_elements(outfile) eq 0 then begin
  print,'PLEASE ENTER OUTFILE'
  return
endif

openw,unit,/get_lun,outfile
printf,unit,line1
printf,unit,line2
printf,unit,''

; Loop through the standard stars
for i=0,nstd-1 do begin

  ; No "match"
  if (rmin[i] gt 5.0) then begin
    printf,unit,FORMAT='(1X,I5,4F9.3)',99999,9999.999,9999.999,99.999,99.999
    ;printf,unit,FORMAT='(1X,I5,4F9.3)',99999,99.999,99.999,99.999,99.999
    printf,unit,''
  ; Found a "match"
  endif else begin
    ind = where(id eq cid[i],nind)
    ind = ind[0]
    printf,unit,format='(1X,I5,4F9.3)',id[ind],x[ind],y[ind],mag[ind],sky[ind]
    printf,unit,''
  endelse

end


; Load the lstfile
test4 = file_test(lstfile)

if test4 eq 0 then begin
  print,lstfile,' DOES NOT EXIST'
  return
end
readcol,lstfile,id2,x2,y2,mag2,sky2,format='I,F,F,F,F',skip=3,/silent


; Printing these lines to the output file as well
; WHY IS THIS NECESSARY????
nid2 = n_elements(id2)

for i=0,nid2-1 do begin
  printf,unit,format='(1X,I5,4F9.3)',id2[i],x2[i],y2[i],mag2[i],sky2[i]
  printf,unit,''
end

; Close the output file
close,unit
free_lun,unit


;stop

;      PARAMETER(n=10000)
;      INTEGER i,j,id(n),cid(n),m,mm
;      CHARACTER*30 apfile,coordfile,lstfile,outfile
;      CHARACTER*69 line1,line2
;      REAL x(n),y(n),mag(n),sky(n),err(n),stdev,skew
;      REAL cx(100),cy(100),r,rold
;
;      PRINT *,'Aperture photometry file ? (.ap)'
;      READ(*,'(A30)') apfile
;      PRINT *,'File with the coordinates of standard stars ?'
;      READ(*,'(A30)') coordfile
;      PRINT *,'Output file from PICKPSF task ? (.lst)'
;      READ(*,'(A30)') lstfile
;      PRINT *,''
;      PRINT *,'Output file ?'
;      READ(*,'(A30)') outfile
;      
;      OPEN(11,FILE=apfile)
;      OPEN(12,FILE=coordfile)
;      OPEN(13,FILE=lstfile)
;      OPEN(14,FILE=outfile)
;
;      READ(11,'(A69)') line1
;      READ(11,'(A69)') line2
;      READ(11,*)
;      WRITE(14,'(A69)') line1
;      WRITE(14,'(A69)') line2
;      WRITE(14,*)
;      
;      DO i = 1, n
;         READ(11,*,END=8)
;         READ(11,*) id(i),x(i),y(i),mag(i)
;         READ(11,*) sky(i),stdev,skew,err(i)
;         m = m + 1
;      ENDDO
;8     CLOSE(11)
;      
;      READ(12,*)
;      DO i = 1, 100
;         READ(12,'(F7.2,F8.2)',END=9) cx(i),cy(i)
;         mm = mm + 1
;      ENDDO
;9     CLOSE(12)
;      
;      DO i = 1, mm
;         rold = 10.
;         DO j = 1, m
;            r = SQRT((cx(i)-x(j))**2 + (cy(i)-y(j))**2)
;            IF (r.LT.2. .AND. r.LT.rold) THEN
;               rold = r
;               cid(i) = id(j)
;            ENDIF
;         ENDDO
;         IF (rold.GT.2.) cid(i) = 0
;      ENDDO
;      
;      DO i = 1, mm
;         IF (cid(i).EQ.0) THEN
;            WRITE(14,50) 99999,99.999,99.999,99.999,99.999
;            WRITE(14,*)
;         ELSE
;            DO j = 1, m
;               IF (cid(i).EQ.id(j)) THEN
;                  WRITE(14,50) id(j),x(j),y(j),mag(j),sky(j)
;                  WRITE(14,*)
;               ENDIF
;            ENDDO
;         ENDIF
;      ENDDO
;50    FORMAT(1X,I5,4F9.3)            
;      
;      READ(13,'(2(/))')
;      DO i = 1, n
;         READ(13,*,END=99) id(i),x(i),y(i),mag(i),sky(i)
;         WRITE(14,50) id(i),x(i),y(i),mag(i),sky(i)
;         WRITE(14,*)
;      ENDDO
;99    CLOSE(13)
;      CLOSE(14)
;
;      STOP
;      END

end
