pro mkwtemplate
;==============================================================================
;
;     MKTEMPLATE.PRO
;
;     This will automate the creation of a template file for use with
;     PHOTINFO.PRO.  These two tasks together create the files necessary
;     for a photometric reduction with Majewski's photom2.f code.
;    
;     This program is designed to create template files only for standard
;     stars taken from Landolt's 1992 catalog (AJ 104, 340), and the file
;     Landolt_92 (which includes all of the data from table 2 of the article)
;     is necessary for the program to function properly.  
;
;     The program will read in the data from the Landolt table and output it
;     to a *.template file, which is input to the PHOTINFO.PRO routine.
;
;     CALLING SEQUENCE:  mktemplate
;
;              landir = string with the UNIX path to the directory where
;                       the file Landolt_92 is located.
;              field =  Name of the standard field you want a template
;                       created for.  (example RU149, SA92, PG0323, etc.)
;              filter = Which filter you are currently calibrating (U,B,V,R,I)
;              color  = Which color you would like used for the color term in
;                       the photometric calibration.  (e.g. for calibrating of
;                       I data, do you want to use V-I or R-I?
;                       the color should always be of the format color ='V-I'
;              runnum = The first column in the template file is a 'running number'
;                       runnum is the integer equal to what you would like the
;                       first running number to be.
;
;     Note:  The template will be created with all of the standards in a field
;            sorted in RA order (as they appear in table 2 of Landolt), so if
;            your .mag.1 file was created with the stars in a different order,
;            you will have to rearrange the order of the output .template file.
;
;     History:   5/19/97:  written by CP
;
;===============================================================================
print,format='($,"Enter directory containing Geisler data ")'
landir='                                                                        '
read,landir
print,format='($,"Enter standard field (e.g. RU149) you are calibrating ")'
field='                     '
read,field
print,format='($,"Enter filter (M,C,T2,T1,DDO51) which you are calibrating ")'
filter=' '
read,filter
print,format='($,"Enter color (M-T2,etc.) which you are calibrating ")'
color='      '
read,color
print,format='($,"Enter the running number for the first star in the field ")'
runnum=0
read,runnum
;
doug=landir+field+'.cat'
;field=strcompress(field,/remove_all)
color=strcompress(color,/remove_all)
color=strupcase(color)
filter=strupcase(filter)
filename='  '
;
;  Read in the data
;
readcol,doug,id,t1,t1e,t1t2,t1t2e,mt1,mt1e,m51,m51e,cm,cme,format='i,d,d,d,d,d,d,d,d,d,d'
;
;  The information which will be written to the output file is the
;  magnitude, color, error in the magnitude, error in the color, and
;  the name of the star associated with those values.
;  Below we define arrays for the standard star data and fill them for
;  the stars in the field the user selected.
;
;  Note the error in the magnitude for all filters except V is calculated 
;  with the quadrature sum of the V magnitude and the color terms used to
;  calculate the magnitude.  e.g.  If you want the filter 'B', the error in
;  the B magnitude is the square root of (error in V)^2 + (error in B-V)^2 
;  since B magnitude = V + B-V
;
mag=fltarr(n_elements(id))
col=fltarr(n_elements(id))
err1=fltarr(n_elements(id))
err2=fltarr(n_elements(id))
if (filter eq 'DDO51') then begin
    for a=0,n_elements(id)-1 do begin
        if m51[a] lt 99.9 then begin
            mag[a]=t1[a]+mt1[a]-m51[a]
            err1[a]=sqrt(t1e[a]^2+mt1e[a]^2+m51e[a]^2)
        endif else begin
            mag[a]=99.999
            err1[a]=9.999
        endelse
    endfor
endif
if (filter eq 'C') then begin
    mag=t1+mt1+cm
    err1=sqrt(t1e^2+mt1e^2+cme^2)
endif
if (filter eq 'M') then begin
    mag=t1+mt1 
    err1=sqrt(t1e^2+mt1e^2)
endif
if (filter eq 'T2') then begin
    mag=t1-t1t2
    err1=sqrt(t1e^2+t1t2e^2)
endif
if (filter eq 'T1') then begin
    mag=t1
    err1=t1e
endif
;
if (color eq 'T1-T2') then begin
    col=t1t2
    err2=t1t2e
endif 
if (color eq 'M-T1') then begin
    col=mt1
    err2=mt1e
endif
if (color eq 'M-T2') then begin
    col=mt1+t1t2
    err2=sqrt(mt1e^2+t1t2e^2)
endif
if (color eq 'M-DDO51') then begin
    for a=0,n_elements(id)-1 do begin
        if m51[a] lt 99.9 then begin
            col[a]=m51[a]
            err2[a]=m51e[a]
        endif else begin
            col[a]=99.999
            err2[a]=9.999
        endelse
    endfor
endif
if (color eq 'C-M') then begin
    col=cm
    err2=cme
endif
;
;  Next, simply write all of this out to the file *.template.
;
;
filename=field+filter+'.template'
get_lun,unit
openw,unit,filename
for zzz=0,n_elements(id)-1 do begin
    printf,unit,format='(I4,1x,F7.4,1x,F6.3,1x,F7.4,1x,F7.4,1x,A12)',runnum,mag(zzz),$
          col(zzz),err1(zzz),err2(zzz),id(zzz)
    runnum=runnum+1
endfor
close,unit
free_lun,unit
;
return
end
