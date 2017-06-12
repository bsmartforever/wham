;+
; SDSS_SEARCH
;
; This program queries the local SDSS point source catalog
; and outputs the results to an ASCII file.
;
; INPUTS:
;  outfile       The final output filename
;  =rarange      A two element array of the RA range to use
;  =decrage      A two element array of the DEC range to use
;  =flags_on     An array of the flag indices to set ON
;  =flags_off    An array of the flag indices to set OFF
;  =cut          An extra cut to make including flag cuts
;  =filecut      Name of file from which to read the cut
;  /cleanphot    Only use "clean" photometry
;
;  /flagprint    Prints out flag names (no query is performed)
;  /flagdes      Prints out flag descriptions (no query is performed)
;  /fieldprint   Prints out field names (no query is performed)
;  /fielddes     Prints out field descriptions (no query is performed)
;
; OUTPUTS:
;  The output records out written to "outfile"
;
;
; EXAMPLE:
;  IDL>sdss_search,'query.dat',rar=[180,190],decr=[10,15],$
;      cut='g<15.0',flags_on=[1,3]
;
;
; QUERY ESTIMATES.
;  An estimate is the total number of 
;
; MAKING CUTS:
;  There are several ways to make cuts in the data.
;  (1) rarange and decrange
;      The easiest way to specify the RA/DEC range is using the
;      rarange and decrange parameters.
;      For example, rar=[181,190],decr[3.5,11.7] will only select
;      stars in the range 181 <= RA < 190, 3.5 <= DEC < 11.7
;
;  (2) The "cut" input
;      You can use the "cut" parameter to input any combination
;      of cuts to use in the query.
;      For example, cut='g<15.5 and (g-i)<1.4 and saturated=1'
;      The logical operators that can be used are: AND, OR, && and ||.
;      The comparison operators that can be used are: ==, <=, >=, !=
;      Quality flags can also be set using the "cut" input, see below.
;
;  (3) The "filecut" input
;      A cut can be loaded from a file.  Setting "filecut" to the name
;      of the file containing the cut will load that file.  The cut
;      will be interpretted the same as if it were input as the "cut"
;      from the command line.  This is useful for often used or long
;      cuts.
;      For example, filecut='cut1.txt', will load the text in the file
;      "cut1.txt" and use it as the cut for the query.
;
;
; QUALITY FLAGS:
;  There are three ways to use the quality flags
;  (1) Using the "cut" input.
;      You can specify what the flags should be using the flag names.
;      For example, cut='SATURATED=1' will only return stars that 
;      that have the SATURATED flag set to ON.
;      =1 means that the flag is set to ON and =0 means the flag is
;      set to OFF.
;      Use /flagprint or /flagdes to get the flag names and
;      descriptions.
;
;  (2) Using "flags_on" and "flags_off".
;      You can input flag index numbers into flag_on or flag_off to
;      specify that they should be set to ON or OFF.
;      For example, flags_on=[1,6], flags_off=[13,16] will only return
;      stars that have the CANONICAL_CENTER (1) and PEAKCENTER (6)
;      flags set to ON, and the COSMIC_RAY (13) and BAD_RADIAL (16)
;      flags set to OFF.
;      Use /flagprint or /flagdes to get the flag names, indices, and
;      descriptions.
;
;  (3) Using each flag's keyword.
;      Each flag can be specified with the command line keyword
;      bearing the flag's name.
;      For example, SATURATED=1,NOPROFILE=0 will select only stars
;      that have the SATURATED flag set to ON and the NOPROFILE flag
;      set to OFF.
;      Use /flagprint or /flagdes to get the flag names, indices, and
;      descriptions.
;
; LOADING/USING THE OUTPUT DATA;
;  The output is saved to an ASCII file.  The data can be loaded in
;  IDL with the importascii.pro program (get the most recent version
;  from http://www.astro.virginia.edu/~dln5q/research/idl/)
;  The follow command will load the data into a structure called "sdss"
;  IDL>sdss = importascii('sdss_search.out',/header)
;
;  The output can also be loaded in the IRAF chart program.
;  The dbformat.sdss and keys.sdss files are in the
;  /net/halo/dln5q/sdss/data/ directory.
;  To load the data in IRAF make sure you have loaded the "mtools"
;  package and type:
;  mtools>chart sdss_search.out dbformat=dbformat.sdss keys=keys.sdss
;
;
; THE CATALOG:
;  The local SDSS DR5 point source catalog contains 85,383,971 stars.
;  See /net/halo/dln5q/sdss/data/apjs2007_adelman-mccarthy_sdssdr5.pdf
;  for a description of the SDSS DR5 catalog or look at the website:
;  http://www.sdss.org/dr5/
;
;
; By D.Nidever  December 2006
;-

;------------------------------------------------------------

; COUNTER
pro counter,num,outof,infostring $
            ,wait_time = waittime $
            ,percent=percent      $
            ,clear=clear
on_error,2
clearline = string("15b)          ;get "15b character to create a fresh line
if keyword_set(clear) then begin
    len = strlen(infostring)
    print,clearline,format='('+str(len)+'x, a)'
endif else begin
    if n_elements(infostring) eq 0 then infostring = 'Number ' 

    lenst = strtrim(strlen(infostring),2)
    leni = strtrim(strlen(strtrim(num,2)),2)
    leno = strtrim(strlen(strtrim(outof,2)),2)
    if keyword_set(percent) then begin
        per = strtrim(round(float(num)*100./outof),2)
        lenp = strtrim(strlen(strtrim(per,2)),2)
        form="($,a"+lenp+",' % Completed',a,a)"
        print,form = form, per, '         ', clearline
    endif else begin
        form="($,a"+lenst+",i"+leni+",' of ',i"+leno+",a,a)"
        print,form = form, infostring, num, outof, '         ',clearline
    endelse
endelse 
if n_elements(waittime) gt 0 then wait,waittime
end

;------------------------------------------------------------

pro sdss_search,outfile,rarange=rarange,decrange=decrange,cut=cut,filecut=filecut,flags_on=flags_on,$
                flags_off=flags_off,flagcut=flagcut,flagprint=flagprint,flagdes=flagdes,$
                fieldprint=fieldprint,fielddes=fielddes,cleanphot=cleanphot,stp=stp,_EXTRA=extra

; Data directory
dir = '/net/halo/dln5q/sdss/data/'

t00 = systime(1)

; Not enough inputs, print syntax
if n_params() eq 0 and n_elements(rar) eq 0 and n_elements(decr) eq 0 and $
  n_elements(cut) eq 0 and n_elements(flags_on) eq 0 and n_elements(flags_off) eq 0 and $
  n_elements(flagcut) eq 0 and n_elements(flagprint) eq 0 and n_elements(flagdes) eq 0 and $
  n_elements(fieldprint) eq 0 and n_elements(fielddes) eq 0 then begin
  print,'Syntax - sdss_search,outfile,rarange=rarange,decrange=decrange,cut=cut,filecut=filecut,flags_on=flags_on,'
  print,'                flags_off=flags_off,flagcut=flagcut,flagprint=flagprint,flagdes=flagdes,'
  print,'                fieldprint=fieldprint,fielddes=fielddes,cleanphot=cleanphot,stp=stp'
  print,'Type IDL>man,"sdsss_query" for more information'
  return
end

; CATCH ERRORS
CATCH, error_status

IF (Error_status NE 0) THEN BEGIN  
   PRINT, 'ERROR index: ', Error_status  
   PRINT, 'ERROR message: ', !ERROR_STATE.MSG  
   CATCH, /CANCEL
   return  
ENDIF  


; #######################################################
; # PRINTING INFORMATION
; #######################################################

; Short printout of flags
if keyword_set(flagprint) then begin
  ; Short version, just flag names and numbers
  restore,dir+'sdss_flags.dat'
  nflags = n_elements(flags)
  for i=0,nflags-1 do print,format='(A26,I5)',flags[i],i+1
  goto,bomb
endif

; Print out the flags, LONG
if keyword_set(flagdes) then begin
  ; Print out the long version with the descriptions
  spawn,'cat '+dir+'sdss_flaginfo.txt'
  goto,bomb
end

; Print out the field/column names
if keyword_set(fieldprint) then begin
  print,'FIELDS: objid, ra, dec, dered_u (u), err_u, dered_g (g), err_g, dered_r (r), err_r, '
  print,'        dered_i (i), err_i, dered_z (z), err_z, flags, delta, match, propermotion (pm), angle'
  goto,bomb
endif

; Print out the field descriptions
if keyword_set(fielddes) then begin
  spawn,'cat '+dir+'sdss_fieldinfo.txt'
  goto,bomb
endif


; Default output filename
if n_elements(outfile) eq 0 then outfile = 'sdss_search.out'
print,'Output filename = ',outfile

; If output file exists then delete it
info = file_info(outfile)
if info.exists eq 1 then file_delete,outfile,/quiet



; FILECUT
;----------------------------
; Getting cut from a file
if n_elements(filecut) gt 0 then begin

  test = file_search(filecut)
  ; File does not exist
  if test(0) eq '' then begin
    print,'ERROR: FILE ',filecut,' DOES NOT EXIST'
    return
  endif

  ; Reading the cut from the file
  openr,unit,/get_lun,filecut
  if n_elements(cut) eq 0 then cut=''
  line = ''
  while (~eof(unit)) do begin
    readf,unit,line
    cut = cut+' '+line
  end

endif ; filecut


; #######################################################
; PREPARE the cut for awk
; #######################################################
; Replace the column names in the cut with $1, $2, etc.
; objid  ra  dec  dered_u  err_u  dered_g  err_g  dered_r  err_r dered_i  err_i  dered_z  err_z  flags  delta  match  propermotion angle
; the flags need to be queries separately
if n_elements(cut) gt 0 then begin

  cut2 = strlowcase(cut)  ; use lowercase

  ; Restoring the flag information
  restore,dir+'sdss_flags.dat'

  ; Replace logical operators, AND, OR
  origname3 = ['and','or']
  replname3 = ['&&','||'] 
  nrepl3 = n_elements(replname3)
  for i=0,nrepl3-1 do cut2 = repstr(cut2,origname3[i],replname3[i])

  ; Removing leading/trailing spaces from '='
  for i=0,2 do cut2 = repstr(cut2,' =','=')
  for i=0,2 do cut2 = repstr(cut2,'= ','=')
  
  nflags = n_elements(flags)
  ; Loop through the flags  
  for i=0L,nflags-1 do begin 
    num = 2.0d0^i
    ;num = 2LL^(i+1LL)
    ;num = strtrim(num,2)
    num = strtrim(string(num,format='(I40)'),2)
    
    ; Replacing flag ON
    origstr = strlowcase(flags[i])+'=1'
    newstr = ' (and($14,'+num+')!=0) ' 
    cut2 = repstr(cut2,origstr,newstr)
 
    ; Replacing flag OFF
    origstr = strlowcase(flags[i])+'=0'
    newstr = ' (and($14,'+num+')==0) '
    cut2 = repstr(cut2,origstr,newstr)
  end


  ; Replacing column names with awk column variable names (i.e. $1, $2. etc.
  origname = ['objid','ra','dec','dered_u','err_u','dered_g','err_g','dered_r','err_r',$
              'dered_i','err_i','dered_z','err_z','delta','match','propermotion','angle']
  replname = ['$1','$2','$3','$4','$5','$6','$7','$8','$9','$10','$11','$12','$13','$15','$16','$17','$18']
  nrepl = n_elements(replname)
  for i=0,nrepl-1 do cut2 = repstr(cut2,origname[i],replname[i])

  ; Now replace short names, u, g, r, i, z, pm
  origname2 = ['u','g','r','i','z','pm']
  replname2 = ['$4','$6','$8','$10','$12','$17']
  nrepl2 = n_elements(replname2)
  for i=0,nrepl2-1 do cut2 = repstr(cut2,origname2[i],replname2[i])

endif else begin
  cut=''
  cut2 = '1==1'
endelse


; ##############################
; # PREPARE the flag cut
; ##############################
;
; Allow three types of flag inputs
; 1.) flags_on =[] and flags_off=[]
; 2.) saturated=1,binned1=0   (command line inputs, use the _EXTRA parameter)
; 3.) flagcut = 'saturated=1 or binned1=0'
;     This is now covered in the normal "cut"

restore,dir+'sdss_flags.dat'

; FIRST type of flag cut, FLAGS_ON, FLAGS_OFF
;---------------------------------------------
if n_elements(flags_on) gt 0 then begin
  if n_elements(cut2) eq 0 then cut2=''

  nflags_on = n_elements(flags_on)
  flags_on_names = strarr(nflags_on)

  ; Looping through the flags
  for i=0,nflags_on-1 do begin
    flags_on_names[i] = flags[flags_on[i]-1]    
    ;num = string(flags_on[i]+1,format='(Z)')
    num = 2.0d0^double(flags_on[i]-1L)  ; start with 2^0=1
    ;num = strtrim(long64(num),2)
    num = strtrim(string(num,format='(I40)'),2)
    cut2 = cut2 + '&& (and($14,'+num+')!=0) '
  end

  ; Print out flags ON
  print,'FLAGS ON: ',flags_on_names

endif ; flags_on

if keyword_set(flags_off) then begin
  if n_elements(cut2) eq 0 then cut2=''

  nflags_off = n_elements(flags_off)
  flags_off_names = strarr(nflags_off)

  ; Looping through the flags
  for i=0,nflags_off-1 do begin
    flags_off_names[i] = flags[flags_off[i]-1]
    ;num = string(flags_off[i]+1,format='(Z)')
    num = 2.0d0^double(flags_off[i]-1L)  ; start with 2^0=1
    ;num = strtrim(long64(num),2)
    num = strtrim(string(num,format='(I40)'),2)
    cut2 = cut2 + '&& (and($14,'+num+')==0) '
  end

  ; Print out flags OFf
  print,'FLAGS OFF: ',flags_off_names

endif ; flags_off


; SECOND type of flag cut, the EXTRA parameter
;---------------------------------------------
if (n_elements(extra) gt 0) then begin

  tags = tag_names(extra)
  ntags = n_elements(tags)

  fcount = 0

  ; Looping through the extra tags
  for i=0,ntags-1 do begin
    gd = where(strlowcase(flags) eq strlowcase(tags[i]),ngd)

    ; This is a legitimate flag name
    if (ngd gt 0) then begin

      flagname = flags[gd[0]]
      flagvalue = extra.(i)
      if (fcount eq 0) then begin
        flags_extra_names = flagname
        flags_extra_value = flagvalue
      endif else begin
        flags_extra_names = [flags_extra_names,flagname]
        flags_extra_value = [flags_extra_value,flagvalue]        
      endelse

      ; Getting the number for this bit
      num = 2.0d0^(gd[0])   ; start with 2^0=1
      ;num = 2LL^(gd[0]+1LL)
      ;num = strtrim(num,2)
      num = strtrim(string(num,format='(I40)'),2)

      ; Flag OFF
      if flagvalue eq 0 then begin
        cut2 = cut2 + '&& (and($14,'+num+')==0) '

      ; Flag ON
      endif else begin
        cut2 = cut2 + '&& (and($14,'+num+')!=0) '
      endelse  

      ; Increment the flag counter
      fcount = fcount+1
    endif
  end

  ; Print out the flags
  outstr = ''
  for i=0,fcount-1 do begin
    outstr = flags_extra_names[i]+'='+strtrim(flags_extra_value,2)
    if i ne fcount-1 then outstr=outstr+', '
  end

  print,'KEYWORD FLAGS: ',outstr

end

;##########################
;# ERROR CHECKING
;##########################

; test the cut on a small file, tail the first file
; to check for errors


; Remove all the and's and then check for letters.
; There shouldn't be any
test = cut2
test = repstr(test,'and','')
dum = stregex(test,'[a-z]',/fold_case,/boolean)

; There are some letters left
if dum ne 0 then begin

  ; Getting all the bad strings
  lo = 0
  test2 = test
  bad=''
  while (lo ne -1) do begin
    lo = stregex(test2,'[a-z]+',/fold_case,length=len)
    bad1 = stregex(test2,'[a-z]+',/fold_case,/extract)
    if lo ne -1 then bad=bad+' '+bad1
    test2 = strmid(test2,0,lo) + strmid(test2,lo+len,strlen(test)-lo-len)
  end

  print,'ERROR: THE QUERY CONTAINS NON-EXISTANT OR MISSPELLED FIELDS/FLAGS -- ',bad
  return
endif


; Figure out which files to search
; Load coordinates of "bricks" from a file
;files = dir+['sdss_355-15.out']
restore,dir+'sdss_files.dat'
nfiles = n_elements(files)

; Search the whole sky by default
if n_elements(rarange) gt 0 then rar2=rarange else rar2=[0,360]
if n_elements(decrange) gt 0 then decr2=decrange else decr2=[-90,90]

ind = where((ra+5.0) gt rar2[0] and ra le rar2[1] and (dec+5.0) gt decr2[0] and dec le decr2[1],nind)

; Nothing to search
if nind eq 0 then begin
  print,'Search outside the SDSS survey area'
  goto,bomb
end

print,strtrim(nind,2),' Files to Search'


; Print the query parameters
print,'--------------------'
print,'Query Parameters:'
print,'--------------------'
if n_elements(rar2) gt 0 then begin
  print,strtrim(rar2[0],2)+' <= RA < '+strtrim(rar2[1],2)
endif
if n_elements(decr2) gt 0 then begin
  print,strtrim(decr2[0],2)+' <= DEC < '+strtrim(decr2[1],2)
endif
if n_elements(flagcut) gt 0 then begin
  print,'FLAG CUT = ',flagcut
endif
if n_elements(cut) gt 0 then begin
  print,'CUT = ',cut
endif
if keyword_set(cleanphot) then print,'CLEAN PHOTOMETRY'
print,'--------------------'
print,''


; CLEAN PHOTOMETRY
;---------------------------
; This is copied from the SDSS website:
; http://cas.sdss.org/astrodr5/en/help/docs/realquery.asp#flags
if keyword_set(cleanphot) then begin
  if n_elements(cut2) eq 0 then cut2='' else cut2=cut2+' && '

  ;#-- detected in BINNED1
  cut2 = cut2 + 'and($14,0x10000000)!=0 '

  ;#-- not EDGE, NOPROFILE, PEAKCENTER, NOTCHECKED, PSF_FLUX_INTERP,
  ;#-- SATURATED, or BAD_COUNTS_ERROR
  cut2 = cut2 + '&& and($14,0x8100000c00a4)==0 '

  ;#-- not DEBLEND_NOPEAK or small PSF error
  ;#-- (substitute psfmagerr in other band as appropriate)
  cut2 = cut2 + '&& ( and($14,0x400000000000)==0 || (err_g<=0.2) ) '

  ;#-- not INTERP_CENTER or not COSMIC_RAY
  cut2 = cut2 + '&& ( and($14,0x100000000000)==0 || and($14,0x1000)==0 ) '
end ; clean photometry


; Print the header to the final output file
spawn,'head -1 '+dir+files(0)+' > '+outfile,dum

checked = 0

;#######################################
;# LOOP THROUGH THE FILES TO SEARCH
;#######################################
FOR i=0L,nind-1 DO BEGIN

  ; The current file to search
  infile = dir+files(ind(i))

  ; Start time
  t0 = systime(1)

  ;#################################
  ;# QUERY ESTIMATES
  ;#################################
  ; Print out an estimate of the time and size of the final output
  ; based on the output of the first brick (and scale by the number of
  ; sources in brick to be searched).
  nrecords = file_lines(outfile)-1LL
  if (checked eq 0 and nrecords gt 0 and nind gt 1) then begin

    ; Restoring info on the files
    restore,dir+'sdss_fileinfo.dat'

    ; Getting info on the first output file
    info = file_info(outfile)

    ; Getting the ratio of selected to total stars in this searched brick
    nrecords0 = finfo[ind(i)].nrecords
    ratio = float(nrecords)/float(nrecords0)       ; ratio of stars queried

    ; Ratio of area
    area = float(rar2[1]-rar2[0])*float(decr2[1]-decr2[0]) < 13775.
    aratio = area/(25.*nind)

    ; Estimates
    trecords = total(finfo(ind).nrecords)    ; total stars in the search area 
    tsize = total(finfo(ind).size)           ; total size in the search area
    frecords = trecords * ratio * aratio     ; final stars
    fsize = tsize * ratio * aratio           ; final size
    ftim = dt*(trecords/nrecords0)           ; final time


    ; Printing out the estimates
    print,'----------------------------'
    print,'QUERY ESTIMATES:'
    print,'----------------------------'

    ; File size
    szout = fsize
    szstr = ' B'
    if fsize gt 1d3 and fsize lt 1d6 then begin
      szout = fsize/1000.  
      szstr = ' KB'
    endif
    if fsize gt 1d6 and fsize lt 1d9 then begin
      szout = fsize/1d6
      szstr = ' MB'
    endif
    if fsize gt 1d9 then begin
      szout = fsize/1d9
      szstr = ' GB'
    endif
    len = strlen(strtrim(long64(szout),2))
    print,'Total File Size = ',string(szout,format='(F'+strtrim(len+3,2)+'.1)'),szstr

    ; Total stars
    len = strlen(strtrim(frecords,2))
    print,'Total Stars = ',string(frecords,format='(I'+strtrim(len+1,2)+')')

    ; Total time
    timstr = 'sec.'
    timout = ftim
    if ftim gt 60 then begin
      timout = ftim/60.
      timstr = 'min.'
    end
    timlen = strlen(strtrim(long64(timout),2))
    print,'Total Time:',string(timout,format='(F'+strtrim(timlen+3,2)+'.1)'),' ',timstr

    print,'----------------------------'
    print,''

    checked = 1
  endif

  ; Counter
  if i gt 0 then counter,i+1,nind,'Box '



  ;#############################################
  ;# PUTTING TOGETHER THE AWK COMMAND
  ;#############################################

  ; The Beginng of the awk command
  cmd = "awk '{if ("

  ; Make sure to skip the first line
  cmd = cmd+' (NR>1) '

  ;---------------
  ; THE CUTS

  ; Making the RA cut
  if n_elements(rar2) gt 0 then begin
    sra0 = strtrim(rar2[0],2)
    sra1 = strtrim(rar2[1],2)
    cmd = cmd +'&& ($2 >= '+sra0+' && $2 < '+sra1+') '
  endif

  ; Making the DEC cut
  if n_elements(decr2) gt 0 then begin
    sdec0 = strtrim(decr2[0],2)
    sdec1 = strtrim(decr2[1],2)
    cmd = cmd +'&& ($3 >= '+sdec0+' && $3 < '+sdec1+') '
  endif

  ; Making any other cut
  if n_elements(cut2) gt 0 then begin
    cmd = cmd +'&& ('+cut2+') '
  endif

  ; The final parenthesis
  cmd = cmd + ')'

  ;---------------------

  ; The Print statement 
  cmd = cmd+' print $0'

  ; The end of the awk command
  cmd = cmd+" }' - < "+infile+" >> "+outfile


  ; RUNNING the query
  spawn,cmd,out,errout


  ; Check the output for errors
  if errout[0] ne '' or n_elements(errout) gt 1 then begin
    print,'ERROR: PROBLEM WITH QUERY'
    print,errout
    return
  endif

  ; Time for this file
  dt = systime(1)-t0

END ; loop through files


;------------------------------------
; Print out statistics of the output
;------------------------------------

; Getting file info
info = file_info(outfile)
if info.exists eq 0 then print,'NO OBJECTS FOUND'


print,'======================'
print,'QUERY RESULTS'
print,'----------------------'


; File size
sz = info.size
szout = sz
szstr = ' B'
if sz gt 1d3 and sz lt 1d6 then begin
  szout = sz/1000.
  szstr = ' KB'
endif
if sz gt 1d6 and sz lt 1d9 then begin
  szout = sz/1d6
  szstr = ' MB'
endif
if sz gt 1d9 then begin
  szout = sz/1d9
  szstr = ' GB'
endif
len = strlen(strtrim(long64(szout),2))
print,'File Size = ',string(szout,format='(F'+strtrim(len+3,2)+'.1)'),szstr


; # of Records
if info.exists ne 0 then begin
  nrecords = file_lines(outfile)-1LL
  len = strlen(strtrim(nrecords,2))
  print,'Nstars = ',string(nrecords,format='(I'+strtrim(len+1,2)+')')
endif else begin
  print,'Nstars = 0'
endelse


; Print out the time it took for the query.
; seconds or minutes
tim = systime(1)-t00
timout = tim
timstr = 'sec.'
if tim gt 60 then begin
  timout = tim/60.
  timstr = 'min.'
end
len = strlen(strtrim(long64(timout),2))
print,'Time = ',string(timout,format='(F'+strtrim(len+3,2)+'.1)'),' ',timstr


print,'======================'

BOMB:

if keyword_set(stp) then stop

end
