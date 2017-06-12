;+
;Kenneth Sembach
;				IMSAVE.PRO
;				Version 5.2
;Created: 09/01/89
;Last Revision:	05/02/99
;
;Program Description:
;	This procedure saves data files in ASCII format with an IMNORM
;	header.  The file can be read by IMREAD.
;
;Restrictions:
;	All paprameters must be passed for proper execution.
;
;Screen Output:  
;	Text
;
;Use:
;	IMSAVE,root,x,y,object,comment,ion,wavc,mapi,order,rflags,updates
;
;On Input:
;		root	:== root of file name to be written (.dat assumed)
;		x	:== x coordinate array
;		y	:== y coordinate array
;		object	:== object comment
;		ion	:== ion comment
;		wavc	:== rest wavelength
;		mapi    :== (Data type:  0 = IUE,  -1 = GHRS,  -5 = CAT/CES
;			     -8 = NRAO 140ft, -7 = STIS, -9 = Coude Feed)
;		order	:== order number
;		r_flags	:== 3 element array containing the axis flag, smoothing
;			    flag, and tau flag, respectively
;		updates	:== string array containing update comments
;On Output:
;		root:	:== "BADFILE" if file is unreadable
;
;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
;	09/01/89  KRS	- Version 3.0 - IMWRITE
;	03/21/91  KRS   - Version 4.0, header comments added.
;	10/06/92  KRS	- Version 5.0, runs under Version 2 IDL.  Logical
;			  unit used to open file.  Error string printed 
;			  upon error. Renamed IMSAVE.
;	06/19/97  KRS 	- Version 5.1, parameter default for updates added.
;	05/02/99  KRS	- Version 5.2, documentation updated for distribution
;
;External Routines Called:
;	None
;----------------------------------------------------------------------------
PRO IMSAVE,root,x,y,object,comment,ion,wavc,mapi,order,rflags,updates, $
           OVERWRITE = overwrite

  ;;Overwrite keyword added by jch -- 11/20/02

IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imsave' & RETURN & ENDIF
  
  IF N_PARAMS() NE 11 THEN BEGIN
    updates = STRARR(80,1)
    updates(0) ='IMSAVE(v5.2):: File created '+!stime
  ENDIF
;
;Initial error control.
;
  ON_IOERROR,ESCAPE
;
;Form image name and open the image.  Check to see if ".dat" is already
;appended to filename.  Check also to see if file already exists!
;
  root = STRTRIM(root,2)
  len = STRLEN(root)
  IF STRUPCASE(STRMID(root,len-4,4)) NE '.DAT' THEN BEGIN
    rext = '.dat'
    ;;IF root EQ STRUPCASE(root) THEN rext = '.DAT'
  ENDIF ELSE rext=''
  exist = FINDFILE(root+rext)
  IF exist(0) NE '' AND NOT keyword_set(overwrite) THEN BEGIN
    PRINT,'IMSAVE(v5.2)::  Warning! (' +root+rext + ') already exists'
    over = ' '
    READ,'IMSAVE(v5.2)::  Overwrite file? ',over
    IF over NE 'y' THEN BEGIN
      PRINT,'IMSAVE(v5.2)::  File left intact'  &  !err = 1
      RETURN
    ENDIF
  ENDIF
  OPENW,unit,root+rext,/GET_LUN					
;
;Find the directory.  If the root name begins with a '/' (UNIX) then
;attach the working directory name to the root name.  If the root name 
;contains a '[' (VMS) then get the VMS directory name.
;
  CD,CURRENT=dir
  IF STRMID(root,0,1) EQ '/' THEN dir = ''
  IF STRMID(root,0,2) EQ '~/' THEN BEGIN
    dir = GETENV('HOME')
    root = STRMID(root,2,STRLEN(root))
  ENDIF
  IF STRMID(dir,0,1) EQ '/' THEN root = dir+'/'+root

  home = GETENV('HOME')
  IF STRMID(root,0,1) EQ '[' THEN BEGIN
    loc = STRPOS(home,'[')
    dir = STRMID(home,0,loc)
    root = dir + root
  ENDIF
  IF STRPOS(root,'[') GT 0 THEN dir=''
;
;Form filename out of directory and image name.
;
  filename = root+rext
;
;Find the number of data pairs flagged bad (y=-9.999).	
;
  badpairs = WHERE(y EQ -9.999)
  IF badpairs(0) EQ '-1' THEN BEGIN badpairs = 0
  ENDIF ELSE badpairs = N_ELEMENTS(badpairs)
;
;Print message to screen.
;
  PRINT,'IMSAVE(v5.2)::  Saving file: ',filename
;
;Find values of header keywords.
;
  h_object  = STRING(object)
  h_comment = STRING(comment)
  h_ion     = STRING(ion)
  h_wavc    = STRING(wavc,'(F8.3)')
  h_mapi    = STRING(mapi,'(I3)')
  h_order   = STRING(order,'(I3)')
  h_npts    = STRING(N_ELEMENTS(x),'(I4)')
  h_axflag  = STRING(rflags(0),'(I2)')
  h_smflag  = STRING(rflags(1),'(I2)')
  h_taflag  = STRING(rflags(2),'(I2)')
  xtitle    = STRING(!x.title,'(A40)')
  ytitle    = STRING(!y.title,'(A40)')
;
;Write the header to the new file.
;
  PRINTF,unit,filename
  IF mapi EQ -1 THEN BEGIN
    PRINTF,unit,'1-d GHRS image created '+!stime+' by IMSAVE(v5.2)'
  ENDIF ELSE IF mapi EQ 0 THEN BEGIN
    PRINTF,unit,'1-d IUE image created '+!stime+' by IMSAVE(v5.2)'
  ENDIF ELSE IF mapi EQ -5 THEN BEGIN
    PRINTF,unit,'1-d CAT/CES image created '+!stime+' by IMSAVE(v5.2)'
  ENDIF ELSE IF mapi EQ -8 THEN BEGIN
    PRINTF,unit,'1-d NRAO 140ft image created '+!stime+' by IMSAVE(v5.2)'
  ENDIF ELSE IF mapi EQ -7 THEN BEGIN
    PRINTF,unit,'1-d STIS image created '+!stime+' by IMSAVE(v5.2)'
  ENDIF ELSE IF mapi EQ -9 THEN BEGIN
    PRINTF,unit,'1-d Coude Feed image created '+!stime+' by IMSAVE(v5.2)'
  ENDIF ELSE BEGIN
    PRINTF,unit,'1-d Unknown image created '+!stime+' by IMSAVE(v5.2)'
  ENDELSE
  PRINTF,unit,STRTRIM(badpairs,2)+' data pairs flagged bad'
  PRINTF,unit,' '
  PRINTF,unit,'$(A8,A40,2x,A)','OBJECT =',h_object,'/ Object'
  PRINTF,unit,'$(A8,A40,2x,A)','COMMENT=',h_comment,'/ Object comment'
  PRINTF,unit,'$(A8,A40,2x,A)','ION    =',h_ion,'/ Ion'
  PRINTF,unit,'$(A8,A40,2x,A)','WAVC   =',h_wavc,'/ Lab wavelength'
  PRINTF,unit,'$(A8,A40,2x,A)','MAPI   =',h_mapi,'/ Original map number'
  PRINTF,unit,'$(A8,A40,2x,A)','ORDER  =',h_order,'/ Order number'
  PRINTF,unit,'$(A8,A40,2x,A)','NPTS   =',h_npts,'/ Length of array'
  PRINTF,unit,'$(A8,A40,2x,A)','XTITLE =',xtitle,'/ X axis title'
  PRINTF,unit,'$(A8,A40,2x,A)','YTITLE =',ytitle,'/ Y axis title'
  PRINTF,unit,'$(A8,A40,2x,A)','AXFLAG =',h_axflag,'/ Axis flag'
  PRINTF,unit,'$(A8,A40,2x,A)','SMFLAG =',h_smflag,'/ Smoothing flag'
  PRINTF,unit,'$(A8,A40,2x,A)','TAFLAG =',h_taflag,'/ Tau plot flag'
;
;Write update history.
;
  IF STRTRIM(updates(0),2) NE ';Update History:' THEN $
     PRINTF,unit,';Update History:'	
  nu = [SIZE(updates),-1]  &  nu = nu(3) > 1
  FOR j=0,nu-1 DO PRINTF,unit,'$(A)',updates(j)
  PRINTF,unit,';IMSAVE(v5.2)::  '+filename+' '+!stime
;
;Print line between header and data.
;
  PRINTF,unit,'-------------------------------------------------------------------------------'

;
;Check to see that x-vector is ASCENDING: -- jch (08/01/02)
;
  IF x[0] GT x[10] THEN BEGIN 
    x = reverse(x)
    y = reverse(y)
  ENDIF 

;
;Write the data to the new file.
;
  temp = [TRANSPOSE(x),TRANSPOSE(y)]
  PRINTF,unit,'$(2E16.8)',temp
;
;Return safely to the main program.
;
  CLOSE,unit  &  FREE_LUN,unit
  RETURN
;------------------------------------------------------------------------------
ESCAPE:
  CLOSE,unit  &  FREE_LUN,unit
  root = 'BADFILE'
  PRINT,'IMSAVE(v5.2)::  '+!err_string
RETURN  &  END
