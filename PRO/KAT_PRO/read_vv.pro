function read_vv, xpos, ypos, radius, radec=radec, $
   hour=hour, degree=degree, vmag=vmag, quiet=quiet

; Purpose:
;	Extract the name, RA (decimal hours), DEC, and Vmag of each 
;	target in the Vernon and Vernon catalogs into a structure. 
;
;	The locations of each caracter in the tables is found in the 
;	ReadMe document.
;
; Input:
;  xpos/ypos - Target coordinants. [Default Galactic Coordinants]
;  radius    - Search radius for background objects [Default 1 degree]
;
;  radec     - Specifies that xpos/ypos are in ra and dec. 
;           [Default ra units in hours]
;  degrees   - Sets ra units to degrees if radec is passed
;
;	Catalog website (select tar):
; 	http://cdsarc.u-strasbg.fr/viz-bin/Cat?VII/258
;
; By Dr. Kat Barger 04/2014 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;If radec is passed, then assume that glon=ra and glat=dec
;Note: the default is to assume that ra is in hours.
if keyword_set(radec) then glactc,xpos,ypos,2000,glon,glat,1,degree=degree $
else begin 
   glon=xpos & glat=ypos 
endelse 

   ;Assume that the radius is 1 degree if not otherwise stated. 
   if (NOT keyword_set(radius)) then radius=1.
   if (NOT keyword_set(vmag)) then vmag = 17.5


   dir='/d/data/VII_258/'
   file='agn.dat'

   ; Determine the number of rows in the file.
   rows = File_Lines(dir+file)

   ; Determine the number of colums in the file by reading
   ; the first line and parsing it into column units.
   OpenR, lun, dir+file, /Get_Lun
   line = ""
   ReadF, lun, line

   ; Find the number of columns in the line.

   ; Create a variable to hold the data.
   data = strarr(rows)

   ; Rewind the data file to its start.
   Point_Lun, lun, 0

   ; Read the data.
   ReadF, lun, data
   Free_Lun, lun

   ;name=strmid(data[0],3,19-3)
   ;ra=ten(float(strsplit(strmid(data[0],22,31-22),/extract)))
   ;if strmid(data[0],32,1) eq '-' then sign = -1. else sign = 1.
   ;dec=sign*ten(float(strsplit(strmid(data[0],34,41-34),/extract)))
   ;if validate_numeric(strmid(data[0],82,87-82)) then vmag_arr=float(strmid(data[0],82,87-82)) else vmag_arr=0
   ;struct=replicate({name:'',ra:0.0D,dec:0.0D,glon:0.0D,glat:0.0D,vmag:0.0,fuv:0.0,nuv:0.0},rows)

;VV_258 catalog
   struct_agn=replicate({name:'',ra:0.0D,dec:0.0D,glon:0.0D,glat:0.0D,vmag:0.0,z:0.0},rows)
   for i=0L, rows-1 do begin
   		struct_agn[i].name=strmid(data[i],2,19-2)
         ra_string=strmid(data[i],22,31-22)
   		;struct_agn[i].ra=ten(float(strsplit(strmid(data[i],22,31-22),/extract)))
   		if strmid(data[i],32,1) eq '-' then sign = -1. else sign = 1.
   		;struct_agn[i].dec=sign*ten(float(strsplit(strmid(data[i],34,41-34),/extract)))
         dec_string=strmid(data[i],32,1)+strmid(data[i],33,41-33)
         stringad,ra_string+' '+dec_string,ra,dec
         struct_agn[i].ra=ra
         struct_agn[i].dec=dec
   		if validate_numeric(strmid(data[i],82,87-82)) then vmag_arr=float(strmid(data[i],82,87-82)) else vmag_arr=0
   		struct_agn[i].vmag=vmag_arr
         struct_agn[i].z=float(strmid(data[i],71,75-71))
         ;ra in hours is default
         glactc,struct_agn[i].ra,struct_agn[i].dec,2000,glon_arr,glat_arr,1,/degree
         struct_agn[i].glon=glon_arr[0]
         struct_agn[i].glat=glat_arr[0]
   endfor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   dir='/d/data/VII_258/'
   file='qso.dat'

   ; Determine the number of rows in the file.
   rows = File_Lines(dir+file)

   ; Determine the number of colums in the file by reading
   ; the first line and parsing it into column units.
   OpenR, lun, dir+file, /Get_Lun
   line = ""
   ReadF, lun, line

   ; Find the number of columns in the line.

   ; Create a variable to hold the data.
   data = strarr(rows)

   ; Rewind the data file to its start.
   Point_Lun, lun, 0

   ; Read the data.
   ReadF, lun, data
   Free_Lun, lun

   ;name=strmid(data[0],3,19-3)
   ;ra=ten(float(strsplit(strmid(data[0],22,31-22),/extract)))
   ;if strmid(data[0],32,1) eq '-' then sign = -1. else sign = 1.
   ;dec=sign*ten(float(strsplit(strmid(data[0],34,41-34),/extract)))
   ;if validate_numeric(strmid(data[0],82,87-82)) then vmag_arr=float(strmid(data[0],82,87-82)) else vmag_arr=0
   ;struct=replicate({name:'',ra:0.0D,dec:0.0D,glon:0.0D,glat:0.0D,vmag:0.0,fuv:0.0,nuv:0.0},rows)

;VV_258 catalog
   struct_qso=replicate({name:'',ra:0.0D,dec:0.0D,glon:0.0D,glat:0.0D,vmag:0.0,z:0.0},rows)
   for i=0L, rows-1 do begin
         struct_qso[i].name=strmid(data[i],2,19-2)
         ra_string=strmid(data[i],22,31-22)
         ;struct_qso[i].ra=ten(float(strsplit(strmid(data[i],22,31-22),/extract)))
         if strmid(data[i],32,1) eq '-' then sign = -1. else sign = 1.
         ;struct_qso[i].dec=sign*ten(float(strsplit(strmid(data[i],34,41-34),/extract)))
         dec_string=strmid(data[i],32,1)+strmid(data[i],33,41-33)
         stringad,ra_string+' '+dec_string,ra,dec
         struct_qso[i].ra=ra
         struct_qso[i].dec=dec
         if validate_numeric(strmid(data[i],82,87-82)) then vmag_arr=float(strmid(data[i],82,87-82)) else vmag_arr=0
         struct_qso[i].vmag=vmag_arr
         struct_qso[i].z=float(strmid(data[i],71,75-71))
         ;ra in hours is default
         glactc,struct_qso[i].ra,struct_qso[i].dec,2000,glon_arr,glat_arr,1,/degree
         struct_qso[i].glon=glon_arr[0]
         struct_qso[i].glat=glat_arr[0]
   endfor

; 1187-1195  F9.4 3.63uJy    FFUV      ?=0 FUV flux (GALEX) in nmaggies (G1)
; 1208-1216  F9.4 3.63uJy    FNUV      ?=0 NUV flux (GALEX) in nmaggies (G1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   dir='/d/data/VII_270/'
   file='dr10q.dat' 

   ; Determine the number of rows in the file.
   rows = File_Lines(dir+file)

   ; Determine the number of colums in the file by reading
   ; the first line and parsing it into column units.
   OpenR, lun, dir+file, /Get_Lun
   line = ""
   ReadF, lun, line

   ; Find the number of columns in the line.

   ; Create a variable to hold the data.
   data = strarr(rows)

   ; Rewind the data file to its start.
   Point_Lun, lun, 0

   ; Read the data.
   ReadF, lun, data
   Free_Lun, lun

   ;VV_258 catalog
   struct_vv=replicate({name:'',ra:0.0D,dec:0.0D,glon:0.0D,glat:0.0D,vmag:0.0,z:0.0},rows)

   for i=0L, rows-1 do begin
         struct_vv[i].name=strmid(data[i],2,19-2)
         struct_vv[i].ra=double(strsplit(strmid(data[i],19,29-19),/extract))
         ;if strmid(data[i],32,1) eq '-' then sign = -1. else sign = 1.
         struct_vv[i].dec=double(strsplit(strmid(data[i],31,40-31),/extract))
         if float(strmid(data[i],801,812-801)) ne 9999.0 then vmag_arr=float(strmid(data[i],801,812-801)) else vmag_arr=0
         struct_vv[i].vmag=vmag_arr
         struct_vv[i].z=float(strmid(data[i],66,74-66))
         ;if validate_numeric(strmid(data[i],1208,1216-1208)) then nuv=float(strmid(data[i],1208,1216-1208)) else nuv=0
         ;struct_vv[i].nuv=nuv/3.63 ;in units of 3.63uJy, converting to uJy
         ;if validate_numeric(strmid(data[i],1187,1195-1187)) then fuv=float(strmid(data[i],1187,1195-1187)) else fuv=0
         ;struct_vv[i].fuv=fuv/3.63 ;in units of 3.63uJy, converting to uJy
         ;ra in hours is default
         glactc,struct_vv[i].ra,struct_vv[i].dec,2000,glon_arr,glat_arr,1,/degree
         struct_vv[i].glon=glon_arr[0]
         struct_vv[i].glat=glat_arr[0]
   endfor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   dir='/d/data/LQAC/'
   file='lqac2.dat' 

   ; Determine the number of rows in the file.
   rows = File_Lines(dir+file)

   ; Determine the number of colums in the file by reading
   ; the first line and parsing it into column units.
   OpenR, lun, dir+file, /Get_Lun
   line = ""
   ReadF, lun, line

   ; Find the number of columns in the line.

   ; Create a variable to hold the data.
   data = strarr(rows)

   ; Rewind the data file to its start.
   Point_Lun, lun, 0

   ; Read the data.
   ReadF, lun, data
   Free_Lun, lun

   struct_lqac=replicate({name:'',ra:0.0D,dec:0.0D,glon:0.0D,glat:0.0D,vmag:0.0,z:0.0},rows)

   for i=0L, rows-1 do begin
         struct_lqac[i].name=strmid(data[i],6,16-6)
         struct_lqac[i].ra=double(strsplit(strmid(data[i],18,31-18),/extract))
         struct_lqac[i].dec=double(strsplit(strmid(data[i],34,46-34),/extract))
         struct_lqac[i].vmag=min([float(strmid(data[i],108,113-108)),float(strmid(data[i],120,125-120))])
         struct_lqac[i].z=float(strmid(data[i],183,187-183))
         glactc,struct_lqac[i].ra,struct_lqac[i].dec,2000,glon_arr,glat_arr,1,/degree
         struct_lqac[i].glon=glon_arr[0]
         struct_lqac[i].glat=glat_arr[0]
   endfor

   struct=[struct_agn,struct_qso,struct_vv,struct_lqac]

   loc=spectnear(struct,glon,glat,radius,count)
   if count eq 0 then begin
      if (NOT keyword_set(quiet)) then begin
         print,''
         print,'*** No sightlines in specified region'
         print,''
      endif
      return,0
   endif else begin
      struct=struct[loc]
      bright=where((struct.vmag le vmag) AND (struct.vmag ne 0.),count)
      if count eq 0 then begin
         if (NOT keyword_set(quiet)) then begin
            print,''
            print,'*** No BRIGHT sightlines in specified region'
            print,''
         endif
         return,0
      endif
      return,struct[bright]
   endelse

end