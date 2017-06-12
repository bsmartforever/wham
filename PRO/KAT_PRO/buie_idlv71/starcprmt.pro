;+
; NAME:
;  starcprmt
; PURPOSE:
;  Promote version of a star catalog file to highest version.
; DESCRIPTION:
;  Reads the star catalog file, which may be ascii (version 0, output of
;    refnet) or binary (v1.0) The discriminant is to read the first 10 byte
;    string and look for version. If it is ascii it is rewritten in place
;    as a v1.0 binary. The binary file is big endian.
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  starcprmt,starcfn
; INPUTS:
;  starcfn - File with star catalog data.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SILENT - Flag, if set suppresses all non-error output.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; Rewrites cat file in place to binary version if ascii (as created by refnet).
; RESTRICTIONS:
; Requires a version of READCOL that supports keywords NGOOD and NLINES.
; PROCEDURE:
;  The catalogue file is updated to the most recent version.  
;    Not changed if already current, or not present.
;
;   The following formats are supported:
;   v0.0  The base ascii format with no version line 
;         This is the  version generated by refnet.
;   v1.0  Version line at the start:   STARC v1.0
;         Binary format in big endian order. 
;         The version line is a 10 byte string.
;         Next is a longword giving number of stars, >= 0.
;         This is followed by two double vectors for ra and dec, then float
;            vectors for bmag and rmag.
;
; MODIFICATION HISTORY:
;  2007/11/21, Written by Peter L. Collins, Lowell Observatory
;  2007/11/30, PLC, modified for validity checking and new READCOL keywds.
;  2008/10/21, MWB, added SILENT keyword
;
;-
pro starcprmt,starfile,SILENT=silent

   self='STARCPRMT: '
   if badpar(starfile,7,0,caller=self +  '(starfile) ') then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ',default=0) then return

   ; If not present, don't do anything.
   if not exists(starfile) THEN return

   info=file_info(starfile)
   if info.size eq 0 then return ; if it is an empty file it remains empty.

   ; Check the file version by reading the first line of the file.
   ; readu is used and this will work correctly on the binary and
   ; the ascii versions. It is assumed that the version string is 10 
   ; bytes long.
   v1pt0='STARC v1.0'
   latest=v1pt0

   version = v1pt0
   openr, slun,starfile,/GET_LUN
   readu, slun, version
   swap_endian_inplace, version, /SWAP_IF_LITTLE_ENDIAN
   free_lun,slun

   ; If it's current, do nothing.
   if version eq latest then return

   ; Since it has no recognized version string assume it to be an ascii file.

   if not silent then $
      print,self, ' Upgrading file, ', starfile, ' from ascii ',' to ',latest
   ; find out how many lines are in the ascii file.
   ; read in all the contents of the ascii file.
   readcol,starfile,hr,m1,s1,dgas,m2,s2,rmag,bmag, $
           format='d,d,d,a,d,d,f,f',/silent,/NAN,NLINES=nlines,COUNT=ngood
   if ngood ne nlines then begin
      print,self, 'File ', starfile, ' contains invalid catalog file lines.'
      ans=''
      read,prompt='Ok to proceed?',ans
      if strlowcase(strmid(ans,0,1)) ne 'y' then return
   endif
   z = where( finite(hr) eq 0 or  finite(m1) eq 0  or  finite(s1) eq 0  or $
               finite(m2) eq 0  or  finite(s2) eq 0  or finite(rmag) eq 0 or $
              finite(bmag) eq 0 , nnan)
   if nnan gt 0 then begin
      print,self, 'File ', starfile, ' contains invalid catalog file fields.'
      return
   endif

   nstars=long(n_elements(hr))
   signas = strmid(dgas,0,1)
   dg = fix(strmid(dgas,1,2))
   hmstorad,hr,m1,s1,ra
   sign = replicate(1.0,nstars)
   z=where(signas eq '-',count)
   if count ne 0 then sign[z] = -1.0
   dmstorad,sign,abs(dg),m2,s2,dec
   z = where ( ra lt 0.0 or ra gt !PI*2.0 or abs(dec) gt !PI/2.0, nrange)
   if nrange gt 0 then begin
      print,self, 'File ', starfile, ' contains invalid catalog positions.'
      return
   endif

   ; write out the new version string for this file.
   latest_out=swap_endian(latest,/SWAP_IF_LITTLE_ENDIAN)
   openw, slun,starfile,/GET_LUN
   writeu, slun,latest_out
   ; write out the star count.
   nstars_out=swap_endian(nstars,/SWAP_IF_LITTLE_ENDIAN)
   writeu, slun, nstars_out
   if nstars gt 0 then begin
      ; ra vector, double of length nstars.
      swap_endian_inplace, ra, /SWAP_IF_LITTLE_ENDIAN
      writeu, slun,ra
      ; dec vector, double of length nstars.
      swap_endian_inplace, dec, /SWAP_IF_LITTLE_ENDIAN
      writeu, slun,dec
      ; bmag vector, double of length nstars.
      swap_endian_inplace, bmag, /SWAP_IF_LITTLE_ENDIAN
      writeu, slun,bmag
      ; rmag vector, double of length nstars.
      swap_endian_inplace, rmag, /SWAP_IF_LITTLE_ENDIAN
      writeu, slun,rmag
   endif
   free_lun, slun
end
