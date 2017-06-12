;+ 
; NAME:
; x_wrechfspec
;    Version 1.0
;
; PURPOSE:
;    Reads and writes the HIRES echfspec structure
;
; CALLING SEQUENCE:
;   hires_wrfspec, echfspec, outfil, /READ, /UVES
;
; INPUTS:
;
; RETURNS:
;
; OUTPUTS:
;
; OPTIONAL KEYWORDS:
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:
;   08-Sep-2005 Written by JXP
;-
;------------------------------------------------------------------------------

pro hires_wrfspec, echfspec, outfil, READ=read, UVES=uves

;
  if  N_params() LT 2  then begin 
    print,'Syntax - ' + $
      'hires_wrfspec, echfspec, outfil, /READ, /UVES [v1.1]'
    return
  endif 

;  Optional Keywords

  if keyword_set(UVES) then begin
      uves_wrfspec, echfspec, outfil, READ=read
      return
  endif

  if n_elements(ecfspec) GT 1 then stop

  if not keyword_set( READ ) then begin
;;;;;;; WRITE ;;;;;;;;;
;  Create anonymous structure
      anon = { $
               field: ' ', $
               obj_id: ' ',        $ ; ID value (a=primary, b-z=serendip, x=NG)
               flg_anly: 0,      $ ;  0=No analy, 1=Extracted, 2=Fluxed 
               obj_type: 0, $
               xyimg: fltarr(2), $ ; xy pix of original image
               mag: 0., $       ; Usually R mag
               phot_fil: ' ', $
               img_fil: ' ', $  ; Img file
               nexp: 0L, $
               wvmnx: dblarr(100,2), $ ; Wave min/max for each exposure
               texp: dblarr(100), $ ; t_exp for each exposure
               obj_fil: strarr(100), $
               flg_fin: 0, $    ; 
               flg_flux: 0, $   ; 1=fnu, 2=flambda
               npix: lonarr(130), $ ; npix
               phys_ordr: lonarr(130), $ ; ORDERS
               wave: dblarr(6000,130), $
               fx: fltarr(6000,130), $
               var: dblarr(6000,130), $ ; <=0 :: rejected pix
               novar: dblarr(6000,130), $ ; <=0 :: rejected pix
               class:  '', $     ;;;;;; ALL ZANS BELOW HERE  ;;;;;;;;;
               subclass: '', $
               z: 0.0, $
               z_err: 0.0, $
               rchi2: 0.0, $
               dof:  0L, $
               rchi2diff: 0.0, $
               tfile: '', $
               tcolumn: lonarr(20) - 1L, $
               npoly:       0L, $
               theta:      fltarr(20), $
               vdisp:     0.0, $
               vdisp_err:  0.0  $
            }

      copy_struct, echfspec, anon, EXCEPT_TAGS=['zans']
      copy_struct, echfspec.zans, anon

      ; Catch zero strings
      a = where(strlen(anon.field) EQ 0, na)
      if na NE 0 then anon[a].field = ' '
      a = where(strlen(anon.obj_id) EQ 0, na)
      if na NE 0 then anon[a].obj_id = ' '
      a = where(strlen(anon.class) EQ 0, na)
      if na NE 0 then anon[a].class = ' '
      a = where(strlen(anon.subclass) EQ 0, na)
      if na NE 0 then anon[a].subclass = ' '
      a = where(strlen(anon.tfile) EQ 0, na)
      if na NE 0 then anon[a].tfile = ' '
      a = where(strlen(anon.obj_fil[0]) EQ 0, na)
      if na NE 0 then anon[a].obj_fil[0] = ' '
      a = where(strlen(anon.phot_fil) EQ 0, na)
      if na NE 0 then anon[a].phot_fil = ' '
      a = where(strlen(anon.img_fil) EQ 0, na)
      if na NE 0 then anon[a].img_fil = ' '

      ; Write
      mwrfits, anon, outfil, /create
      spawn, 'gzip -f '+outfil
      delvarx, anon
  endif else begin
;;;;; READ ;;;;;;;;
      tmp = {hiresfspecstrct} 
      anon = xmrdfits(outfil, 1, /silent)
      echfspec = replicate(tmp, n_elements(anon))

      ; COPY
      copy_struct, anon, echfspec, /recur_to
      delvarx, anon, tmp
  endelse
  return
end
