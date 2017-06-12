;+ 
; NAME:
; lls_stat
;
; PURPOSE:
;    Given a LLS struct and gz list, determine the indices of those
;    LLS satisfying the statistical sample.
;
; CALLING SEQUENCE:
;   sdss_llsstat
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
; PROCEDURES CALLED:
;
; REVISION HISTORY:
;   Feb-2009 Written by JXP
;-
function sdss_llsstat, llsstr, qsos, PARTIAL=partial, VPROX=vprox, PROX=prox, $
                   OIDX=odix, MOCK=mock, MAXDZ=maxdz, ZEM_MIN=zem_min

  if  N_params() LT 2  then begin 
    print,'Syntax - ' + $
      'indx = lls_stat(llsstr, qsos, /PARTIAL, VPROX=, /PROX), [v1.1]'
    return, -1
  endif 
  if not keyword_set( VPROX ) then vprox = 3000.
  if not keyword_set( MAXDZ ) then maxdz = 99.99
  if not keyword_set( ZEM_MIN ) then zem_min = 0.

  nlls = n_elements(llsstr)
  lls = lindgen(nlls)

  msk_smpl = bytarr(nlls)  ; 0=bad, 1=good

  zmax = x_relvel(qsos.zem,VPROX) 
  if not keyword_set(MOCK) then $
    x_radec, llsstr.qso_ra, llsstr.qso_dec, rad, decd

  for ii=0L,nlls-1 do begin
      qq = lls[ii]

      ;; Two LLS on one sightline?
      if not keyword_set(MOCK) then begin
          mtlls = where(abs(rad-rad[qq]) LT 0.001 AND $
                        abs(decd-decd[qq]) LT 0.001 AND $
                        abs(llsstr.qso_zem-llsstr[qq].qso_zem) LT 0.03 AND $
                        abs(llsstr.zabs-llsstr[qq].zabs) LT 0.04, nmLLS)
          if nmLLS NE 1 then stop  ;; LLS are probably too close in z
      endif 
      
      ;; Cut on NHI
      if keyword_set(PARTIAL) and llsstr[qq].NHI GT 17.5 then continue
      if not keyword_set(PARTIAL) and llsstr[qq].NHI LE 17.5 then continue

      ;; Match to QSO RA, DEC
      if not keyword_set(MOCK) then begin
          idx = where(abs(qsos.ra-rad[qq]) LT 0.001 AND $
                      abs(qsos.dec-decd[qq]) LT 0.001 AND $
                      abs(qsos.zem-llsstr[qq].qso_zem) LT 0.03, nidx)
      endif else begin
          idx = where(qsos.plate EQ llsstr[qq].sdss_plate AND $
                      abs(qsos.zem-llsstr[qq].qso_zem) LT 0.03, nidx)
      endelse
      if nidx NE 1 then stop

      ;; Query redshift
      if qsos[idx].zt2 GT 0. AND $
        llsstr[qq].zabs GT (qsos[idx].zt2 > (qsos[idx].zem - MAXDZ)) AND $
        qsos[idx].zem GT ZEM_MIN then begin 
          if not keyword_set(PROX) and $ ; Intervening
            llsstr[qq].zabs LT zmax[idx] then msk_smpl[qq] = 1B
          if keyword_set(PROX) and $ ; Proximate
            llsstr[qq].zabs GE zmax[idx] then msk_smpl[qq] = 1B
      endif
  endfor

  ;; Return
  gd = where(msk_smpl,complement=OIDX,ngd)
  if ngd EQ 0 then return, -1 else begin
;      if arg_present(IDXA) then idxa = idxa[gd]
      return, lls[gd]
  endelse

end
