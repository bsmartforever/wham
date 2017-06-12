;+
; NAME:
;  hemr
; PURPOSE: (one line)
;  Compute the hemispherical reflectance
; DESCRIPTION:
;    This function is coded from equation 12.10 on page 365 in Hapke's book,
;    "Theory of Reflectance and Emittance Spectroscopy".
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  ans = hemr(w,imu,holes,p,b0,theta)
; INPUTS:
;  w     - Single scattering albedo.
;  imu   - Cosine of the incidence angle.
;  holes - Compaction parameter value (1986 formalism).
;  p     - Parameters of the single particle phase function
;          (default "function" is a constant of value p, this is not very
;           useful for this routine and is not recommended).
;          There are two legal input forms for p:
;            1. an array of dimensionality Pparms
;            2. an array of dimensionality (n_elements(w),Pparms)
;  b0    - Backscatter value.
;  theta - Surface roughness value.  (radians)
; OPTIONAL INPUT PARAMETERS:
;  None.
; KEYWORD PARAMETERS:
;  nmu - number of points between 0 and 1 at which to evaluate mu = cos(e).
;             Default 6.
;  nphi - number of points between 0 and pi at which to evaluate phi
;             Default 6.
;  H93   - Flag passed to bidr2, if set, uses the 1993 version of Hapke's
;            approximation to the Chandresekar H function.  The 1993 version
;            is more accurate but considerably slower to compute.
;  Pfn   - Flag passed to bidr2. REQUIRED.
;          Specify function to use for P(g) instead of the default constant.
;            The function must be a procedure taking arguments g,a,F,/radians
;              g  phase angle in radians (with keyword /radians set)
;              a  an array of Pparms parameters
;              F  phase function evaluated at phase angles g
;            Use "fn_hg3.pro" as a model.
;  Pparms- Flag passed to bidr2. Specify number of parameters to be passed to
;             P(g).  Really only needed if the number of parameters isn't one.
;  pedantic - Flag pased to bidr, if set, returns NaN for w > 1.
; OUTPUTS:
;  Return value is the hemispherical reflectance.
; COMMON BLOCKS:
;  None.
; SIDE EFFECTS:
;  None.
; RESTRICTIONS:
;  Any input may be a vector.  If more than one is a vector then the
;     lengths must match.  The return will have the same dimensions as
;     the input.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Leslie Young, SwRI, 2010/12/28, cloned from bidr2 where similar
;-
function hemr,in_w,in_imu,in_holes,in_p,in_b0,in_theta, $
              NMU=NMU, NPHI=NPHI,H93=h93,Pfn=Pfn,Pparms=Pparms,pedantic=pedantic

    h93 = keyword_set(h93)
    if not keyword_set(Pparms) then Pparms = 1
    check=[n_elements(in_w),n_elements(in_imu), $
           n_elements(in_holes),n_elements(in_p)/Pparms, $
           n_elements(in_b0),n_elements(in_theta)]
    tlen = max(check)
    z=where(check ne 1 and check ne tlen,count)
    if count ne 0 then begin
       print,'HEMR: Error, lengths of inputs must match or be 1.'
       return,0.0
    endif
    
   ; Promote all inputs to same length
    if n_elements(in_w)     eq 1 then w     = replicate(in_w,    tlen) else w     = in_w
    if n_elements(in_imu)   eq 1 then imu   = replicate(in_imu,  tlen) else imu   = in_imu
    if n_elements(in_holes) eq 1 then holes = replicate(in_holes,tlen) else holes = in_holes
    if n_elements(in_b0)    eq 1 then b0    = replicate(in_b0,   tlen) else b0    = in_b0
    if n_elements(in_theta) eq 1 then theta = replicate(in_theta,tlen) else theta = in_theta

   ; For BIDR2 there are four P input options: p(scalar), p[Pparms], p[tlen],
   ;   p[tlen,Pparms].  But for HEMR only two: p[Pparms], p[tlen,Pparms]
   unhappy = 1

   ; p[tlen,Pparms]
   if n_elements(in_p) eq tlen*Pparms and Pparms gt 1 then begin
      sz = size(in_p)
      if sz[1] eq tlen and sz[2] eq Pparms then begin
         p = in_p
         unhappy = 0
      endif
   endif

   ; p[Pparms]
   if n_elements(in_p) eq Pparms and Pparms gt 1 then begin
      p = fltarr(tlen,Pparms)
      ; this works for both p[1,Pparms] and p[Pparms]
      for i=0,tlen-1 do p[i,*] = in_p
      unhappy = 0
   endif

   if unhappy then begin
      print,'HEMR: Error, dimensionality of input P is bad'
      return,0.0
   endif

   if not keyword_set(Pfn) then begin
      print,'HEMR: Error, Pfn must be set'
      return,0.0
   endif

   hemr = fltarr(tlen)

   if not keyword_set(nmu) then nmu = 6
   dmu = 1./nmu
   mu = (findgen(nmu)+0.5) * dmu
   if not keyword_set(nphi) then nphi = 6
   dphi = !pi/nphi
   phi = (findgen(nphi)+0.5) * dphi
   cosphi = cos(phi)
   sini = sqrt(1-imu^2)
   sine = sqrt(1-mu^2)
   mumat = mu # replicate(1,nphi) ; [nmu, nphi]

   ; The following matrix arithmetic performs the double integral
   ; over mu and phi in Hapke 10.10, or
   ; rh = Sum[ Sum[  2 * r * mu / mu0, {iphi = 0, nphi-1}], {imu = 0, nmu-1}]
   for i = 0, tlen-1 do begin
      ; cosg has dimensions [nmu, nphi].
      cosg = imu[i] * mumat + sini[i] * (sine # cosphi)
      g = acos(cosg)
      r =reform( $
         bidr2(w[i],mumat,imu[i],g,holes[i],p[i,*],b0[i],theta[i], $
         H93=h93,Pfn=Pfn,Pparms=Pparms,pedantic=pedantic), nmu, nphi)
      hemr[i] = 2 * total(mu # r) * dmu * dphi / imu[i]

   endfor

   return, hemr
   
end
