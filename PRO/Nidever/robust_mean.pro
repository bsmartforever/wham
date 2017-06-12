pro robust_mean,vector,robmean,robsigma,sig=sig0,error=error,$
                ind=ind,rejind=rejind,numrej=numrej,verbose=verbose,$
                stp=stp

;+
;
; ROBUST_MEAN
;
; This calculates a robust, outlier resistant MEAN with
; optional weighting.
;
; INPUTS:
;  vector    The vector of values for which to compute the mean of.
;  =sig      Optional uncertainties in the vector values.  These
;              will be used for weighting.
;  /verbose  Verbose output.
;  /stp      Stop at the end of the program.
;
; OUTPUTS:
;  robmean   The robust mean of the vector values.
;  robsigma   The robust standard deviation of vector.
;  =error    The error message if one occured
;  =ind      Indices of non-rejected values.
;  =rejind   Indices of rejected values.
;  =numrej   The number of rejected values.
;
; USAGE:
;  IDL>robust_mean,vector,robmean,robsigma,sig=sig,error=error
;
; By D.Nidever   July 2008
;- 

undefine,error,robmean,robsigma,ind,rejind,numrej

; Not enough inputs
nvector = n_elements(vector)
if nvector eq 0 then begin
  print,'Syntax - robust_mean,vector,robmean,robsigma,sig=sig,error=error,'
  print,'                     ind=ind,rejind=rejind,numrej=numrej'
  error = 'Not enough inputs'
  return
endif

; Defaults
maxiter = 20
eps = 1.0e-24

; Do we have uncertainties?
;  if not, then unweighted
nsig = n_elements(sig0)
if nsig gt 0 then sig=sig0 else sig=fltarr(nvector)+1.0
if total(sig) eq 0.0 then sig=fltarr(nvector)+1.0


; Starting MEAN and SIGMA
; Weights, using WMEANERR.PRO
if (nsig gt 0) then begin
  WMEANERR,vector,sig,robmean,robsigma
; No weights, use median/MAD
endif else begin
  robmean = MEDIAN(vector,/even)
  robsigma = MAD(vector)
endelse

; Iterate until it converged
old_nind = nvector
count = 0
done = 0
WHILE (done eq 0) do begin


  ; Remove outliers from the whole array
  ; Make sure we have a decent SIGMA
  if (robsigma gt 0.0) then begin
    rejind = where(abs(vector-robmean) gt 2.5*robsigma,numrej)
    ind = where(abs(vector-robmean) le 2.5*robsigma,nind)

    ; No good points left, loosen sigma limit
    if nind eq 0 then begin
      rejind = where(abs(vector-robmean) gt 3.0*robsigma,numrej)
      ind = where(abs(vector-robmean) le 3.0*robsigma,nind)
    endif
 
    ; No good points left, loosen sigma limit again
    if nind eq 0 then begin
      rejind = where(abs(vector-robmean) gt 4.0*robsigma,numrej)
      ind = where(abs(vector-robmean) le 4.0*robsigma,nind)
    endif

  endif else begin
    ind = lindgen(nvector)
    undefine,rejind
    numrej = 0
    nind = nvector
  endelse


  ; Use WMEANERR to get weighted mean
  WMEANERR,vector[ind],sig[ind],NEW_mean,NEW_sigma

  ; What's the change
  dmean = abs(robmean-NEW_mean)
  dsigma = abs(robsigma-NEW_sigma)
  dnind = abs(nind-OLD_nind)

  ; Are we done?
  if (dmean lt eps and dsigma lt eps and dnind eq 0 and count gt 0) or $
    (count gt maxiter) then done=1

  ; New best estimate
  OLD_mean = robmean
  OLD_sigma = robsigma
  OLD_nind = nind
  robmean = NEW_mean
  robsigma = NEW_sigma

  count++

  ;stop

ENDWHILE

; Verbose output
if keyword_set(verbose) then begin
  print,'Mean  = ',strtrim(robmean,2)
  print,'Sigma = ',strtrim(robsigma,2)
  print,'Nused = ',strtrim(nind,2)
  print,'Nrej  = ',strtrim(numrej,2)
  print,'Niter = ',strtrim(count,2)
endif

if keyword_set(stp) then stop

end
