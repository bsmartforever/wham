;+ 
; NAME: 
;       KDF_MODE 
; PURPOSE: 
;       Find the mode of a sample from a continuous 1-d distribution 
; EXPLANATION: 
;       Finds the mode by finding the maximum of the probability 
;       distribution, as found by a kernel density estimation with a 
;       Gaussian kernel. 
; CALLING EXAMPLE: 
;       mode = kdf_mode(x) 
; INPUTS: 
;       x = array of sample points 
; KEYWORDS: 
;       None. 
; OUTPUT: 
;       Returns the mode of the distribution 
; COMMON BLOCKS: 
;       kdf_x (to pass the data array around) 
; PROCEDURE: 
;       This function uses MINF_BRACKET and MINF_PARABOLIC from the 
;       NASA astronomy IDL library to maximize the KDF without using PDEs. 
; MODIFICATION HISTORY: 
;       Written, Gray Kanarek 2010. 
;- 
FUNCTION kdf, u 
  common kdf_x, xx 
  nu = n_elements(u) 
  case nu of 
    0: message, 'U must be a scalar or array of target values' 
    1: res = u*0 
    else: begin 
      s = size(u) 
      res = make_array(s[1:s[0]],value=0.,type=size(type,/dim)) 
          end 
  endcase 
  lim = [-6,6.] 
  for i=0,nu-1 do begin 
    arg = u[i]-xx 
    nonz = where(arg ge lim[0] and arg le lim[1],nz) 
    if (nz eq 0) then continue 
    res[i] = total(exp(-arg[nonz]^2/2.)/2.5066283)/n_elements(xx) 
  endfor 
  return, -res ;we're looking for the max, so return negative prob. 
end 
FUNCTION kdf_mode, x_array 
  common kdf_x 
  xx = x_array 
  xa = min(xx) & xb = max(xx) 
  minf_bracket, xa, xb, xc, func='kdf' 
  minf_parabolic, xa, xb, xc, xm, fm, func='kdf' 
  return, xm 
end 