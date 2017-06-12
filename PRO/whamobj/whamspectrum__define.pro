; docformat = 'rst rst'
  
;+
; Class for WHAM-type spectra. Encapsulates velocity, data, 
; and variance vectors and defines common methods for
; manipulating and plotting spectra.
; 
; :Properties:
;   ndp
;     Number of data points in the spectrum (integer)
;   vel
;     Velocity vector (double array, length ndp)
;   data
;     Data vector (double array, length ndp)
;   var
;     Variance vector (double array, length ndp)
; 
; :Author: Matt Haffner
; 
; :History:
;   9/9/10 : Creation
;   
;   9/10/10 : Documentation update
;-

compile_opt idl2, logical_predicate

;+
; :Description:
;    Wrapper for PLOT() and ERRORPLOT(). Feeds the appropriate
;    function with velocity and data vectors.
;
; :Keywords:
;    errorbars : in, optional, type=boolean
;      if set, plot with errorbars using ERRORPLOT()
;    _extra : in, optional, type=keywords
;      keywords to pass to PLOT() or ERRORPLOT()
;      
; :Returns: PLOT object
;
; :Author: Matt Haffner
;-
function WHAMSpectrum::Plot, style, errorbars = errorbars, _extra = extra
  
  if ~isa(style) then style = ''
  
  if KEYWORD_SET(errorbars) then $
    p = ERRORPLOT(self.vel.ToArray(), self.data.ToArray(), sqrt(self.var.ToArray()), style, _extra = extra) $
  else $
    p = PLOT(self.vel.ToArray(), self.data.ToArray(), style, _extra = extra)
  
    return, p

end

;+
; :Description:
;    Create a new WHAMSpectrum with a restricted velocity range.
;    
;    Note: If vmin_in is greater than vmax_in, the method 
;    automatically swaps the inputs.
;
; :Params:
;    vmin_in : in, required, type=float
;       minimum velocity to include in output spectrum
;    vmax_in : in, required, type=float
;       maximum velocity to include in output spectrum
;
; :Returns: WHAMSpectrum object sliced to velocity limits or null spectrum on error.
;
; :Author: Matt Haffner
;-
function WHAMSpectrum::Slice, vmin_in, vmax_in

  ;; incorrect number of params passed
  if N_PARAMS() ne 2 then begin
    message, "Requires exactly two arguments: min and max velocities.", /info
    return, WHAMSpectrum()  ; null spectrum
  endif
  
  ;; make copies since we could modify inputs
  vmin = vmin_in
  vmax = vmax_in

  v = self.vel.ToArray()

  ;; range checking
  IF vmin EQ vmax THEN BEGIN
    MESSAGE, "VMIN equals VMAX; can't slice.", /info
    RETURN, WHAMSpectrum()  ; null spectrum
  ENDIF
  
  IF vmin GT vmax THEN BEGIN
    MESSAGE, "VMIN is greater than VMAX; swapping inputs.", /info
    t = vmin
    vmin = vmax
    vmax = t
  ENDIF
  
  slice = WHERE(vmin LE v AND v LE vmax, slice_count)
  
  ;; oops, no data in that range...
  IF slice_count EQ 0 THEN BEGIN
    MESSAGE, "No data between " + strtrim(vmin, 2) + " and " + strtrim(vmax, 2) + "; can't slice.", /info
    return, WHAMSpectrum()  ; null spectrum
  endif

  return, WHAMSpectrum((self.vel)[slice], (self.data)[slice], (self.var)[slice])

end

;+
; :Description:
;    Numerically integrate over the spectrum. If no arguments are supplied
;    the full velocity range will be used. 
;    
;    Note: The spectrum can not contain duplicate velocity data points; 
;    INT_TABULATED will return an error.
;
; :Params:
;    vmin : in, optional, type=float
;      minimum velocity for the integration range
;    vmax : in, optional, type=float
;      maximum velocity for the integration range
;
; :Returns: float
;
; :Author: Matt Haffner
;-
FUNCTION WHAMSpectrum::Integrate, vmin, vmax, moment = moment

  if ~isa(moment) then moment = 0

  ;; non-spectral data
  IF self.ndp EQ 1 THEN RETURN, (self.data)[0]
  
  ;; full integration
  IF N_PARAMS() EQ 0 THEN vmin = MIN(self.vel.ToArray(), MAX=vmax)
  
  slice = self.Slice(vmin, vmax)
  
  if slice.ndp eq 0 then return, 0

  int = int_tabulated(slice.vel, double(slice.data), /sort)
  if moment gt 0 then $
    int = int_tabulated(slice.vel, double(slice.data * (slice.vel) ^ moment), /sort) / int
  
  return, int

end

;+
; :Description:
;    Populate object with an empty spectrum. This is safer than leaving
;    the vectors with !NULL object references.
;
; :Author: Matt Haffner
;-
pro WHAMSpectrum::Null
  self.ndp = 0
  self.vel = list()
  self.data = list()
  self.var = list()
end

function WHAMSpectrum::_overloadSize
  return, self.ndp
end

;+
; :Description:
;    Return an object property. vel, data, and var are
;    returned as arrays as the most likely use by the caller.
;
; :Keywords:
;    ndp
;    vel
;    data
;    var
;
; :Author: Matt Haffner
;-
pro WHAMSpectrum::GetProperty, ndp = ndp, $
  vel = vel, data = data, var = var
  
  ndp = self.ndp
  if arg_present(vel) then vel = self.vel.ToArray()
  if arg_present(data) then data = self.data.ToArray()
  if arg_present(var) then var = self.var.ToArray()
  
end

pro WHAMSpectrum::SetProperty, $
  vel = vel, data = data, var = var
  
  if isa(vel) then self.vel = list(vel, /extract)
  if isa(data) then self.data = list(data, /extract)
  if isa(var) then self.var = list(var, /extract)
  
end


;+
; :Description:
;    Initialize a WHAMSpectrum using input vectors or
;    calls ::Null.
;
; :Params:
;    p1 : in, optional, type=list or array
;      velocity vector as an array or list 
;      or a list containting exactly
;      three lists or arrays in order: (vel, data, var). 
;    p2 : in, optional, type=list or array
;      data vector as an array or list
;    p3 : in, optional, type=list or array
;      variance vector as an array or list
;
; :Returns: boolean for success
;
; :Author: Matt Haffner
;-
function WHAMSpectrum::Init, p1, p2, p3

  if ~isa(p1) then begin
    ;; init an empty spectrum object
    self.Null
    return, 1
  endif
  
  ;; user passed a single list in for initialization; should have length of 3
  if N_PARAMS() eq 1 and isa(p1, 'LIST') then begin

    ;; do we have 3 vectors? 
    if N_ELEMENTS(p1) ne 3 then begin
      message, 'LIST argument must have three elements: velocity, data, variance; returning empty spectrum.', /info
      self.Null
      return, 1
    endif
    
    ;; are they all the same length?
    ndp = N_ELEMENTS(p1[0])
    if N_ELEMENTS(p1[1]) ne ndp or N_ELEMENTS(p1[2]) ne ndp then begin
      message, 'LIST elements must all have same length; returning empty spectrum.', /info
      self.Null
      return, 1
    endif
    
    ;; looks OK, fill structure with copies of input
    self.ndp = ndp
    self.vel = list(p1[0], /extract)
    self.data = list(p1[1], /extract)
    self.var = list(p1[2], /extract)
    
    return, 1
  endif ; single LIST argument

  if N_PARAMS() eq 3 then begin
    if isa(p1, /array) and isa(p2, /array) and isa(p3, /array) then begin
      
      ;; are they all the same length?
      ndp = N_ELEMENTS(p1)
      if N_ELEMENTS(p2) ne ndp or N_ELEMENTS(p3) ne ndp then begin
        message, 'Arguments must all have same length; returning empty spectrum.', /info
        self.Null
        return, 1
      endif

      ;; looks OK, fill structure with copies of input
      self.ndp = ndp
      self.vel = list(p1, /extract)
      self.data = list(p2, /extract)
      self.var = list(p3, /extract)
      
      return, 1
    endif
  endif
 
  message, 'Can''t parse arguments: need three arrays or a list of three arrays; returning empty spectrum.', /info
  self.Null
  return, 1
 
end

;+
; :Description:
;    Class definition for WHAMSpectrum
;
; :Fields:
;   ndp
;     Number of data points in this spectrum
;   vel
;     LIST (length=ndp) containing velocity vector
;   data
;     LIST (length=ndp) containing data vector
;   var
;     LIST (length=ndp) containing variance vector
;
; :Author: Matt Haffner
;-
pro WHAMSpectrum__define

  on_error, 2
  if double(!version.release) lt 8.0 then message, 'Sorry, requires IDL version 8.0 or above.'

  struct = {WHAMSpectrum, $
            ndp: fix(0), $
            vel: list(), $
            data: list(), $
            var: list(), $
            inherits IDL_Object $
            }

end
