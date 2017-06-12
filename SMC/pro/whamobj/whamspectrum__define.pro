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

  if *self.points eq !NULL then begin
    message, 'No data points in spectrum! Nothing to plot...', /info
    return, !null
  endif
  
  if ~isa(style) then style = ''
  
  if KEYWORD_SET(errorbars) then $
    p = ERRORPLOT((*self.points)[*,0], (*self.points)[*,1], sqrt((*self.points)[*,2]), $
      style, _extra = extra) $
  else $
    p = PLOT((*self.points)[*,0], (*self.points)[*,1], style, _extra = extra)
    
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
  
  v = (*self.points)[*,0]
  
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
  
  return, WHAMSpectrum((*self.points)[slice, *])
  
end

function WHAMSpectrum::_overloadBracketsRightSide, isRange, s1
  
  ;; spectra are 1D objects
  if n_elements(isRange) gt 1 then begin
    message, "Index to WHAMSpectrum must be one-dimensional.", /info
    return, []
  endif

  ndp = n_elements(self)  

  if isRange eq 0 then begin
    ;; non-range, return numeric array... should we return a spectrum object instead?
    result = list()
 
    ;; limit check or store result
    foreach i, s1 do begin
      ;; convert negative indicies
      if i lt 0 then i = ndp + i

      if (i lt 0) || (i ge ndp) then begin
        message, "Index to WHAMSpectrum out of range (0 - " + strtrim(ndp-1,2) + ").", /info
        return, []
      endif else begin
        result.Add, (*self.points)[i, *]
      endelse
    endforeach

    return, reform(result.toArray())  

  endif else begin
    ;; have a range, return WHAMSpectrum slice

    ;; convert negative indicies
    if s1[0] lt 0 then s1[0] = ndp + s1[0]
    if s1[1] lt 0 then s1[1] = ndp + s1[1]

    ;; could handle this and return a reverse, but keep same as IDL for now
    if s1[0] gt s1[1] then begin
      message, 'Illegal subscript range: start greater than end.', /info
      return, []
    endif
        
    ;; limit check
    if (s1[0] lt 0) || (s1[0] ge ndp) || (s1[1] lt 0) || (s1[1] ge ndp) then begin
      message, "Index to WHAMSpectrum out of range (0 - " + strtrim(ndp-1,2) + ").", /info
      return, []
    endif else begin
      return, WHAMSpectrum((*self.points)[s1[0]:s1[1]:s1[2], *])
    endelse
    
  endelse

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
    
  ndp = n_elements(self)
  
  ;; null spectrum
  if ndp eq 0 then return, !VALUES.f_nan
  
  ;; non-spectral data
  IF ndp EQ 1 THEN RETURN, ((*self.points)[*,1])[0]
  
  ;; full integration
  IF N_PARAMS() EQ 0 THEN vmin = MIN((*self.points)[*,0], MAX=vmax)
  
  slice = self.Slice(vmin, vmax)
  
  if ndp eq 0 then return, 0
  
  int = int_tabulated(slice.vel, double(slice.data), /sort)

  ;; TODO: think we want to return higher moments relative to lower ones... 
  ;;       e.g., 2nd moment is * (v - |v|)^2 not just v^2.
  if moment gt 0 then $
    int = int_tabulated(slice.vel, double(slice.data * (slice.vel) ^ moment), /sort) / int
    
  return, int
  
end

;+
; :Description:
;    Populate object with an empty spectrum; note, we are creating a valid (non-null)
;    pointer here that points to a !NULL value.
;
; :Author: Matt Haffner
;-
pro WHAMSpectrum::Null
  self.points = PTR_NEW(!NULL)
end

function WHAMSpectrum::_overloadSize
  self->GetProperty, ndp=ndp
  return, ndp
end

;+
; :Description:
;    Output user-friendly info for HELP on a WHAMSpectrum object.
;
; :Params:
;    myName : in, required, type=string
;      variable name for the object
;
; :Author: Matt Haffner
;-
function WHAMSpectrum::_overloadHelp, myName
  
  ndp = N_ELEMENTS(self)
  return, string(FORMAT='(%"%-15s WHAMSpectrum[%d]")', myName, ndp)
    
end

;+
; :Description:
;    User-friendly PRINT output for WHAMSpectrum objects.
;    Testing only for now.
;
; :Author: Matt Haffner
;-
function WHAMSpectrum::_overloadPrint

  out = list(string(["Velocity", "Data", "Variance"], format = '(%"%10s %10s %10s\r")'))
  for i = 0, N_ELEMENTS(self)-1 do $
    out.Add, string((*self.points)[i, *], format = '(%"%10.3f %10.3f %10.3f\r")')
  
  return, out.toArray()
  
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
  vel = vel, data = data, var = var, points = points
  
  if *self.points eq !NULL then begin
    ndp = 0
    vel = []
    data = []
    var = []
  endif else begin
    points_dim = size(*self.points, /dim)
    
    ndp = points_dim[0]
    
    ;; these make data copies, so only do if needed
    IF ARG_PRESENT(vel) then vel = (*self.points)[*, 0]
    IF ARG_PRESENT(data) then data = (*self.points)[*, 1]
    IF ARG_PRESENT(var) then var = (*self.points)[*, 2]
  endelse
  
  if ARG_PRESENT(points) then points = *self.points
end

pro WHAMSpectrum::SetProperty, $
  vel = vel, data = data, var = var, points = points
  
  if isa(points) then begin

    ;; check if array dimensions make sense for a spectrum
    p_dim = size(points, /dim)
    if (n_elements(p_dim) ne 2) || (p_dim[1] ne 3) then begin
      message, 'POINTS must have dimentions [N, 3]; no change made to spectrum.', /info
      return
    endif
    
    ;; looks OK, fill points with a copy of input
    self.points = PTR_NEW(p1)

    ;; done, even if other arguments supplied
    if isa(vel) || isa(data) || isa(var) then $
        message, 'POINTS argument supplied; all others ignored.', /info
    return
  endif 

  ;; all three supplied--size can change
  if isa(vel) && isa(data) && isa(var) then begin

    ;; are they all the same length?
    ndp = N_ELEMENTS(vel)
    if (N_ELEMENTS(data) ne ndp) || (N_ELEMENTS(var) ne ndp) then begin
      message, 'Arguments must all have same length; no change made to spectrum.', /info
      return
    endif

    ;; looks OK, fill points
    p = (list(vel, data, var)).toArray(/transpose)
    self.points = PTR_NEW(p)

    ;; done
    return    
  endif

  ;; now deal with less than 3 vectors supplied

  ;; if any argument is the wrong size, don't do anything--least-harm approach
  ndp = N_ELEMENTS(self)
  if (isa(vel) && n_elements(vel) ne ndp) || $
     (isa(data) && n_elements(data) ne ndp) || $
     (isa(var) && n_elements(var) ne ndp) then begin
       
     message, 'Arguments must all be ' + strtrim(ndp, 2) + ' elements long; no change made to spectrum', /info
     return
  endif

  if isa(vel) then (*self.points)[*,0] = (list(vel, /extract)).toArray()
  if isa(data) then (*self.points)[*,1] = (list(data, /extract)).toArray()
  if isa(var) then (*self.points)[*,2] = (list(var, /extract)).toArray()
  
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
  
  ;; user passed a single list or array in for initialization
  if N_PARAMS() eq 1 then begin
    if isa(p1, 'LIST') then begin
    
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
      
      ;; looks OK, fill points with a copy of input
      self.points = PTR_NEW(p1.toArray(/transpose))
      
      ;; end single list processing
      return, 1
    endif else if isa(p1, /array, /number) then begin
    
      ;; check if array dimensions make sense for a spectrum
      p1_dim = size(p1, /dim)
      if (n_elements(p1_dim) ne 2) || (p1_dim[1] ne 3) then begin
        message, 'Single array argument must have dimentions [N, 3]; returning empty spectrum.', /info
        self.Null
        return, 1
      endif
      
      ;; looks OK, fill points with a copy of input
      self.points = PTR_NEW(p1)
      
      ;; end single array processing
      return, 1
    endif
  endif
  
  ;; user passed in three vectors to initialize
  if N_PARAMS() eq 3 then begin
    if isa(p1, /array) and isa(p2, /array) and isa(p3, /array) then begin
    
      ;; are they all the same length?
      ndp = N_ELEMENTS(p1)
      if N_ELEMENTS(p2) ne ndp or N_ELEMENTS(p3) ne ndp then begin
        message, 'Arguments must all have same length; returning empty spectrum.', /info
        self.Null
        return, 1
      endif
      
      ;; looks OK, fill points with inputs...
      ;;; using LIST here handles both lists and arrays nicely
      p = (list(p1, p2, p3)).toArray(/transpose)
      self.points = PTR_NEW(p)
      
      return, 1
    endif
  endif
  
  message, 'Can''t parse arguments: need three arrays or a list of three arrays; returning empty spectrum.', /info
  self.Null
  return, 1
  
end

;+
; :Description:
;    Class definition for WHAMSpectrum, a simple 3-vector (velocity, data, variance) object 
;
; :Fields:
;   points
;     pointer to a [N, 3] array of a spectrum where [*, 0] is velocity, [*, 1] is data, and [*, 2] is variance
;
; :Author: Matt Haffner
;-
pro WHAMSpectrum__define

  on_error, 2
  if double(!version.release) lt 8.0 then message, 'Sorry, requires IDL version 8.0 or above.'
  
  struct = {WHAMSpectrum, $
    points: PTR_NEW(), $
    inherits IDL_Object $
  }
  
end
