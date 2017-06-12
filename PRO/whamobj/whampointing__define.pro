; docformat = 'rst rst'

;+
; Class for WHAM pointings (i.e., spectral observations).
; 
; :Properties:
;   name
;     Pointing name, normally original file path (string)
;   shortname
;     A short version of the name, e.g. b123_45 (string)
;   ndp
;     Number of data points in this spectrum (integer)
;   vel
;     Velocity vector (double array, length ndp)
;   data
;     Data vector (double array, length ndp)
;   var
;     Variance vector (double array, length ndp)
;   glon
;     Galactic longitude of pointing (float, degrees)
;   glat
;     Galactic latitdue of pointing (float, degrees)
;   vlsr
;     Difference between geocentric and LSR velocity frames (float, km/s)
;   vgeo
;     Difference between raw and geocentric velocity frames (float, km/s)
;   day
;     WHAM day number of observation: days since 1997 Jan 1 (integer, days)
;   time
;     Decimal hours since midnight UTC (float, hours)
;   int
;     Total numerical integration of this spectrum (float, no unit conversion)
;   fit
;     Fit loaded from a processed extension (WHAMFit)
;   params
;     Hash containing all FITS keywords from the primary header (HASH)
;   ext_params
;     Hash containing all FITS keywords from the extension header (HASH)
;   spectrum
;     The spectrum for this pointing (WHAMSpectrum)
; 
; :Author: Matt Haffner
;-

compile_opt idl2, logical_predicate

;+
; :Description:
;   Open a plot window and display the spectrum associated
;   with this pointing. Fully labels the plot using
;   info loaded into the pointing.
;
; :Params:
;    style : in, optional, type=string
;      A style string to pass to spectrum.plot().
;
; :Keywords:
;    adu : in, optional, type=boolean
;      Set to change y-axis labeling to ADU (old) instead of ADU/sec.
;    over : in, optional, type=boolean
;      Set to overplot this pointing on the current plot.
;    _extra : in, optional, type=keywords
;      Keywords to pass to spectrum.plot() (and ultimately PLOT()/ERRORPLOT()).
;
; :Author: Matt Haffner
;-
function WHAMPointing::Plot, style, adu = adu, over = over, _extra = extra
    
  if N_ELEMENTS(style) eq 0 then style = 'D-b'

  title = '$\it l\rm$ = ' + string(self.glon, format = '(F6.2)') + $
          ', $\it b\rm$ = ' + string(self.glat, format = '(F6.2)') 

  if KEYWORD_SET(adu) then begin
    ytitle = 'Intensity [ADU]'
  endif else begin
    ytitle = 'Intensity [ADU s$^{-1}$]'
  endelse

  if KEYWORD_SET(over) then begin
    p = self.spectrum.plot(style, /over, _extra = extra)
  endif else begin
    p = self.spectrum.plot(style, name = self.shortname, $
      title = title, WINDOW_TITLE=self.shortname, $
      xtitle = 'Velocity [km s$^{-1}$]', ytitle = ytitle, $
      _extra = extra)
  endelse
  
  return,p
end

;+
; :Description:
;   Plot the data and fit that produced the current spectrum.
;
; :Keywords:
;    style : in, optional, type=string
;      A PLOT() style string to pass through for the data.
;    fitstyle : in, optional, type=string
;      A PLOT() style string to pass through for the fit.
;    rawvel : in, optional, type=boolean
;      Set to use the original spectrum's velocity frame (typically the "raw" one).
;    adu : in, optional, type=boolean
;      Set to change the y-axis label to ADU instead of ADU/sec. 
;    _extra : in, optional, type=keywords
;      Keywords to pass to PLOT().
;
; :Author: Matt Haffner
;-
function WHAMPointing::PlotOrigFit, style = style, fitstyle = fitstyle, $
  rawvel = rawvel, adu = adu, _extra = extra

  if self.fit.nbkg eq 0 and self.fit.ngauss eq 0 then begin
    message, '[' + self.name + '] No fit loaded to plot.', /info
    return, !null
  endif

  if N_ELEMENTS(style) eq 0 then style = 'D-b'
  if N_ELEMENTS(fitstyle) eq 0 then fitstyle = '-r'

  ;; Assume fit parameters are in raw velocity frame.
  ;;  This could be generalized by tracking back through the original extentions, 
  ;;  but for now, this is quick and handles many cases...

;  if N_ELEMENTS(velrange) ne 2 then $
;    velrange = aPlot.xrange
    
;  vel = indgen(velrange[1] - velrange[0]) + velrange[0]
  raw_vel = self.fit.orig.vel
  
  params = list(self.fit.bkg, /extract)    ; seed params with copy of bkg
  for i = 0, self.fit.ngauss-1 do begin 
    params.Add, (self.fit.mean)[i]
    params.Add, (self.fit.width)[i]
    params.Add, (self.fit.area)[i]
  endfor
  for i = 0, self.fit.natmos-1 do begin 
    params.Add, (self.fit.atmos_mean)[i] + self.vgeo
    params.Add, (self.fit.atmos_width)[i]
    params.Add, (self.fit.atmos_area)[i] * self.fit.atmos_level
  endfor
  
  fit = whamgauss(raw_vel, params.ToArray(), self.fit.nbkg)
  
  if KEYWORD_SET(rawvel) then plot_vel = raw_vel else plot_vel = self.vel.ToArray()
  
  title = '$\it l\rm$ = ' + string(self.glon, format = '(F6.2)') + $
          ', $\it b\rm$ = ' + string(self.glat, format = '(F6.2)') 

  if KEYWORD_SET(adu) then begin
    ytitle = 'Intensity [ADU]'
  endif else begin
    ytitle = 'Intensity [ADU s$^{-1}$]'
  endelse

  p = plot(plot_vel, self.fit.orig.data, style, $
        name = self.shortname, title = title, WINDOW_TITLE=self.shortname, $
        xtitle = 'Velocity [km s$^{-1}$]', ytitle = ytitle, $
        _extra = extra)
  
  f = plot(plot_vel, fit, fitstyle, /over)

  return, list(p, f)

end

;+
; :Description:
;    Read in a WHAM spectrum from a FITS file. Loads the spectrum
;    as well as the FITS keyword headers. A few parameters are 
;    extracted into explict WHAMPointer fields for quicker
;    access. If the extension contains fit keywords, the fit and 
;    the original spectrum are loaded, if possible.
;
; :Params:
;    file : in, required, type=string
;      The FITS file containting the pointing.
;
; :Keywords:
;    ftsext : in, optional, type=string, default='PROCSPEC'
;      The extension to read the pointing from.
;      
; :Author: Matt Haffner
;-
pro WHAMPointing::Read, file, ftsext = ftsext, guessvc = guessvc

  if N_ELEMENTS(ftsext) eq 0 then ftsext = 'PROCSPEC'

  self.name = file
  self.shortname = FILE_BASENAME(file, '.fts', /FOLD_CASE)

  ;; See if this is a block pointing
  in_block = stregex(self.shortname, 'b([0-9]+)_([0-9]+)', /extract, /subexpr)
  if in_block[0] ne '' then begin
    self.block = in_block[1]
    self.pointing = in_block[2]
  endif
   
  spectrum = mrdfits(file, ftsext, ext_header, status = status, /silent)
   
  if status eq 0 then begin 
   self.spectrum = WHAMSpectrum(spectrum.velocity, spectrum.data, spectrum.variance)
    
    main_header = HEADFITS(file)
    ext_header = HEADFITS(file, exten = ftsext)
  
    self.params = header2hash(main_header)
    self.ext_params = header2hash(ext_header)
    
    ;; deal with very old headers w/out demanded coords
    if total(self.params.HasKey(['DGAL-LON', 'DGAL-LAT'])) eq 2 then begin
      self.glon = (self.params)['DGAL-LON']
      self.glat = (self.params)['DGAL-LAT']
    endif else begin 
      self.glon = (self.params)['GAL-LON']
      self.glat = (self.params)['GAL-LAT']
    endelse
   
    ;; fix negative longitudes
    IF self.glon LT 0 THEN self.glon = 360 + self.glon
    
    ;; extract a few other parameters
    self.vlsr = (self.params)['VLSR']

    ;; try to extract a fit and add the related spectrum
    if (ftsext ne 'RAWSPEC') and (ftsext ne 'PROCSPEC') then begin
      self.fit = WHAMFit(self)
      if self.fit then begin
      
        ;; If file is too old, it might not have ORIGEXT. Fall back on 
        ;; RAWSPEC, which should be safe, but **might not be correct**
        if self.ext_params.HasKey('ORIGEXT') then begin
          orig_ext = (self.ext_params)['ORIGEXT']
        endif else begin
          orig_ext = 'RAWSPEC'
        endelse 
          
        self.fit.AddSpectrum, self.name, orig_ext         
      endif

    endif
      
    jdcnv, 1997, 1, 1, 0, day0        ;; WHAM epoch zero
    self.day = parsedate((self.params)['DATE-OBS'], /fits) - day0
    
    timestr = (self.params)['TIME-OBS']
    timeparts = str_sep(timestr, ":")
    self.time = timeparts[0]+timeparts[1]/60.0+timeparts[2]/3600.0
    
    if keyword_set(guessvc) then begin 

      ;; Estimate velocity calibration using tunes only
      self.vgeo = Predict_Vel_Calib3([(self.params)['PAMON']/10, (self.params)['PBMON']/10], $
          strtrim((self.params)['FSHNAME'], 2), $
          OBS = (((self.params)['OBS-LAT'] lt 0) ? 'ctio' : 'kpno'), /sil)

      self.spectrum.vel -= (self.vgeo + self.vlsr)

    endif else if (ftsext ne 'RAWSPEC') and (ftsext ne 'PROCSPEC') then begin
      ;; try to extract velocity shift of reduced data... 
      ;;   whamspect should stuff this in a keyword, but it doesn't
      
      ;; load a PROCSPEC, if there is one; fall back to RAWSPEC, if not
      f_unit = fxposit(file, 'PROCSPEC', errmsg=errmsg)
      if f_unit lt 0 then $
        f_unit = fxposit(file, 'RAWSPEC', errmsg=errmsg)

      if f_unit ge 0 then begin  
        unreduced = mrdfits(f_unit, 0, ext_header, status = status, /silent)
        free_lun, f_unit

        if status eq 0 then begin 
          ;; compute a vgeo in the same sense as vlsr:
          ;;  vlsr is location of v(lsr) = 0 km/s in the geo frame
          ;;  vgeo is location of v(geo) = 0 km/s in the raw frame
          vshift = (unreduced.velocity)[-1] - (self.spectrum.vel)[-1] 
          self.vgeo = vshift - self.vlsr
        endif
      endif
    endif
  endif else begin
    message, '[' + self.name + '] Error reading file or extension; pointing unchanged.', /info
  end

end

;+
; :Description:
;    A pointer wrapper for spectrum.Integrate()
;
; :Params:
;    vmin : in, optional, type=float, default=MIN(self.vel)
;      Minimum velocity for the integration.
;    vmax : in, optional, type=float, default=MAX(self.vel)
;      Maximum velocity for the integration.
;
; :Author: Matt Haffner
;-
function WHAMPointing::Integrate, vmin, vmax, _extra = _extra

  if N_PARAMS() eq 0 then begin
    return, self.spectrum.Integrate(_extra = _extra)
  endif else begin
    if ~isa(vmin) then vmin = min(self.spectrum.vel)
    if ~isa(vmax) then vmax = max(self.spectrum.vel)

    return, self.spectrum.Integrate(vmin, vmax, _extra = _extra)
  endelse
end

;+
; :Description:
;    Create a test pointing with a synthetic Gaussian.
;    NOTE: little extra added to the structure for now...
;
; :Author: Matt Haffner
;-
pro WHAMPointing::CreateTest

  self.name = 'Test Pointing'
  self.shortname = 'Test'
  
  vel = indgen(101) * 2.0 - 100
  data = gauss1(vel, [0, 30 / ( 2 * SQRT(2* ALOG(2)) ), 10])
  var = data
  self.spectrum = WHAMSpectrum(vel, data, var)
  
  self.glon = 0.0
  self.glat = 0.0
  
end

;; Can't implement this since a single pointing looks like an array to ISA then :(
;
;function WHAMPointing::_overloadSize
;  return, N_ELEMENTS(self.spectrum)
;end

;+
; :Description:
;    Output user-friendly info for HELP on a WHAMPointing object
;
; :Params:
;    myName : in, required, type=string
;      variable name for the object
;
; :Author: Matt Haffner
;-
function WHAMPointing::_overloadHelp, myName

  return, string(FORMAT='(%"%-15s WHAMPointing[%d]: \"%s\"")', $
      myName, self.spectrum.ndp, self.name)

end

;+
; :Description:
;    User-friendly PRINT output for WHAMPointing objects.
;    Testing only for now.
;
; :Author: Matt Haffner
;-
function WHAMPointing::_overloadPrint

;  foreach tag_names(self), field do begin
;    help, self, output = out
  out = 'Success: '

  return, out

end

;+
; :Description:
;    Return property values
;
; :Keywords:
;    name
;    shortname
;    ndp
;    vel
;    data
;    var
;    glon
;    glat
;    vlsr
;    vgeo
;    day
;    time
;    int
;    fit
;    params
;    ext_params
;
; :Author: haffner
;-
pro WHAMPointing::GetProperty, name = name, shortname = shortname, $
  ndp = ndp, vel = vel, data = data, var = var, $
  glon = glon, glat = glat, vlsr = vlsr, vgeo = vgeo, $
  day = day, time = time, block = block, pointing = pointing, $
  int = int, fit = fit, $
  params = params, ext_params = ext_params, spectrum = spectrum
  
  name = self.name
  shortname = self.shortname
  ndp = N_ELEMENTS(self.spectrum)
  if arg_present(vel) then vel = self.spectrum.vel
  if arg_present(data) then data = self.spectrum.data
  if arg_present(var) then var = self.spectrum.var
  glon = self.glon
  glat = self.glat
  vlsr = self.vlsr
  vgeo = self.vgeo
  day = self.day
  time = self.time
  block = self.block
  pointing = self.pointing
  int = self.int
  fit = self.fit
  params = self.params
  ext_params = self.ext_params
  spectrum = self.spectrum
  
end

;+
; :Description:
;    Initialize a WHAMPoining object. Called after creation.
;
; :Params:
;    file : in, optional, type=string
;      If preseent, load pointing from a WHAM FITS file.
;
; :Keywords:
;    ftsext : in, optional, type=string
;      the FITS extention to load the spectrum from
;    test : in, optional, type=boolean
;      create a test pointing
;
; :Author: Matt Haffner
;-
function WHAMPointing::Init, file, test = test, _extra = _extra

  ;; create a synthetic pointing for testing
  if KEYWORD_SET(test) then begin 
    self.CreateTest
  endif else begin 
    ;; load a pointing from a FITS file
    if n_elements(file) ne 0 then begin 
      self.Read, file, _extra = _extra
    endif else begin
      self.spectrum = WHAMSpectrum()
    endelse
  endelse

  ;; provide a static full integration
  if self.spectrum.ndp gt 0 then self.int = self.Integrate()

  ;; need to return false here if init fails...
  return, 1

end

;+
; :Description:
;    Structure definition for WHAMPointing
;
; :Author: Matt Haffner
;-
pro WHAMPointing__define
    
  on_error, 2
  if double(!version.release) lt 8.1D then message, 'Sorry, requires IDL version 8.1 or above.'
  
  struct = {WHAMPointing, $
            name: string(''), $
            shortname: string(''), $
            spectrum: WHAMSpectrum(), $
            glon: !values.f_nan, glat: !values.f_nan, $
            vlsr: float(0), vgeo: float(0), $
            day: long(0), time: float(0), $
            block: fix(0), pointing: fix(0), $
            params: hash(), $
            ext_params: hash(), $ 
            int: !values.f_nan, $
            fit: WHAMFit(), $
            inherits IDL_Object $
          }
            
end
  