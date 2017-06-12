; docformat = 'rst rst'

;+
; :Author: haffner
;-

function WHAMFit::CalcFit, vel, bkg = bkg, atmos = atmos, gauss = gauss, $
            useorig = useorig, geozero = geozero

  if KEYWORD_SET(useorig) then vel = self.orig.vel
  if ~isa(geozero) then geozero = 0 

  params = list()
  if KEYWORD_SET(bkg) then begin 
    params.Add, self.bkg, /extract
    nbkg = self.nbkg
  endif else begin
    params.Add, 0.0
    nbkg = 1
  endelse

  if KEYWORD_SET(atmos) then begin 
    for i = 0, self.natmos-1 do begin 
      params.Add, (self.atmos_mean)[i] + geozero
      params.Add, (self.atmos_width)[i]
      params.Add, (self.atmos_area)[i] * self.atmos
    endfor
  endif

  if isa(gauss) then begin
    
    if isa(gauss, 'INT', /array) or isa(gauss, 'LONG', /array) then begin
      foreach g, gauss do begin
        if g lt 1 or g gt self.ngauss then begin
          message, 'GAUSS keyword element "' + strtrim(g, 2) + '" is out of bounds: [1, ' + strtrim(g, 2) + ']; skipped.', /info
          continue
        endif else begin          
          params.Add, (self.mean)[g-1]
          params.Add, (self.width)[g-1]
          params.Add, (self.area)[g-1]
        endelse
      endforeach
    endif else begin
      message, 'GAUSS keyword is not an integer array; skipping.', /info
    endelse
  endif

  fit = whamgauss(vel, params.ToArray(), nbkg)
  return, fit
  
end

FUNCTION WHAMFit::OverplotRegions, vel_convert, y, style

  if N_PARAMS() eq 2 then style = '-k2'

  regionPlots = list()
  foreach r, self.regions do begin
    regionPlots.Add, plot([r[0], r[1]] + vel_convert, [0, 0] + y, style, /over)
  endforeach
  
  return, regionPlots
end

;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    file_or_pointing
;    fts_ext
;
;
;
; :Author: haffner
;-
pro WHAMFit::AddSpectrum, file_or_pointing, fts_ext

  if isa(file_or_pointing, 'WHAMPointing') then $
    file = file_or_pointing.name $
  else $
    file = file_or_pointing

  ;; try and load a spectrum related to the fit
  orig_fits = mrdfits(file, fts_ext, ext_header, status = status, /silent)

  if status eq 0 then begin 
    self.orig = WHAMSpectrum(orig_fits.velocity, orig_fits.data, orig_fits.variance)
  endif else begin
    message, 'Can''t find original fit data extension in ' + $
      file + '(' + fts_ext + ')' + '.', /info
  endelse

end

;+
; :Description:
;    Populate object with an empty fit. This is safer than leaving
;    the vectors with !NULL object references.
;
; :Author: Matt Haffner
;-
pro WHAMFit::Null
    self.chi2 = float(0)
    self.nbkg = fix(0)
    self.ngauss = fix(0)
    self.natmos = fix(0)
    self.nregions = fix(0)
    self.regions = list()
    self.bkg = list()
    self.sd_bkg = list()
    self.mean = list()
    self.sd_mean = list()
    self.width = list()
    self.sd_width = list()
    self.atmos = float(0)
    self.sd_area = list()
    self.atmos_mean = list()
    self.atmos_width = list()
    self.atmos_area = list()
    self.orig = WHAMSpectrum()
end

;+
; :Description:
;    Describe the procedure.
;
;
;
; :Keywords:
;    chi2
;    nbkg
;    ngauss
;    natmos
;    nregions
;    regions
;    atmos_level
;    bkg
;    sd_bkg
;    mean
;    sd_mean
;    width
;    sd_width
;    area
;    sd_area
;    orig
;    atmos_mean
;    atmos_width
;    atmos_area
;
; :Author: haffner
;-
pro WHAMFit::GetProperty, chi2 = chi2, $
  nbkg = nbkg, ngauss = ngauss, natmos = natmos, nregions = nregions, regions = regions, $
  atmos_level = atmos_level, bkg = bkg, sd_bkg = sd_bkg, mean = mean, sd_mean = sd_mean, $
  width = width, sd_width = sd_width, area = area, sd_area = sd_area, $
  orig = orig, $
  atmos_mean = atmos_mean, atmos_width = atmos_width, atmos_area = atmos_area

  chi2 = self.chi2
  nbkg = self.nbkg
  ngauss = self.ngauss
  natmos = self.natmos
  nregions = self.nregions
  regions = self.regions
  atmos_level = self.atmos
  bkg = self.bkg.ToArray()
  sd_bkg = self.sd_bkg.ToArray()
  mean = self.mean.ToArray()
  sd_mean = self.sd_mean.ToArray()
  width = self.width.ToArray()
  sd_width = self.sd_width.ToArray()
  area = self.area.ToArray()
  sd_area = self.sd_area.ToArray()
  
  atmos_mean = self.atmos_mean.ToArray()
  atmos_width = self.atmos_width.ToArray()
  atmos_area = self.atmos_area.ToArray()

  orig = self.orig
;  orig_vel = self.orig.vel.ToArray()
;  orig_data = self.orig.data.ToArray()
;  orig_var = self.orig.var.ToArray()
end

pro WHAMFit::SetProperty, nbkg = nbkg, ngauss = ngauss, $
  bkg = bkg, mean = mean, width = width, area = area
  
  if isa(nbkg) then self.nbkg = nbkg
  if isa(ngauss) then self.ngauss = ngauss
  if isa(bkg) then self.bkg = list(bkg, /extract)
  if isa(mean) then self.mean = list(mean, /extract)
  if isa(width) then self.width = list(width, /extract)
  if isa(area) then self.area = list(area, /extract)
  
end

;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    p
;
;
;
; :Author: haffner
;-
function WHAMFit::Init, p
  
  if ~isa(p) then begin
    ;; init an empty fit object
    self.Null
    return, 1
  endif

  if (p.ext_params).HasKey('CHI2') then begin 
    head = p.ext_params

    self.chi2 = head['CHI2']
    self.nbkg = head['BORDER'] + 1
    self.ngauss = head['NGAUSS']
    if head.HasKey('ATMOS') then self.atmos = head['ATMOS']
    
    self.bkg = list(length = self.nbkg)
    self.sd_bkg = list(length = self.nbkg)
    
    for i = 0, self.nbkg-1 do begin
      (self.bkg)[i] = head['BKG' + strtrim(i+1,2)]
      (self.sd_bkg)[i] = head['BKGSD' + strtrim(i+1,2)]
    endfor

    self.mean = list(length = self.ngauss)
    self.sd_mean = list(length = self.ngauss)
    self.width = list(length = self.ngauss)
    self.sd_width = list(length = self.ngauss)
    self.area = list(length = self.ngauss)
    self.sd_area = list(length = self.ngauss)

    for i = 0, self.ngauss-1 do begin 
      (self.mean)[i] = head['MEAN' + strtrim(i+1,2)]
      (self.sd_mean)[i] = head['MEANSD' + strtrim(i+1,2)]
      (self.width)[i] = head['WIDTH' + strtrim(i+1,2)]
      (self.sd_width)[i] = head['WIDTHSD' + strtrim(i+1,2)]
      (self.area)[i] = head['AREA' + strtrim(i+1,2)]
      (self.sd_area)[i] = head['AREASD' + strtrim(i+1,2)]
    endfor
    
    self.natmos = total(strmatch((head.Keys()).ToArray(), 'ATMEAN*'))
    self.atmos_mean = list(length = self.natmos)
    self.atmos_width = list(length = self.natmos)
    self.atmos_area = list(length = self.natmos)
    
    for i = 0, self.natmos-1 do begin 
      (self.atmos_mean)[i] = head['ATMEAN' + strtrim(i,2)]
      (self.atmos_width)[i] = head['ATWDTH' + strtrim(i,2)]
      (self.atmos_area)[i] = head['ATAREA' + strtrim(i,2)]
    endfor

    self.nregions = total(strmatch((head.Keys()).ToArray(), 'RSTART*'))
    self.regions = list(length = self.nregions)
    
    for i = 0, self.nregions-1 do begin
      (self.regions)[i] = list(head['RSTART' + strtrim(i+1,2)], head['REND' + strtrim(i+1,2)])
    endfor
        
    ;; Success!
    return, 1
        
  endif else begin
    message, '[' + p.name + ' (' + (p.ext_params)['EXTNAME'] + ')] contains no fit.', /info
    return, 0
  endelse

end

;+
; :Description:
;    Describe the procedure.
;
;
;
;
;
; :Author: haffner
;-
pro WHAMFit__define

  on_error, 2
  if double(!version.release) lt 8.0 then message, 'Sorry, requires IDL version 8.0 or above.'

  struct = {WHAMFit, $
            chi2: float(0), $
            nbkg: fix(0), $
            ngauss: fix(0), $
            natmos: fix(0), $
            nregions: fix(0), $
            regions: list(), $
            bkg: list(), $
            sd_bkg: list(), $
            mean: list(), $
            sd_mean: list(), $
            width: list(), $
            sd_width: list(), $
            area: list(), $
            sd_area: list(), $
            atmos: float(0), $
            atmos_mean: list(), $
            atmos_width: list(), $
            atmos_area: list(), $
            orig: WHAMSpectrum(), $
            inherits IDL_Object $
            }

end