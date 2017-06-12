pro process_ms, ms, IHa, Mean_V, ms_hi, nhi, vhi, $
  vmin = vmin, vmax = vmax, nostars = nostars, restore = restore

  restore, './ms_mid.dat'
  restore, './ms_mid_hi.dat'

  
  if ~isa(vmin) then vmin = -150
  if ~isa(vmax) then vmax = -50

  restore, '/d/wham/pro/data/sao.dat'

;This is convoluted but the input file is in the wrong units so I convert back to galactic
  ; ms=ha
  ; ms2=ha
  ; ms_hi=hi
  ; mlon=ms2.glon
  ; mlat=ms2.glat
  ; mag2gal,mlon,mlat,glon,glat
  ; ms2.glon=glon
  ; ms2.glat=glat

  ;findstars, ms2, sao6, closemap, closestars, nclose, closedist
  findstars, ms, sao6, closemap, closestars, nclose, closedist

  bad_pointings = list()
  foreach e, closemap do bad_pointings.Add, e, /extract
  bad_pointings = bad_pointings.ToArray()
  bad_pointings = bad_pointings[uniq(bad_pointings, sort(bad_pointings))]

  ;nostars = list(indgen(n_elements(ms2)), /extract)
  nostars = list(indgen(n_elements(ms)), /extract)
  nostars.Remove, bad_pointings
  nostars = nostars.ToArray()

  ; ms_hi2=replicate({glon:0.0,glat:0.0,data:0.0,vel:0.0},n_elements(ms_hi.mlon))
  ; ms_hi2.glon=ms_hi.mlon
  ; ms_hi2.glat=ms_hi.mlat
  ; ms_hi2.data=ms_hi.data
  ; ms_hi2.vel=ms_hi.vlsr

  ; mlon=ms_hi2.glon
  ; mlat=ms_hi2.glat
  ; mag2gal,mlon,mlat,glon,glat
  ; ms_hi2.glon=glon
  ; ms_hi2.glat=glat

  ;; / 22.8 converts I_Ha to ~ Rayleighs
  iha = intmap(ms, vmin = vmin, vmax = vmax) / 22.8


   ;save, ms, filename = '/d/wham/bsmart/MS/ms_processed.dat', /compress

  if N_ELEMENTS(ms_hi) ne 0 then begin
    ihi = intmap(ms_hi, vmin = vmin, vmax = vmax, /hi)
   nhi = 1.823e-2 * ihi ;; scales to units of 10^20 cm^-2
   ms_hi_v = ms_hi
   ms_hi_v.data = ms_hi_v.data*ms_hi_v.vel
    vhi = intmap(ms_hi_v, vmin = vmin, vmax = vmax, /hi) / ihi
  endif
end
