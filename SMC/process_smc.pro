pro process_smc, smc, IHa, Mean_V, smc_hi, nhi, vhi,second_mom_ha, second_mom_hi, $
  vmin = vmin, vmax = vmax, nostars = nostars, restore = restore, save=save

  ;for some reason restore keyword isn't working, so manyually restoring for now
  if KEYWORD_SET(restore) then restore, './smc_ha_corrected.sav'
  if KEYWORD_SET(restore) then restore, './smc_hi.dat'

  ;restore, './smc_ha_corrected.sav'
  ;restore, './smc_hi.dat'
  
  if ~isa(vmin) then vmin = 100
  if ~isa(vmax) then vmax = 200

  restore, '/d/wham/bsmart/PRO/data/sao.dat'
  findstars, smc, sao6, closemap, closestars, nclose, closedist

  bad_pointings = list()
  foreach e, closemap do bad_pointings.Add, e, /extract
  bad_pointings = bad_pointings.ToArray()
  bad_pointings = bad_pointings[uniq(bad_pointings, sort(bad_pointings))]

  nostars = list(indgen(n_elements(smc)), /extract)
  nostars.Remove, bad_pointings
  nostars = nostars.ToArray()

  ;; / 22.8 converts I_Ha to ~ Rayleighs
  iha = intmap(smc[nostars], vmin = vmin, vmax = vmax, moment=0)/22.8> 0
  mean_v = intmap(smc[nostars], vmin = vmin, vmax = vmax, moment=1)
  second_mom_ha = intmap(smc[nostars], vmin = vmin, vmax = vmax, moment=2)

  if N_ELEMENTS(smc_hi) ne 0 then begin
    ihi = intmap(smc_hi, vmin = vmin, vmax = vmax, /hi, moment=0)
    nhi = 1.823e-2 * ihi ;; scales to units of 10^20 cm^-2
    smc_hi_v = smc_hi
    smc_hi_v.data = smc_hi_v.data*smc_hi_v.vel
    vhi = intmap(smc_hi_v, vmin = vmin, vmax = vmax, /hi, moment=1)
    second_mom_hi= intmap(smc_hi_v, vmin = vmin, vmax = vmax, moment=2)
  endif
  if KEYWORD_SET(save) then save, smc, filename = '/d/wham/bsmart/MS/smc_ha_clean.dat', /compress
end
