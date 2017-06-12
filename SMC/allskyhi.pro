;This program reads in the block list for the smc. It then processed the ae correction;
;If the keyword save is set, it will save the files as smc_processed
;It then checks whther or not you also want to save the corresponding hi data.

pro read_smc, smc, save = save, smc_hi = smc_hi

  smc = readblocklist('blocks.ha.lst', ext='ATMSUB')
  ;fixes the scaling for night t0 night variations
  raw = apply_ae(smc, /reverse, /no_lookup, ta= 0.93, /no_degrad)
  smc=apply_ae(raw)
  if KEYWORD_SET(save) then save, smc, filename = '/d/wham/bsmart/MS/smc_ha_corrected.dat', /compress
    raw = apply_ae(smc, /reverse, /no_lookup, ta= 0.93, /no_degrad)
	smc=apply_ae(raw)
  if ARG_PRESENT(smc_hi) then begin
    min_l = min(smc.glon, max = max_l)
    min_b = min(smc.glat, max = max_b)
    smc_hi = exthi(0, 360, -90, 90, vmin = -400, vmax = 0, DATADIR = '/d/wham3/hi/lab_data', /wham)
    if KEYWORD_SET(save) then save, smc_hi, filename = '/d/wham/bsmart/smc/smc_hi.dat', /compress
  endif
  
end
