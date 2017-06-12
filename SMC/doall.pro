pro doall


  read_smc, smc, /save
  process_smc, smc, iha, mean_v, smc_hi, nhi, vhi, nostars = nostars
  !p.thick = 10 
  !p.charthick = 10 
  restore, 'MCELS/smc_small.dat'
  @plot_smc_ps
  @plot_smc_hi_ps


end

