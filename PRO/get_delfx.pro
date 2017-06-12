pro get_delfx, date

date_i=date+20000000L
; Compute the time dependent degradation offset, slope of degradation is 0.0000365 per day starting 20090101
  yr_obs=fix(date_i/10000L)
  mn_obs=fix((date_i-yr_obs*10000L)/100L)
  dy_obs=fix(date_i-(yr_obs*10000L+mn_obs*100L))
  jdcnv,yr_obs,mn_obs,dy_obs,0,jd
  jdcnv,2009,01,01,0,jd_start
  mjd=jd-jd_start
  delflx=mjd*0.0000365
  print,date_i,' : using ln(I) offset value of: ',delflx

  end
