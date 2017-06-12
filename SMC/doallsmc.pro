;This file calls the read_smc function, the process_smc. Read_smc reads in the smc files, does ae corrections,
; and then saves the fules so they can be read quickly in the future. If /save us used, it will not right new
;files but only read the saved files. You have to go and change read_smc manually if you want to do induvidual
; processing

; NOTE: SMC is uncorrected anything. It is the images which are corrected. So, smc needs nostars

pro doallsmc
;this can be commented in and out depending on if it changes
;read_smc, smc, /save, /smc_hi
;processing converts both the ha intensity to rayleighs and the hi intensity to proper units
  process_smc, smc, iha, mean_v, smc_hi, nhi, vhi,second_mom_ha, second_mom_hi, nostars = nostars, /restore
  imap = (intmap(smc[nostars], vmin = 60, vmax = 210, moment=0)/22.8) > 0
  vmin=min(smc.vel)
  vmax=max(smc.vel)
  x = -reduceto180(smc_hi.glon)
  y = smc_hi.glat
  ;Don't think I need this?
  ;z = nhi

  ;plothist, imap, xrange=[0,3], bin=.1, thick=1, charthick=1
  @plot_smc_ps
  @plot_smc_v_ps
  @plot_smc_sm_ps

  ;Correcting for nyquist sampling by smoothing
  gridmap, smc, outgrid, xc,yc, z=z, vmin=60, vmax=210, moment=0
  centlon=mean(smc.glon)
  centlat=mean(smc.glat)
  calclc, smc, 1, vmin=60, vmax=210, radius=20, center=[centlon,centlat]

  ;temperture estimated
  t=10000.00
  ;distance in kilaparsecs
  d=61.00
  ;los line of sight in parsecs
  ;los=4750.00

  ;tdtest, smc, iha, los, /sphere
  ;tdtest, smc, iha, los2, /ell1
  tdtest, smc[nostars], imap, los3, /ell2
  ;tdtest, smc, iha, los4, /ell3
  ;Here check where the sphere didn't cover everything and set it to 1 kpc
  los_fix=where(FINITE(los3) eq 0, /null)
  los3[los_fix]=0
  ;Now set everything bellow the rayleigh threshold to zero
  los_fix=where(iha lt 0.03)
  los3[los_fix]=0

 ; ha_mass, los, ne2, z, iha, t, d, totmas, totmas_c
 ; print, "Spherical total mass = ", totmas_c
  ha_mass, los3, ne2, z, iha, t, d, totmas, totmas_c
 ; print, "Ellipsoid 1 total mass = ", totmas_c
  ;ha_mass, los3, ne2, z, iha, t, d, totmas, totmas_c
  ;print, "Ellipsoid 2 total mass = ", totmas_c
  ;ha_mass, los4, ne2, z, iha, t, d, totmas, totmas_c
  ;print, "Ellipsoid 3 total mass = ", totmas_c

  ; ;nel is electron density
  ; em=2.75*(t/10^4)^0.924*(iha)
  ; em_corrected=2.75*(t/10^4)^0.924*(z)
  ; ;print, em_corrected
  ; nel2=em/los
  ; ;comparing both
  ; nel2_c=em_corrected/los
  ; ;print ,nel2
  ; noz=where(nel2 gt 0, /null)
  ; noz_c=where(nel2_c gt 0, /null)
  ; nel=sqrt(nel2[noz])
  ; nel_c=sqrt(nel2_c[noz_c])

  ; ;Okay, distance is in kpc, but line of site is in parsecs? Is that how it should be?
  ; mass=8.26*(d)^2*em[noz]*(nel)^(-1)
  ; mass_c=0.659*(d)^2*em_corrected[noz_c]*(nel_c)^(-1)
  ; totmas=total(mass)
  ; totmas_c=total(mass_c)
   print, totmas
   print, totmas_c

   map=smc[nostars]


  ; whammap, map, 0, 0, iha, /use, $
 ; /lin, zmin=.01, zmax=.6, xmargin=[6,0], ymargin=[4,0], $
 ; scale=1.5, beamrad=0.25, cbottom=1, smooth=1, missing=255, background ="white", smgrid=.25
  
;colorbar, position=[0.19, 0.05, 0.90, 0.10], range=[.01, .6], bottom=1, ncolors=252, $
;  title = 'v!dLSR!n [km s!u-1!n]'

 ; MinData=min(smc_hi.data) & MaxData=max(smc_hi.data)
 ; cgColorbar, Divisions=4, Minor=5, Format='(F0.2)',range=[.01, .6], /fit, /vertical, title = 'Intensity [Rayleighs]'
  
;x = -reduceto180(smc_hi.glon)
;y = smc_hi.glat
;z = nhi

;contour, z, x, y, /irr, /overplot, levels=[.5, 1, 5, 10], $
;  c_thick = [1,5,7,12], C_LABELS = [1, 1, 1, 1], c_color = color(0), c_linestyle = [0]


  ;cgScatter2D, alog(iha), alog(nhi)
end
