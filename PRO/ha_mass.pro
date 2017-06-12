pro ha_mass, los, ne2, z, iha, t, d,  totmas, totmas_c

	;corrected means we are correcting for overcounting. z is the smoothing of the iha values

  em=2.75*(t/10^4)^0.924*(iha)
  em_corrected=2.75*(t/10^4)^0.924*(z)
  ;print, em_corrected
  nel2=em/los
  ;comparing both
  nel2_c=em_corrected/los
  ;print ,nel2
  noz=where(nel2 gt 0, /null)
  noz_c=where(nel2_c gt 0, /null)
  nel=sqrt(nel2[noz])
  nel_c=sqrt(nel2_c[noz_c])

  ;Calculate the mass
  ;The uncorrected just uses the equation from Kat's bridge paper
  ;Corrected takes into account the pixel size difference and solid angle difference
  mass=8.26*(d)^2*em[noz]*(nel)^(-1)
  mass_c=0.659*(d)^2*em_corrected[noz_c]*(nel_c)^(-1)

  totmas=total(mass)
  totmas_c=total(mass_c)

  END