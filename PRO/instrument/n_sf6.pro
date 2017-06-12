function n_sf6, p, T

  R = 8.314472D  ; Pa - m^3 / K - mol
  A = 1.700D-5   ; m^3 / mol
  B = 47.3D-10   ; m^6 / mol^2 @ 22.9 C

  p_Pa = p * 10 * 133.322368D   ; convert from cm Hg to Pa
  T_K = T + 273.15  ; convert from C to K

  return, ( ( A * (p_Pa / (R * T_K)) ) + ( B * (p_Pa / (R * T_K))^2 ) ) + 1
  
end