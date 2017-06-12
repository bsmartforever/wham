FUNCTION Vel_Stretch,  Lambda, v_0=v_0, v_1=v_1, v_2=v_2

  ;; This function returns a 'standard' velocity vector
  ;;  that stretches the scale out as a function
  ;;  of the wavelength. The ring pattern is smaller
  ;;  for smaller wavlengths. 
  ;; Look at /d/wham2/madsen/Image_Sizes and printouts/notes
  ;;  for more details.

  ;; makeSpect params from the makeSpect.mpd
  ;;  file, current as of 02/14/01
  ;; Account for other non-linearities in the vel scale
  ;; CHANGE THIS IF WE RE-DO THE NON-LINEARITY STUFF 
  ;;  WITH THE H-ALPHA/D-ALPHA LAMP!

  v_0 = keyword_set(v_0) ? v_0 : -112.31088
  v_1 = keyword_set(v_1) ? v_1 : 2.083
  v_2 = keyword_set(v_2) ? v_2 : -0.00036

  ;; Compute the delta_v(1/2) 
  ;; Lambda must be in Angstroms

  Delta = 23.126127-0.0035215291*Lambda   
  Ndp = 133  ;; Standard number of data points in vector
  v = FLTARR(Ndp)
  I_Ref = 33    ;; Reference point to stretch out to
                ;; Normally = 33, last data point in Halpha

  FOR j = 0, Ndp-1 DO BEGIN
    i = v_1 LT 0 ? Ndp - 1 - j : j  ; start at 132 if counting rings from red
    v[j] = v_0 + v_1 * (i) + v_2 * (i)^2 - Delta*( 1 + (1.0/(Ndp-1-I_Ref))*(I_ref-j) )
  ENDFOR

  RETURN, v

END
