function fix_trans, map, t0_new

  out = map

  for i = 0, n_elements(map)-1 do begin 
    header = headfits(map[i].name, exten = 0)
    ps_header = headfits(map[i].name, exten = 2)

    atmcor_old = fxpar(ps_header, 'ATMCOR')
    airmass = fxpar(header, 'AIRMASS')

    t0_old = exp(-alog(atmcor_old)/airmass)
;    print, atmcor_old, airmass, t0_old

    if abs(t0_old - 0.95) le 0.01 then begin 
      print, 'Fixing element: ', strtrim(i, 2)

      atmcor_new = Exp(-1.0*alog(t0_new)*airmass)

      out[i].data = (atmcor_new/atmcor_old) * map[i].data
      out[i].var = (atmcor_new/atmcor_old)^2.0 * map[i].var
    endif 

  endfor

  return, out

end
