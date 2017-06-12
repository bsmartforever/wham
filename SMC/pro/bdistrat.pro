PRO bdistrat, map, map2, z, dm, da, de, $
  step = step, vrange = vrange, quiet = quiet

  minb = min(map.glat)
  maxb = max(map.glat)

  IF n_elements(step) EQ 0 THEN step = 0.85
  IF n_elements(vrange) EQ 0 THEN vrange = [-80, 80]

  z = fltarr((maxb - minb)/step + 10)
  dm = z
  da = z
  de = z
  
  i = 0
  
  WHILE ((minb + i*step) LE maxb) DO BEGIN
      zbin = where((minb + i*step - step/2.0) LE map.glat AND $
                   map.glat LE (minb + i*step + step/2.0))
      zbin2 = where((minb + i*step - step/2.0) LE map2.glat AND $
                   map2.glat LE (minb + i*step + step/2.0))

      z(i) = median(map[zbin].glat)

      IF NOT keyword_set(quiet) THEN $
        print,  'Doing bin' + string(i) + ' b =' + string(z[i]) + $
        string(n_elements(zbin)) + ' and ' + string(n_elements(zbin2)) + $
        ' elements'
;      print, (minb + i*step - step/2.0), (minb + i*step + step/2.0)

      IF n_elements(zbin) NE n_elements(zbin2) THEN $
        message, 'ERROR: zbins not equal in maps'

      darray = fltarr(n_elements(zbin))
      
      FOR j = 0, n_elements(zbin)-1 DO BEGIN
          zindx = zbin(j)
          vbin = where(vrange(0) LE map(zindx).vel AND $
                       map(zindx).vel LE vrange(1))
          vbin2 = where(vrange(0) LE map2(zindx).vel AND $
                       map2(zindx).vel LE vrange(1))
          darray[j] = $
            int_tabulated(map[zindx].vel[vbin], map[zindx].data[vbin]) / $
            int_tabulated(map2[zindx].vel[vbin2], map2[zindx].data[vbin2])
          
      ENDFOR
      dm[i] = median(darray, /even)
      da[i] = avg(darray)
      de[i] = meanabsdev(darray, /median)/sqrt(n_elements(darray))
      
      i = i+1
  ENDWHILE
  z = z[0:i-1]
  dm = dm[0:i-1]
  da = da[0:i-1]
  de = de[0:i-1]
  
END

