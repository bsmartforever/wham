pro doall

read_ms, ms, /save
  process_ms, ms, iha, mean_v, ms_hi, nhi, vhi, nostars = nostars


  gal2mag, ms_hi.glon,ms_hi.glat, hi_mlon, hi_mlat

  gal2mag, ms.glon,ms.glat, mlon, mlat

  k = n_elements(hi_mlon)

  mlon_max=max(mlon)
  mlon_min=min(mlon)
  mlat_max=max(mlat)
  mlat_min=min(mlat)

  msize= size(ms_hi[0])

  ms_hi_small=replicate(ms_hi[0],1)
  nhi_small=nhi[0]

  flag=0


  print, max(ms.vel), min(ms.vel)

;trying to get the hi map to be the correct size and not the weird diamond thing it is otherwise
  for i=0, k-1  do begin

  if hi_mlon[i] ge mlon_min and hi_mlon[i] le mlon_max then begin

    if hi_mlat[i] ge mlat_min and hi_mlat[i] le mlat_max then begin

      if i eq 0 then begin

      ms_hi_small[i] =  ms_hi[i]
      flag=1
      endif else begin

      if flag ne 1 then begin

      ms_hi_small[0]= ms_hi[i]

      nhi[0]=nhi[i]

      flag=1

      endif else begin

      ms_hi_small = [ms_hi_small, ms_hi[i]]
      nhi_small= [nhi_small, nhi[i]]

      endelse

      endelse

    endif

    endif

  endfor

  @colplstart
	device, file=' ms_hi_mid.ps'

	@mycmap

	whammap, ms_hi_small, 0, 0, iha, $
  		/lin, smooth=2, smgrid=.45, tv=tv, mimage=mimage, $
  		zmin=0.01, zmax=1, xmargin=[10,0], ymargin=[4,0], scale=1.5, /gal2mag, /magellanic, charthick=4.0, charsize= 1.5

      MinData=min(ms_hi.data) & MaxData=max(ms_hi.data)
         cgColorbar, Divisions=4, Minor=5, Format='(F0.2)',range=[mindata, maxdata], /fit; title='log( N!DHI!N / cm!U-2!N )'
;         colorbar, ncolors = !d.table_size-1, range = [mindata, maxdata], $
;           position = [0.93, 0.1+0.095, 0.95, 0.80+0.025], $
;           orientation=0,right=1,charsize=charsize,charthick=charbig,$
;           _extra=extra,color=cgcolor('black'),title=title, tickFORMAT='(F6.2)’

;, POSITION=[0.15, 1.0, 0.95, 1.05],  /vertical use this when the horizontal bars don't work
;POSITION=[0.15, 1.0, 0.95, 1.05],  /vertical

gal2mag, ms_hi_small.glon,ms_hi_small.glat, hi_mlon_small, hi_mlat_small



x = -reduceto180(hi_mlon_small)
y = hi_mlat_small
z = nhi_small

print, z

contour, z, x, y, /irr, /overplot, levels=[.01,.05, .2, .6], $
  c_thick = [3,5,7,10], c_color = color(240), c_linestyle = [0], c_labels=[1,1,1,1]
;levels=[.005,.05,.5,1] levels for end
	device, /close
	set_plot,'x'

	  @colplstart
	device, file=' ms_mid.ps'


	@mycmap
	;whammap, ms[nostars], 0, 0, iha[nostars], smimage, /use, $
  ;		/lin, smooth=2, smgrid=0.5, tv=tv, mimage=mimage, $
  ;		zmin=0.01, zmax=0.6, xmargin=[10,0], ymargin=[4,0], scale=1.5, /MAGELLANIC, /GAL2MAG, charthick=4.0

   ;   MinData=min(ms.data) & MaxData=max(ms.data)
   ;   cgColorbar, Divisions=4, Minor=5, Format='(F0.2)', range=[mindata, maxdata],title='mRayleighs'm, /fit


  zmin = 0.01
  zmax = 10

  ;; linear version
  vmin=min(ms.vel)
  vmax=max(ms.vel)


  imap = (intmap(ms, vmin = vmin, vmax = vmax)/22.8) > 0

  ms.data=ms.data/22.8

  whammap, ms, 0, 0, imap, /useimage, /lin, $
    smooth=1, smgrid = 0.25, zmin = 0, zmax = 0.6, charthick=4.0, charsize=1.5, /MAGELLANIC, /GAL2MAG

    ;curspect, ms

  cgColorbar, Divisions=4, Minor=5, Format='(F0.2)', range=[0, 0.6], /fit, /vertical; title='Intensity [R]'


  ;colorbar, NCOLORS=!d.TABLE_SIZE-1, MINRANGE=zmin, MAXRANGE=zmax, $
  ;  title = 'Intensity [R]', FORMAT='(F6.2)', charsize = 1, $
  ;  /horizontal, /top, position = [0.91, 0.1, 0.93, 0.82]

  	gal2mag, ms_hi.glon,ms_hi.glat, mlon, mlat

x = -reduceto180(hi_mlon_small)
y = hi_mlat_small
z = nhi_small

print, z

contour, z, x, y, /irr, /overplot, levels=[.01,.05, .2, .6], $
  c_thick = [3,5,7,10] , c_color = color(240), c_linestyle = [0],c_labels=[1,1,1,1]

  ; c_thick = [3,5,7,10] and levels=[.01,.05, .2, .6], levels used for top

  ;levels=[.005,.05, .2, .4, .6, .8,1] use these levels for the fainter directions

  ;levels=[.005,.05,.5,1] levels for end

	device, /close
	set_plot,'x'

end
