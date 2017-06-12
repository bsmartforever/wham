@kplot

PRO make_nyquist_blocks, REGION=REGION, PS=PS, l_wide=l_wide, b_wide=b_wide, n_points=n_points
  
  ;; MAKE A NYQUIST-SAMPLED GRID AND ASSOCIATED BLOCKS
  
  ;; Inputs:
  ;;   Width: Beam diamater (1.0 for WHAM)
  ;;   L_Min/Max: Min and max longitude of area to cover
  ;;   B_Min/Max: Min and max latitude of area to cover
  ;;   NX_Blocks: Number of blocks along longitude axis
  ;;   NY_Blocks: Number of blocks along latitude axis
  ;;   Block_Zero: Starting poinfor block numbers
  ;;   filename:  Name of file to dump pointings.dat data

  ;; Keywords
  ;;   Region: 0 - If not specified, a prompt will ask for the min and max latitude and longitude to be used
  ;;           1 - Assumes SMC with L_Min = 290, L_Max = 315, B_Min = -55, B_Max = -35
  ;;	         2 - Assumes LMC with 
  ;;	         3 - Assumes MB with L_Min = 285, L_Max = 295, B_Min = -40, B_Max = -30

  ;; e.g. MAKE_NYQUIST_BLOCKS, REGION=3

  ;; Program based on Greg's Madsen's Nyquist code, but more user friendly.
  ;;
  ;; Modifications:
  ;;    06/2013 - Kat: Now accepts lontitude values between -180. to +360.
  ;;      This way they code can handle creating grids that overlap with lontitude = 0

  ;; Optional debug flags

  Plot_XY = 0
  Plot_Cart = 0
  Plot_Stereo = 1
  
  IF NOT Keyword_set(Region) then Region = 0 

  ;; Specify beam diameter/FWHM in degrees
  Width = 1.0                   
  
  ;; Specify total size of area in units of degrees

  IF Region eq 0 THEN BEGIN  
     print,''
     print,'Region not specified, please specify.'
     L_Min=''
     L_Max=''
     read,'Min GLon: ',L_Min
     read,'Max GLon: ',L_Max

     if (L_min le 0) AND (L_max le 0) then begin
        print,'Both the min and max longitudes are negitive. Adding 360 to these values.'
        l_min=l_min+360.
        l_max=l_max+360.
     endif
     l_min=min([l_min,l_max],max=l_max)
     print,'Min & Max GLON: ',L_Min,L_Max

     response=test_number([L_Min,L_Max],'Invalid range, Try again: ','Exiting Program',range=[-180.,360.0])
     L_Min=response(0)
     L_Max=response(1)
     B_Min=''
     B_Max=''
     read,'Min GLat: ',B_Min
     read,'Max GLat: ',B_Max
     b_min=min([b_min,b_max],max=b_max)
     response=test_number([B_Min,B_Max],'Invalid range, Try again: ','Exiting Program',range=[-90.0,90.0])
     B_Min=response(0)
     B_Max=response(1)

     print,'Min & Max GLAT: ',B_Min,B_Max
     print,''
     Block_Zero=''
     read,'Specify starting block number. Must be 4000+: ',Block_Zero
     Block_Zero=test_number(Block_Zero,'Invalid Starting Block, Try again: ','Exiting Program',range=[4000,10000]) ;; initial block number
     print,'Block Starting Number: ',fix(Block_Zero)
     filename=''
     read,'File name to save data blocks: ',filename
  ENDIF

  IF Region eq 1 THEN BEGIN
  ;; SMC
     print,''
     print,'Region Selected: Small Magellanic Cloud'
     print,''
     L_Min = 290
     L_Max = 315
     B_Min = -55
     B_Max = -35
     print,'Min & Max GLAT: ',B_Min,B_Max
     print,'Min & Max GLON: ',L_Min,L_Max 
     print,''
     Block_Zero = 2000  ;; initial block number  
     filename = 'smc_pointings.dat'  
  ;; Number of blocks on a side
  NX_Blocks = 6
  NY_Blocks = 6  
  ENDIF

  IF Region eq 2 THEN BEGIN
  ;; LMC
     print,''
     print,'Region Selected: Large Magellanic Cloud'
     print,''
     L_Min = 265   
     L_Max = 295
     B_Min = -45
     B_Max = -20
     print,'Min & Max GLAT: ',B_Min,B_Max
     print,'Min & Max GLON: ',L_Min,L_Max 
     print,'' 
     Block_Zero = 4000  ;; initial block number
     filename = 'lmc_pointings.dat' 
  ;; Number of blocks on a side
     NX_Blocks = 8
     NY_Blocks = 8  
  ENDIF
    
  IF Region eq 3 THEN BEGIN
  ;; Bridge
     print,''
     print,'Region Selected: Magellanic Bridge'
     print,''
     L_Min = 282
     L_Max = 302
     B_Min = -48
     B_Max = -27
     print,'Min & Max GLAT: ',B_Min,B_Max
     print,'Min & Max GLON: ',L_Min,L_Max  
     print,''
     Block_Zero = 3000  ;; initial block number
     filename = 'bridge_pointings.dat' 
  ;; Number of blocks on a side
     NX_Blocks = 6
     NY_Blocks = 7  
  ENDIF
     
  
  ;; Now get to real stuff
  
  ;; Total number of points on a side
  ;; This will be bigger than requested area due to warping
  
  NX = ROUND(ABS(L_Max-L_Min)/(0.5*Width))
  NY = ROUND(ABS(B_Max-B_Min)/(0.5*Width))+4

  if region eq 0 then begin
    if (NOT keyword_set(l_wide)) AND (NOT keyword_set(b_wide)) AND (keyword_set(n_points)) then begin
        l_wide=round(sqrt(n_points))/2.
        b_wide=round(sqrt(n_points))/2.
    endif else if (keyword_set(l_wide)) AND (NOT keyword_set(b_wide)) AND (keyword_set(n_points)) then begin
        b_wide=round(n_points/l_wide)/2.
    endif else if (NOT keyword_set(l_wide)) AND (keyword_set(b_wide)) AND (keyword_set(n_points)) then begin
        l_wide=round(n_points/b_wide)/2.
    endif else if ((NOT keyword_set(l_wide)) AND (NOT keyword_set(b_wide))) $
      OR ((keyword_set(l_wide)) OR (NOT keyword_set(b_wide))) AND (NOT keyword_set(n_points)) then begin
        b_wide=3.
        l_wide=3.
    endif


    ;; Number of blocks on a side
    NX_Blocks = fix(abs(L_Max-L_Min),type=2)/l_wide
    NY_Blocks = fix(abs(B_Max-B_Min),type=2)/b_wide  
  endif 
  
  PRINT, 'Original Grid size: ', STRTRIM(NX, 2), ' by ', STRTRIM(NY, 2)
  
  ;; FROM MIRIAD MANUAL...
  ;; NYQUIST'S THEOREM:
  ;; USE HEXAGONAL GRID WITH POINTINGS AT VERTICES OF EQUILATERAL TRIANGLES OF 
  ;; SIDE 1/SQRT(3) * BEAM
  
  ;; First, make a square grid
  ;; X-spacing is 1/sqrt(3) * Width
  ;; Y-spacing is 1/2 * Width
  
  xgrid = (1/sqrt(3) * width) * rebin( (findgen(nx)-nx/2), nx, ny)
  ygrid = (0.5 * width) * rebin( (findgen(1, ny)-ny/2), nx, ny)
  
  ;; Now make the Hexagonal grid
  ;; Shift x position of every other row
  ;;  by 0.5 * 1/(SQRT(3))  * Width
  
  FOR i = 0, NY-1, 2 DO xgrid[*, i] = xgrid[*, i] - Width*0.5/SQRT(3)
  
  IF Plot_XY THEN BEGIN
     ;; Show the grid positions in X/Y space
     PLOT, [0], XR = 1.2*[-NX/4., NX/4.], YR = 1.2*[-NY/4., NY/4.], xtitle = 'X', ytitle = 'Y'
     OPLOT, xgrid, ygrid, ps = 3
  ENDIF

  ;; CONVERT GRID SPACINGS FROM REGULAR GRID IN STEREOGRAPHIC TO POSITIONS
  ;; IN LAT/LON... 

  ;; Specify the center of the projection
  CRVAL = [AVG([L_Max, L_Min]), AVG([B_Max, B_Min])]
  
  ;; Do transformation
  WCSXY2SPH, xgrid, ygrid, longrid, latgrid, 4, CRVAL = crval

  
if keyword_set(ps) then begin


    name="Nyquist.eps"

    entry_device=!d.name
    set_plot,'PS'
    !p.font=0
    device,filename=name,bits_per_pixel=8,color=1
    device,/landscape,encapsulated=1, /helvetica
    device,xsize=10,xoffset=(11.0-10)*0.5,/inches
    device,ysize=10,yoffset=10,/inches


   color=cgColor('black',/triple)
   TVLCT,color,255
   !P.color=cgColor('black')

endif


  IF PLOT_Cart THEN BEGIN
     ;; Plot the grid points and blocks on screen, in Lon/Lat space
     ;;  but projected onto cartesian plot
     ;; Color code the blocks
     
     plot, [0], XR = [MAX(longrid), MIN(longrid)], YR = [MIN(latgrid), MAX(latgrid)], XS = 3, YS = 19
                                ;plot, [0], XR = [360, 0], $
                                ;      YR = CRVAL[1]+[-Y_Size, Y_Size], XS = 3, YS = 19
     
     LOADCT, 39
     FOR i = 0, NY-1 DO BEGIN
        FOR j = 0, NX-1 DO BEGIN
           oplot, [longrid[j, i]], [latgrid[j, i]], $
                  ps = 3
        ENDFOR
     ENDFOR
     KOPLOT, [L_Min, L_Min], [B_Min, B_Max]
     KOPLOT, [L_Max, L_Max], [B_Min, B_Max]
     
     KOPLOT, [L_Min, L_Max], [B_Min, B_Min]
     KOPLOT, [L_Min, L_Max], [B_Max, B_Max]
  ENDIF
  
  ;; Grab only points we need

if (L_Min lt 0) and (L_max gt 0) then BEGIN
   ind_low = WHERE(Longrid GE (L_Min+360.-0.5*Width) AND $
              Longrid LE 360. AND $
              Latgrid GE (B_Min-0.5*Width) AND $
              Latgrid LE (B_Max+0.5*Width) )

    ind_high = WHERE(Longrid GE 0. AND $
              Longrid LE (L_Max+0.5*Width) AND $
              Latgrid GE (B_Min-0.5*Width) AND $
              Latgrid LE (B_Max+0.5*Width) )

    ind=fltarr(N_ELEMENTS(ind_low)+N_ELEMENTS(ind_high))
    ind[0:N_ELEMENTS(ind_low)-1]=ind_low
    ind[N_ELEMENTS(ind_low):N_ELEMENTS(ind_low)+N_ELEMENTS(ind_high)-1]=ind_high
endif else begin

  Ind = WHERE(Longrid GE (L_Min-0.5*Width) AND $
              Longrid LE (L_Max+0.5*Width) AND $
              Latgrid GE (B_Min-0.5*Width) AND $
              Latgrid LE (B_Max+0.5*Width) )

endelse

  Longrid = Longrid[Ind]
  Latgrid = Latgrid[Ind]
  
  PRINT, "Reduced number of points:", N_ELEMENTS(Longrid) 
  
  
  ;; Make block number grid
  
  ;; Spacing for block numbering
  D_Lon = ABS(MAX(Longrid)-MIN(Longrid))/NX_Blocks
  D_Lat = ABS(MAX(Latgrid)-MIN(Latgrid))/NY_Blocks
  
  Bgrid = INTARR(N_ELEMENTS(Longrid))
  
  FOR i = 0, N_ELEMENTS(Longrid)-1 DO BEGIN
     if (l_min lt 0.) and (l_max gt 0.) then begin
        Row = (FLOOR((Latgrid[i]-MIN(Latgrid))/D_Lat)) <  (NY_Blocks-1)
        tmp_Longrid=Longrid
        tmp_Longrid[where(Longrid gt 180.)]=tmp_Longrid[where(Longrid gt 180.)]-360.
        tmp_Longrid=tmp_Longrid-min(tmp_Longrid)
        D_Lon = ABS(MAX(tmp_Longrid)-MIN(tmp_Longrid))/NX_Blocks
        Col = (FLOOR((tmp_Longrid[i]-MIN(tmp_Longrid))/D_Lon)) <  (NX_Blocks-1)
        Bgrid[i] = Col + Row*NX_Blocks + Block_Zero
     endif else begin
        Row = (FLOOR((Latgrid[i]-MIN(Latgrid))/D_Lat)) <  (NY_Blocks-1)
        Col = (FLOOR((Longrid[i]-MIN(Longrid))/D_Lon)) <  (NX_Blocks-1)
        Bgrid[i] = Col + Row*NX_Blocks + Block_Zero
     endelse
  ENDFOR
  
  PRINT, "Avg points per block: ", round(N_ELEMENTS(Longrid)/(NX_Blocks*NY_Blocks))
  
  
  IF PLOT_Stereo THEN BEGIN
  
     ;; Replot new grid on a sphere
     
     b_min = b_min-2
     b_max = b_max+2
     l_min = l_min-2
     l_max = l_max+2
     scale = 0.6
     

     !P.position = [.1, .15, .9, .9]
     MAP_SET, avg([b_min, b_max]), -avg([l_min, l_max]), 0,  /aitoff, $
              limit = [b_min, -l_max, b_max, -l_min], /noborder, $
              /iso, title = title, charsize = 2
     

     ;;kplot, [0], [0], XR = [MAX(longrid), MIN(longrid)], YR = [MIN(latgrid), MAX(latgrid)], XS = 3, YS = 19
     FOR i = 0, N_ELEMENTS(Longrid)-1 DO koplot, -[longrid[i]], [latgrid[i]], ps = 1, $
                                                 color = (Bgrid[i]-block_zero+1)*(255/(NX_Blocks*NY_Blocks)), $
                                                 symsize = 4.5

     ;; Outline the map
     
     Dlat = B_Max-b_min
     Dlon = L_Min LT L_Max ? L_Max-L_Min : L_Min-(L_Max-360)
     OPLOT, -L_Max+LINDGEN(1001)*dlon/1000, $
            INTARR(1001)+(B_Min+1.0e-2), /noclip
     OPLOT, -L_Max+LINDGEN(1001)*dlon/1000, $
            INTARR(1001)+(B_Max-1.0e-2), /noclip
     OPLOT, INTARR(1001)-(L_Min + 1.0e-2), $
            B_Min + LINDGEN(1001)*Dlat/1000.0, /noclip
     OPLOT, INTARR(1001)-(L_Max - 1.0e-2), $
            B_Min + LINDGEN(1001)*Dlat/1000.0, /noclip
     
     ;; Get good spacings
     Scale = 1.5
     lonlabadj = 1
     latlabadj = 1
     GRIDSPACING, L_Min, L_Max, Plonmin, Plonmax, Plondel, $
                  adjust = lonlabadj
     GRIDSPACING, B_Min, B_Max, Platmin, Platmax, Platdel, $
                  adjust = latlabadj
     
     ;; latitude grid and labels
     FOR lat = Platmin, Platmax, Platdel DO BEGIN
        OPLOT, -L_Max+LINDGEN(1000)*Dlon/1000.0, $
               INTARR(1000)+lat, line = 1, /noclip, color = 255
        XYOUTS, -(L_Max+Dlon*0.02*scale $
                  / (!x.window[1]-!x.window[0]) / cos(lat/!radeg)), $
                lat-.02, TEXTOIDL(strtrim(string(fix(lat)), 2)), $
                align = 1.0, charsize = scale, font = !P.font
     ENDFOR 
     
     ;; longtude grid and labels
     
     FOR lon = Plonmax, Plonmin, -Plondel DO BEGIN
        OPLOT, intarr(1000)-lon, $
               B_Min+lindgen(1000)*Dlat/1000.0, $
               line = 1, /noclip, color = 255
        latlab = B_Min-Dlat*0.03*scale/(!y.window[1]-!y.window[0])
        lontext = STRTRIM(STRING(FIX((lon))), 2)
        XYOUTS, -lon, latlab, $
                TEXTOIDL(lontext), $
                align = 0.5, charsize = scale, font = !P.font
     ENDFOR
  ENDIF
  
  xyouts,avg(!p.position[[0,2]]),avg([0,!p.position[1]])*.5,'Galactic Longitude [Degrees]',align=0.5,/normal,charsize=scale
  xyouts,avg([0,!p.position[0]])*.5,avg(!p.position[[1,3]]),'Galactic Latitude [Degrees]',align=0.5,/normal,charsize=scale,orientation=90.

  ;; For pointings.dat, need to sort by block number
  
  longrid = longrid[SORT(bgrid)]
  latgrid = latgrid[SORT(bgrid)]
  bgrid = bgrid[SORT(bgrid)]
  
  ;; Print out coords and block numbers to a pointings.dat file
 
  if filename eq '' then filename='NYQUIST.dat'
  filename=validate_extension(filename,'dat')
  print,''
  print,'Saving to file: ',filename

  OPENW, unit, filename, /GET_LUN
  FOR i = 0, N_ELEMENTS(bgrid)-1 DO  $
          PRINTF, unit, longrid[i], ' ', latgrid[i], '  ', bgrid[i], $
                  FORMAT = '(F6.2,A,F6.2,A,I4)'
  
  CLOSE, unit
  FREE_LUN, unit

if keyword_set(ps) then begin

    !p.multi=0

    device,/close_file
    set_plot,entry_device
    Close, /All

    set_plot,'x'
endif

END
