@kplot

PRO MAKE_NYQUIST_BLOCKS
  
  ;; MAKE A NYQUIST-SAMPLED GRID AND ASSOCIATED BLOCKS
  
  ;; Inputs:
  ;;   Width: Beam diamater (1.0 for WHAM)
  ;;   L_Min/Max: Min and max longitude of area to cover
  ;;   B_Min/Max: Min and max latitude of area to cover
  ;;   NX_Blocks: Number of blocks along longitude axis
  ;;   NY_Blocks: Number of blocks along latitude axis
  ;;   Block_Zero: Starting point for block numbers
  ;;   filename:  Name of file to dump pointings.dat data

  ;; Optional debug flags
  
  Plot_XY = 0
  Plot_Cart = 0
  Plot_Stereo = 1
  
  ;; Specify beam diameter/FWHM in degrees
  Width = 1.0                   
  
  ;; Specify total size of area in units of degrees
  
  ;; SMC
;  L_Min = 290
;  L_Max = 315
;  B_Min = -55
;  B_Max = -35
;  
;  ;; Number of blocks on a side
;  NX_Blocks = 6
;  NY_Blocks = 6
;  
;  Block_Zero = 2000  ;; initial block number
;  
;  filename = '~/Astro/Data/Pointings/smc_pointings.dat'
  
  ;; Bridge?
  
  ;L_Min = 285
  ;L_Max = 295
  ;B_Min = -40
  ;B_Max = -30
  
  ;; Number of blocks on a side
  ;NX_Blocks = 3
  ;NY_Blocks = 3
  
  ;Block_Zero = 3000  ;; initial block number
  
  ;filename = '~/wham/SMC/bridge_pointings.dat'
  
  ; Smith Cloud
  L_Min = 20
  L_Max = 70
  B_Min = -50
  B_Max = 0
  
  NX_Blocks = 12
  NY_Blocks = 18
  
  Block_Zero = 5000
  
  filename = '~/Desktop/smith_pointings.dat'
  
  ;; Now get to real stuff
  
  ;; Total number of points on a side
  ;; This will be bigger than requested area due to warping
  
  NX = ROUND(ABS(L_Max-L_Min)/(0.5*Width))
  NY = ROUND(ABS(B_Max-B_Min)/(0.5*Width))+4
  
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
  
  Ind = WHERE(Longrid GE (L_Min-0.5*Width) AND $
              Longrid LE (L_Max+0.5*Width) AND $
              Latgrid GE (B_Min-0.5*Width) AND $
              Latgrid LE (B_Max+0.5*Width) )
  Longrid = Longrid[Ind]
  Latgrid = Latgrid[Ind]
  
  PRINT, "Reduced number of points:", N_ELEMENTS(Longrid) 
  
  
  ;; Make block number grid
  
  ;; Spacing for block numbering
  D_Lon = ABS(MAX(Longrid)-MIN(Longrid))/NX_Blocks
  D_Lat = ABS(MAX(Latgrid)-MIN(Latgrid))/NY_Blocks
  
  Bgrid = INTARR(N_ELEMENTS(Longrid))
  
  FOR i = 0, N_ELEMENTS(Longrid)-1 DO BEGIN
     Row = (FLOOR((Latgrid[i]-MIN(Latgrid))/D_Lat)) <  (NY_Blocks-1)
     Col = (FLOOR((Longrid[i]-MIN(Longrid))/D_Lon)) <  (NX_Blocks-1)
     Bgrid[i] = Col + Row*NX_Blocks + Block_Zero
  ENDFOR
  
  PRINT, "Avg points per block: ", N_ELEMENTS(Longrid)/(NX_Blocks*NY_Blocks) 
  
  
  IF PLOT_Stereo THEN BEGIN
  
     ;; Replot new grid on a sphere
     
     b_min = b_min-2
     b_max = b_max+2
     l_min = l_min-2
     l_max = l_max+2
     scale = 0.6
     
     !P.position = [.1, .1, .9, .9]
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
  
  
  ;; For pointings.dat, need to sort by block number
  
  longrid = longrid[SORT(bgrid)]
  latgrid = latgrid[SORT(bgrid)]
  bgrid = bgrid[SORT(bgrid)]
  
  ;; Print out coords and block numbers to a pointings.dat file
  
  OPENW, unit, filename, /GET_LUN
  FOR i = 0, N_ELEMENTS(bgrid)-1 DO  $
          PRINTF, unit, longrid[i], ' ', latgrid[i], '  ', bgrid[i], $
                  FORMAT = '(F6.2,A,F6.2,A,I4)'
  
  CLOSE, unit
  FREE_LUN, unit

END
