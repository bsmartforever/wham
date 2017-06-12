FUNCTION Get_Phi, X_i, Y_i, X_cent, Y_cent
   
   R = sqrt((Y_i-Y_cent)^2+(X_i-X_cent)^2)
   IF (Y_i LT Y_cent) THEN Phi = ASIN((X_cent-X_i)/r) + !Pi
   IF (Y_i GE Y_cent) THEN $
    Phi = ASIN((X_i-X_cent)/r) + 2.0*!Pi*(X_i LT X_cent)

   RETURN, Phi
   
END

PRO findCenter, images, params = params, verbose = verbose
  
  ON_ERROR, 2
  
  IF KEYWORD_SET(verbose) THEN PRINT,  '**** FIND CENTER ****'
  
  ;; Geometric center of image, used as fiducial for moving around ring
  X_cent = ROUND(N_ELEMENTS(Images.Clean[*, 0]) / 2.0 )
  Y_cent = ROUND(N_ELEMENTS(Images.Clean[0, *]) / 2.0 )
  
  ;; Width of search box while moving around ring
  W = 1                  
  
  ;; Starting point on ring
  
  ;; Simple starting point, find a max value along horiz line through
  ;; geometric center
  ;; TO DO: More sophisticated method for initial value?  Don't want
  ;; to start on top of a cosmic ray.  
  Nx = N_ELEMENTS(Images.Clean[*, 0]) 
  Ny = N_ELEMENTS(Images.Clean[0, *]) 
  Y = Y_cent
  temp = Images.Clean[X_cent:Nx-1, Y_cent]
  X = WHERE(temp EQ MAX(temp)) + X_cent
  
  IF KEYWORD_SET(verbose) THEN PRINT,  'Starting point is ', X, Y, format = '(A,2I6)'
  
  ;; Initial value of radius, stored to test if we wander too much later
  R_init =  SQRT( (X-X_cent)^2 + (Y-Y_cent)^2 )
  
  ;; WARNING: Starting point has to be at least W pixels away from
  ;;  initial guess at center
  IF R_init LE W THEN MESSAGE,  'Starting point too close to center'
    
  ;; Allocate array of x/y pixel indices that trace out a ring. 
  ;; Max circumference is  2*pi*rmax, but could wander around that so
  ;; add in factor of 4 to be safe
  
  X_circ = INTARR(4*ROUND(2*!pi*Params.rmax))
  Y_circ = INTARR(4*ROUND(2*!pi*Params.rmax))
    
  ;; Allocate search box round staring point; values will be populated with angle
  m = DBLARR(2*W+1, 2*W+1)
  
  ;; Angle between geometric centre and starting point, angle is to
  ;; centre of pixels
  Phi_Init = Get_Phi(X+.5, Y+.5, X_cent+.5, Y_cent+.5)
  
  Phi = Phi_init
  
  i = 0  ;; counting index around the ring
  
  ;; Loop all the way around the ring, go until we've gotten within 1
  ;; deg of original angle.   Give it at least 10 steps before
  ;; comparing angles to make sure we really get started.
  
  WHILE( ABS(Phi-Phi_init) GT !Pi/180. OR (i LT 10) ) DO BEGIN
     
     ;; Populate m box with angle between X,Y and center
     
     FOR a = 0, 2*W DO BEGIN
        FOR b = 0, 2*W DO BEGIN
           m[a, b] = Get_Phi(X+a-W+.5, Y+b-W+.5, X_cent+.5, Y_cent+.5)
        ENDFOR
     ENDFOR
     ;; Need to worry about going through phi=0 as angle flips from
     ;; 359 to 0
     IF STDDEV(m) GE 0.5 THEN m[WHERE(m LE !Pi)] = m[WHERE(m LE !Pi)] + 2*!pi
     
     ;; Print the m box
     ;;FOR j = 0, 2*W DO PRINT, m[j, 0:2*W]*!radeg, format = '(3F7.2)'

     ;; Subtract off central pixel angle
     m = m - m[W, W]
     
     ;; Make little box image, zero out the pixels where local phi is not positive
     IF (X-W) LT 0 OR (X+W) GT (Nx-1) OR (Y-W) LT 0 OR (Y+W) GT (Nx-1) THEN $
        MESSAGE, 'Search box went out of bounds of image'
     temp_Image = Images.Clean[X-W:X+W, Y-W:Y+W]
     temp_Image[WHERE(m LE 0.0)] = 0.0
     
     Next_Coord = WHERE(temp_Image EQ MAX(temp_Image))
     IF N_ELEMENTS(Next_Coord) GT 1 THEN BEGIN
        IF KEYWORD_SET(verbose) THEN PRINT,  'WARNING: Identical pixel values in search box'
        Next_Coord = Next_Coord[0]  ;; just pick one
     ENDIF
     
     ;; Find next  X,Y
     Next_X = Next_Coord MOD (2*W+1)
     Next_Y = (Next_Coord-Next_X) / (2*W+1)
     
     X = Next_X[0] - W + X
     Y = Next_Y[0] - W + Y
     
     ;; Put them into storage array
     X_circ[i] = X
     Y_circ[i] = Y
     
     ;; Test to see if we've wandered too far
     ;; This could happen for faint spectra, cosmic rays, or bad
     ;; starting point
     IF ABS(SQRT( (X - X_cent)^2 + (Y-Y_cent)^2 ) - R_init) GT 10 THEN $
        MESSAGE, 'Ring trace has wandered too far'
     
     ;; Find how far of an angle we have made around the ring,  turn
     ;; over indexing variable
     Phi = Get_Phi(X+.5, Y+.5, X_cent+.5, Y_cent+.5)
     i = i + 1
     
  ENDWHILE
  
  ;; Reduce the X_circ, Y_circ arrays
  X_circ = X_circ[0:i-2]
  Y_circ = Y_circ[0:i-2]
  N_pts = N_ELEMENTS(X_circ) 

  ;; Create 1-D array of ring values
  Ring_Val = FLTARR(N_pts)
  FOR i = 0, N_pts-1 DO Ring_Val[i] = Images.Clean[X_Circ[i], Y_Circ[i]]
  
  
  ;; Plot the ring intensity as fn of azimuth
  IF KEYWORD_SET(verbose) THEN BEGIN
     KPLOT, FINDGEN(N_pts), Ring_Val, psym = -2
     Mom = MOMENT(Ring_Val)
     Str = 'Mean = ' +STRTRIM(Mom[0], 2) + ' Sig = ' +STRTRIM(SQRT(Mom[1]), 2)
     XYOUTS, 0.5, 0.2, Str, /normal, charsize = 1.5
  ENDIF

  ;; Throw out outliers along the ring, they can skew the ring center
  Box = 5
  j = 0 ;; index of good values
  N_bad = 0  ; counter of bad values
  Good_Ring_Val = FLTARR(N_pts) 
  Good_X_Circ = FLTARR(N_pts) 
  Good_Y_Circ = FLTARR(N_pts) 
  FOR i = 0, N_pts-1 DO BEGIN
     ;; Find running boxcar avg, sigma
     ;; Throwing out 2-sig outliers, 11 pix-wide box seems good
     ;; Need to worry about boundaries, do a wraparound
     IF i-Box LT 0 THEN $
        Mom = MOMENT([Ring_Val[0:i+Box], $
                      Ring_Val[N_pts+i-Box:N_pts-1]]) 
     IF i+Box GT N_pts-1 THEN $
        Mom = MOMENT([Ring_Val[i-Box:N_pts-1], $
                      Ring_Val[0:i+Box-N_pts]])
     IF i-Box GE 0 AND i+Box LE N_pts -1 THEN $
        Mom = MOMENT(Ring_Val[i-Box:i+Box])
     IF ABS(Ring_Val[i]-Mom[0]) LT 2*SQRT(Mom[1]) THEN BEGIN
        Good_Ring_Val[j] = Ring_Val[i]
        Good_X_Circ[j] = X_Circ[i]
        Good_Y_Circ[j] = Y_Circ[i]
        j = j+1
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(verbose) THEN PRINT, 'Ring Outlier:', i, Ring_Val[i]
        N_Bad = N_Bad+1
     ENDELSE
  ENDFOR
  IF KEYWORD_SET(verbose) THEN PRINT, 'Threw out ', N_Bad, ' ring values'
  
  ;; Reset arrays
  Ring_Val = Good_Ring_Val[0:j-1]
  X_Circ = Good_X_Circ[0:j-1]
  Y_Circ = Good_Y_Circ[0:j-1]
  
  ;; Now find the center
  ;; Idea here is to loop through and minimize the deviation of the
  ;; X,Y_circ points from a circle
  
  ;; Average value of  X_circ, Y_circ as a starting point
  X_Final = AVG(X_circ)
  Y_Final = AVG(Y_circ)
  N_pts = N_ELEMENTS(X_circ) 
  
  ;;  Test centers go from +/- delta_R*N_r, step size of delta_R
  ;;                           (pixel units)
  ;; e.g. first pass:  +/- 5 pix, step=.2
  ;;         2nd pass: +/- .2 pix, step=.01
  ;;         3rd pass: +/-  .01 pix, step=0.001
  
  N_r = [25, 20, 10]
  delta_R = [.2, .01, .001]
  
  
  FOR k = 0, N_ELEMENTS(delta_R)-1 DO BEGIN
     
     R = FLTARR(N_r[k]*2., N_r[k]*2., N_pts)
     
     Stats = DBLARR(2.*N_r[k], 2.*N_r[k], 4)
     
     FOR i = 0, 2*N_r[k]-1 DO BEGIN
        FOR j = 0, 2*N_r[k]-1 DO BEGIN
           ;; Calculate distance from each X,Y_circ point to test center
           R(i, j, *) = SQRT((X_circ-(X_Final + $
                                      (delta_R(k)*(i-N_r(k)))))^2 + $
                             (Y_circ-(Y_Final + (delta_R(k)*(j-N_r(k)))))^2)
           Stats(i, j, *) = MOMENT(R(i, j, *),  /DOUBLE)
        ENDFOR
     ENDFOR
     
     Z = WHERE(Stats(*, *, 1) EQ MIN(Stats(*, *, 1)))
     Z =  FLOAT(Z[0])
     
     ;; Find the x,y that minimizes variance
     X_temp = Z MOD (2*N_r(k))
     Y_temp = (Z-X_temp) / (2.*N_r(k))
     
     X_Final = (X_temp - N_r(k))*delta_R(k) + X_Final
     Y_Final = (Y_temp - N_r(k))*delta_R(k) + Y_Final
     
     IF KEYWORD_SET(verbose) THEN BEGIN
        PRINT, 'Pass Number: ', k+1
        PRINT, "X,Y Shift: ", ABS(X_temp-N_r(k)), ABS(Y_temp-N_r(k)), $
               FORMAT = '(A10,2I4)'
        PRINT,  "R = ",  Stats(X_temp, Y_temp, 0),  $
                "  Sigma = ",  SQRT(Stats(X_temp, Y_temp, 1)),  $
                "X = ",  X_final,  "Y = ", Y_final,  $
                FORMAT = '(A4,F9.5,A9,F9.6,A6,F6.3,A6,F6.3)'
     ENDIF
     
  ENDFOR
  IF KEYWORD_SET(verbose) THEN PRINT, 'Centers from param file: ', $
                                      Params.xc, Params.yc, format = '(A,2F7.3)'
  IF KEYWORD_SET(verbose) THEN PRINT, 'Centers from auto finder: ', $
                                      X_final, Y_final, format = '(A,2F7.3)'
  IF KEYWORD_SET(verbose) THEN PRINT, 'Params center is ', $
                                      SQRT((params.xc-X_final)^2+(Params.yc-Y_final)^2), $
                                      ' pixels away from auto center', $
                                      format = '(A,F7.3,A)'
  IF KEYWORD_SET(verbose) THEN PRINT,  '**** END FIND CENTER ****'
  
  ;; Assign new centers to Params structure, only if we found a good centre
  
  IF ABS(Params.xc - X_Final) GE .25 OR ABS(Params.yc-Y_Final) GE .25 THEN BEGIN
     PRINT, 'WARNING: findCenters are > .25 pix away from provided center, ignoring findCenter'
  ENDIF ELSE BEGIN
     Params.xc = X_final
     Params.yc = Y_final
  ENDELSE
  
END

