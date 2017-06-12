FUNCTION Greg_SpArith, Map_A, Map_B, add = add, subtract = subtract, $
                        multiply = multiply, divide = divide, average = average, $
                        total = total, short_vel = short_vel
  
  ;; Short_Vel keyword added for velocity cutting
  
  ;; This version does the full blown interpolation over the
  ;; overlapping data points
  
  IF keyword_set(add) THEN op = 1 ELSE $
     IF keyword_set(subtract) THEN op = 2 ELSE $
     IF keyword_set(multiply) THEN op = 3 ELSE $
     IF keyword_set(divide) THEN op = 4 ELSE $
     IF keyword_set(average) THEN op = 5 ELSE $
     IF keyword_set(total) THEN op = 6 ELSE BEGIN
     print, 'Please specify at least one operation: '
     print, '  /add, /subtract, /multiply, /divide, /average, /total'
     return, 0
  ENDELSE  
  
  ;; Constant operations (i.e. Map_B is a scalar number)
  
  IF N_Params() GT 1 AND Size(Map_B, /type) NE 8 THEN BEGIN
     Out = Map_A
     CASE op OF 
        1: out.data = out.data + mapb
        2: out.data = out.data - mapb
        3: out.data = out.data * mapb
        4: out.data = out.data / mapb
     ENDCASE 
     RETURN, out    
  ENDIF
  
  
  
  ;; Note: ops 1 - 4 only work if N_elements(Map_A)=N_elements(Map_B)
  ;;       ops 5 - 6 only work for one map
  
  
  IF op LE 4 THEN BEGIN
     ;; Go through each point in the Map and perform the operations
     
     ;; Each pointing has to have the same number of data points in
     ;; the vel. vector, but this changes based on the overlap!
     ;;  Very annoying
     
     ;; Just do the N_ELEMENTS(Map_A)=N_ELEMENTS(Map_B)=1 case for now
     
     IF N_ELEMENTS(Map_A) EQ 1 AND N_ELEMENTS(Map_B) EQ 1 THEN BEGIN
        
        ;IF N_ELEMENTS(Map_A[0].Vel) NE N_ELEMENTS(Map_B[0].Vel) THEN BEGIN
        ;   MESSAGE,  'Mismatched number OF VELOCITY DATA POINTS', /info
        ;   PRINT, N_ELEMENTS(Map_A[0].Vel), N_ELEMENTS(Map_B[0].Vel)
        ;   RETURN, 0
        ;ENDIF
        
        ;; Determine overlapping velocity ranges in the two vectors
        N_Vel_A = N_ELEMENTS(Map_A[0].Vel) 
        N_Vel_B = N_ELEMENTS(Map_B[0].Vel) 
        
        Delta_V = Map_A[0].Vel[N_Vel_A-1]-Map_A[0].Vel[N_Vel_A-2]
        Max_Vel = MIN([Map_A[0].Vel[N_Vel_A-1], Map_B[0].Vel[N_Vel_B-1]])  ;; minimum of highest vel point
        Min_Vel = MAX([Map_A[0].Vel[0], Map_B[0].Vel[0]])
        
        ;; Shorten the velocity range for WHAM data
        IF KEYWORD_SET(Short_Vel) THEN $
           Min_Vel = MAX([Map_A[0].Vel[N_Vel_A-1], Map_B[0].Vel[N_Vel_B-1]])-200.0
        
        ;; Construct a velocity vector (with uniform spacing, based on given
        ;;                              delta V)
        N = (Max_Vel - Min_Vel)/Delta_V
        Vel_Vector = FLTARR(N)
        FOR i = 0, N-1 DO Vel_Vector[i] = Min_Vel + i*Delta_V
        
        ;; Interpolate every spectrum onto Vel_Vector
        Interp_Data = FLTARR(2, N) 
        Interp_Var = FLTARR(2, N) 
        Interp_Data[0, *] = INTERPOL(Map_A[0].Data, Map_A[0].Vel, Vel_Vector)
        Interp_Var[0, *] = (INTERPOL(SQRT(Map_A[0].Var), Map_A[0].Vel, Vel_Vector))^2
        Interp_Data[1, *] = INTERPOL(Map_B[0].Data, Map_B[0].Vel, Vel_Vector)
        Interp_Var[1, *] = (INTERPOL(SQRT(Map_B[0].Var), Map_B[0].Vel, Vel_Vector))^2

        ;; Define output structure
        out = { $
                 name: string(''), $
                 vel: Vel_Vector, $
                 data: fltarr(N), $
                 var: fltarr(N) $
              }
        FOR i = 0, N-1 DO BEGIN
           IF op EQ 1 THEN BEGIN
              Out.Data[i] = TOTAL(Interp_Data[*, i])
              Out.Var[i] = TOTAL(Interp_Var[*, i])
           ENDIF
           IF op EQ 2 THEN BEGIN
              Out.Data[i] = Interp_Data[0, i]-Interp_Data[1, i]
              Out.Var[i] = Interp_Var[0, i]+Interp_Var[1, i]
           ENDIF
           IF op EQ 3 THEN BEGIN
              Out.Data[i] = Interp_Data[0, i] * Interp_Data[1, i]
              Out.Var[i] = Interp_Data[0, i]^2*Interp_Var[1, i] + Interp_Data[1, i]^2*Interp_Var[0, i]
           ENDIF
           IF op EQ 4 THEN BEGIN
              Out.Data[i] = Interp_Data[0, i] / Interp_Data[1, i]
              Out.Var[i] = Inter_Var[0, i] / (Interp_Data[1, i]^2) + $
                 (Interp_Data[0, i]/Interp_Data[1, i]^2)^2 * Interp_Var[1, i]
           ENDIF
        ENDFOR
     ENDIF ELSE PRINT, 'Greg_SParith Error!'
     RETURN, Out
  ENDIF

  IF op EQ 5 OR op EQ 6 THEN BEGIN   

     ;; Determine overlapping velocity ranges based on all data in Map_A
     N_Vel_A = N_ELEMENTS(Map_A[0].Vel) 
     
     Delta_V_A = Map_A[0].Vel[N_Vel_A-1]-Map_A[0].Vel[N_Vel_A-2]
     Max_Vel_A = MIN(Map_A.Vel[N_Vel_A-1])  ;; minimum of highest vel point in A
     Min_Vel_A = MAX(Map_A.Vel[0])
     
     ;; Shorten the velocity range for WHAM data
     IF KEYWORD_SET(Short_Vel) THEN Min_Vel_A = MAX(Map_A.Vel[N_Vel_A-1])-200.0
     
     ;; Construct a velocity vector (with uniform spacing, based on given
     ;;                              delta V)
     N_A = (Max_Vel_A - Min_Vel_A)/Delta_V_A
     Vel_Vector_A = FLTARR(N_A)
     FOR i = 0, N_A-1 DO Vel_Vector_A[i] = Min_Vel_A + i*Delta_V_A
     
     ;; Interpolate every spectrum onto Vel_Vector_A
     Interp_Data = FLTARR(N_ELEMENTS(Map_A), N_A) 
     Interp_Var = FLTARR(N_ELEMENTS(Map_A), N_A) 
     FOR i = 0, N_ELEMENTS(Map_A)-1 DO BEGIN
        Interp_Data[i, *] = INTERPOL(Map_A[i].Data, Map_A[i].Vel, Vel_Vector_A)
        Interp_Var[i, *] = (INTERPOL(SQRT(Map_A[i].Var), Map_A[i].Vel, Vel_Vector_A))^2
     ENDFOR
     ;; Define output structure
     out = { $
              name: string(''), $
              vel: Vel_Vector_A, $
              data: fltarr(N_A), $
              var: fltarr(N_A) $
           }
     FOR i = 0, N_A-1 DO BEGIN
        Out.Data[i] = TOTAL(Interp_Data[*, i])
        Out.Var[i] = TOTAL(Interp_Var[*, i])
     ENDFOR
     IF op EQ 5 THEN BEGIN
        Out.Data =  Out.Data / N_ELEMENTS(Map_A) 
        Out.Var = Out.Var / N_ELEMENTS(Map_A) ^2
     ENDIF
     RETURN, Out
  END
       
END
