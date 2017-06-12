
@kplot

FUNCTION Get_Phi, X_i, Y_i, X_cent, Y_cent
   
   R = sqrt((Y_i-Y_cent)^2+(X_i-X_cent)^2)
   IF (Y_i LT Y_cent) THEN Phi = ASIN((X_cent-X_i)/r) + !Pi
   IF (Y_i GE Y_cent) THEN $
    Phi = ASIN((X_i-X_cent)/r) + 2.0*!Pi*(X_i LT X_cent)

   RETURN, Phi
   
END

PRO Find_Center
   
   COMMON Find_Center_Data, Image, Header, X_init, Y_init, $
    X_cent, Y_cent,  Mag,  X_Final, Y_Final
   
   COMMON Guess_Center_Data, Guess_Center_Click_Num, Main_Guess_Widget, $
    Guess_Center_X, Guess_Center_Y,  Guess_Window_Alive
   
   COMMON Initial_XY_Data, Main_Initial_XY_Widget, $
    Initial_XY_X, Initial_XY_Y,  Initial_XY_Window_Alive
   
   COMMON Widget_Data,  Width_Widget, X_Cent_Widget, Y_Cent_Widget, $
    X_Init_Widget, Y_Init_Widget,  X_Final_Widget, Y_Final_Widget, Path, Winnum
   
   X_init = 0
   Y_init = 0
   X_cent = 0
   Y_cent = 0
   
   Path =  '~/'
   
   Guess_Window_Alive = 0
   Initial_XY_Window_Alive = 0
   
   Guess_Center_Click_Num = 0
   
   Font =  '-adobe-helvetica-medium-r-normal--18-180-75-75-p-98-iso8859-1'
   
   
   Main_Widget = Widget_Base(title='Find Center - Main Window', column=1)
   
   Button_Row_Widget = Widget_Base(Main_Widget, column=1)
   
   File_Selection_Widget = Widget_Button(Button_Row_Widget, $
                                         value='Choose a Fits File', $
                                         uvalue='choose_file',  $
                                        Font=font)
   
   Guess_Center = Widget_Button(Button_Row_Widget, value='Guess the Center', $
                               uvalue='guess_center',  Font=font)
   
   Initial_Guess = Widget_Button(Button_Row_Widget, $
                         value='Set Starting Position on the Ring', $
                                 uvalue='initial_xy', Font=font)
   
   Find_Center_Button = Widget_Button(Button_Row_Widget, $
                               value='Find the Center', $
                               uvalue='find_center', Font=font)
   
   Quit = Widget_Button(Button_Row_Widget, value='Quit', uvalue='quit', $
                       Font=font)
   
   Info_Widget = Widget_Base(title='Find Center - Info Window', column=1,  $
                            Group_Leader=Main_Widget)
   
   Row_1_Widget = Widget_Base(Info_Widget, row=1)
   
   Width_Widget = Cw_Field(Row_1_Widget, /Integer,  $
                           Title='Width of Search Box: ', Value=1,  $
                           XSize=4)
   
   Row_2_Widget = Widget_Base(Info_Widget, row=1)
   
   Label = Widget_Label(Row_2_Widget, Value='Position of the Guessed Center  ')
   
   X_Cent_Widget = Cw_Field(Row_2_Widget, /Floating,  $
                            Title=' X: ', value=X_cent,  $
                            XSize=7)
   
   Y_Cent_Widget = Cw_Field(Row_2_Widget, /Floating,  $
                            Title=' Y: ', value=Y_cent, $
                            XSize=7)
   
   Row_3_Widget = Widget_Base(Info_Widget, row=1)
   
   Label = Widget_Label(Row_3_Widget, Value='Starting Position on the Ring   ')
   
   X_init_Widget = Cw_Field(Row_3_Widget, /Integer,  $
                            Title=' X: ', value=X_init,  $
                            XSize=7)
   
   Y_init_Widget = Cw_Field(Row_3_Widget, /Integer,  $
                            Title=' Y: ', value=Y_init,  $
                            XSize=7)
   
   Row_4_Widget = Widget_Base(Info_Widget, row=1)
   
   Label = Widget_Label(Row_4_Widget, Value='Computed Center is at           ')
   
   X_Final_Widget = Cw_Field(Row_4_Widget, /Floating,  $
                            Title=' X: ', value=X_init,  $
                            XSize=7)
   
   Y_Final_Widget = Cw_Field(Row_4_Widget, /Floating,  $
                            Title=' Y: ', value=Y_init,  $
                            XSize=7)
   
   Widget_Control, /Realize,  Main_Widget
   Widget_Control, /Realize, Info_Widget
   XManager, 'Find_Center', Main_Widget
   
END


PRO Find_Center_Event, event
  
  COMMON Widget_Data
  
   Widget_Control,  event.id,  Get_Uvalue=Uvalue
   
   CASE Uvalue OF
      
      'choose_file': BEGIN
         file_name = DIALOG_PICKFILE(Display_Name='Pick a File', $
                                     Path=Path, $
                                     Filter='*.fts',  Group=event.id, get_path =  path)

         DISPLAY_IMAGE, file_name,  event.top
      END
      
      'guess_center': BEGIN
         Guess_Center, event, event.top
      END
      
      'initial_xy': BEGIN
         Initial_XY, event, event.top
      END
      
      'find_center': BEGIN
         Widget_Control, event.id,  /HourGlass
         Find_Center_pro
      END
      
      'quit': Widget_Control, event.top, /Destroy
      
   ENDCASE
   
END

PRO Display_Image, file_name,  Parent_Widget
   
   COMMON Find_Center_Data, Image, Header, X_init, Y_init, $
    X_cent, Y_cent, Mag,  X_Final, Y_Final
   
   COMMON Widget_Data
   
   Image = ReadFits(file_name, Header)
   
   IF FXPAR(Header, 'PARBIN') EQ 4 THEN BEGIN
      Image = Image - 605.66 
      Mag = 4.0
   ENDIF ELSE BEGIN
      Image =  Image - 822.43
      Mag = 1.0
   ENDELSE
   ;Mag = Floor(1200/FxPar(Header, 'NAXIS1'))
   
   Main_Widget = Widget_Base(title='Find Center - Image', column=1, $
                            Group_Leader=Parent_Widget)
   
   Image_Widget = Widget_Draw(Main_Widget, Retain=2, $
                              /Button_Events, uvalue='image', $
                              XSIZE=Mag*Fxpar(Header, 'NAXIS1'), $
                              YSIZE=Mag*Fxpar(Header, 'NAXIS2'), $
                              Event_Pro='Image_Event')
   
   Widget_Control, Main_Widget, /Realize
   
   TVSCL, SIGRANGE(REBIN(Image, Mag*FxPar(Header, 'NAXIS1'), $
                      Mag*FxPar(Header, 'NAXIS2')))
   Winnum = !D.window
   
   ;; Plot up the cross hairs; geometric center of image
   
   PLOTS, FINDGEN(Mag*Fxpar(Header, 'NAXIS1')), $
    REPLICATE(Mag*Fxpar(Header, 'NAXIS2')/2.0,Mag*Fxpar(Header, 'NAXIS1')), $
    COLOR=12000,  /DEVICE
   PLOTS, REPLICATE(Mag*Fxpar(Header, 'NAXIS1')/2.0,$
                    Mag*Fxpar(Header, 'NAXIS2')), $
    FINDGEN(Mag*Fxpar(Header, 'NAXIS2')), $
    COLOR=12000,  /DEVICE
   
      
END
   
PRO Image_Event, event
   
   COMMON Guess_Center_Data, Guess_Center_Click_Num, Main_Guess_Widget, $
    Guess_Center_X, Guess_Center_Y,  Guess_Window_Alive
   
   COMMON Initial_XY_Data, Main_Initial_XY_Widget, $
    Initial_XY_X, Initial_XY_Y,  Initial_XY_Window_Alive

   IF event.type EQ 1 THEN BEGIN
      Guess_Center, event
      Guess_Center_Click_Num = Guess_Center_Click_Num + 1
      Initial_XY, event
   ENDIF
   
END


PRO Guess_Center, event, Parent_Widget
   
   COMMON Find_Center_Data, Image, Header, X_init, Y_init, $
    X_cent, Y_cent,  Mag,  X_Final, Y_Final
   
   COMMON Guess_Center_Data, Guess_Center_Click_Num, Main_Guess_Widget, $
    Guess_Center_X, Guess_Center_Y,  Guess_Window_Alive
   
   COMMON Widget_Data,  Width_Widget, X_Cent_Widget, Y_Cent_Widget, $
    X_Init_Widget, Y_Init_Widget,  X_Final_Widget, Y_Final_Widget
   
   IF KEYWORD_SET(Parent_Widget) THEN BEGIN
      
      Guess_Center_X = FLTARR(4)
      Guess_Center_Y = FLTARR(4)
      
      Guess_Center_Click_Num = 0
      
      Main_Guess_Widget = Widget_Base(title='Click on the Image',  $
                                     Group_Leader=Parent_Widget)
      
      Guess_Widget = Widget_Label(Main_Guess_Widget, $ 
                                  Group_Leader=Main_Guess_Widget, $
                                  Value='Click on the four corners of the ring to be fit. Start at "North" and go clockwise.',  $
                                  Font =  '-adobe-helvetica-medium-r-normal--18-180-75-75-p-98-iso8859-1')
      
      Widget_Control, Main_Guess_Widget, /Realize
      
      Guess_Window_Alive = 1
      
   ENDIF ELSE BEGIN
      
      IF Guess_Window_Alive EQ 1 THEN BEGIN
         Guess_Center_X(Guess_Center_Click_Num) = event.x / Mag
         Guess_Center_Y(Guess_Center_Click_Num) = event.y / Mag
         
         IF (Guess_Center_Click_Num EQ 3) THEN BEGIN
            
            Widget_Control, Main_Guess_Widget, /Destroy 
            Guess_Window_Alive = 0
            
            TVSCL, SIGRANGE(REBIN(Image, Mag*FxPar(Header, 'NAXIS1'), $
                      Mag*FxPar(Header, 'NAXIS2')))
            
            PLOTS, FINDGEN(Mag*Fxpar(Header, 'NAXIS1')), $
             REPLICATE(Mag*Fxpar(Header, 'NAXIS2')/2.0,$
                       Mag*Fxpar(Header, 'NAXIS1')), $
             COLOR=12000,  /DEVICE
            PLOTS, REPLICATE(Mag*Fxpar(Header, 'NAXIS1')/2.0,$
                             Mag*Fxpar(Header, 'NAXIS2')), $
             FINDGEN(Mag*Fxpar(Header, 'NAXIS2')), $
             COLOR=12000,  /DEVICE

            IF X_init NE 0 THEN BEGIN 
               PLOTS, Mag*[X_init, Y_init], $
                PSYM=6, SYMSIZE=2.0, COLOR=600000, /DEVICE
            ENDIF
            
            FOR i=0, 3 DO BEGIN
               PLOTS, Mag*[Guess_Center_X(i), Guess_Center_Y(i)], $
                PSYM=4, SYMSIZE=2.0, COLOR=120000, /DEVICE
            ENDFOR
            
            
            IF (Guess_Center_X(2) EQ Guess_Center_X(0)) THEN BEGIN
               X_cent = Guess_Center_X(2)
               m_2 = (Guess_Center_Y(3)-Guess_Center_Y(1))/ $
                (Guess_Center_X(3)-Guess_Center_X(1))
               Y_cent = (X_cent-Guess_Center_X(1)) * m_2 + Guess_Center_Y(1)
            ENDIF ELSE BEGIN
               m_1 = (Guess_Center_Y(2)-Guess_Center_Y(0))/ $
                (Guess_Center_X(2)-Guess_Center_X(0))
               
               m_2 = (Guess_Center_Y(3)-Guess_Center_Y(1))/ $
                (Guess_Center_X(3)-Guess_Center_X(1))
               
               X_cent = (m_1*Guess_Center_X(0) - m_2*Guess_Center_X(1) + $
                    Guess_Center_Y(1)-Guess_Center_Y(0)) / $
                (m_1-m_2)
               
               Y_cent = (X_cent-Guess_Center_X(1)) * m_2 + Guess_Center_Y(1)
            ENDELSE
            
            Widget_Control, X_Cent_Widget, Set_Value=X_cent
            Widget_Control, Y_Cent_Widget, Set_Value=Y_cent
            
            PLOTS, Mag*[X_cent, Y_cent], PSYM=4, SYMSIZE=2.0, $
             COLOR=500000, /DEVICE
            
         ENDIF
         
      ENDIF
      
   ENDELSE
   
END

PRO Initial_XY, event, Parent_Widget
   
   COMMON Find_Center_Data, Image, Header, X_init, Y_init, $
    X_cent, Y_cent, Mag,  X_Final, Y_Final
   
   COMMON Initial_XY_Data, Main_Initial_XY_Widget, $
    Initial_XY_X, Initial_XY_Y,  Initial_XY_Window_Alive
   
   COMMON Widget_Data,  Width_Widget, X_Cent_Widget, Y_Cent_Widget, $
    X_Init_Widget, Y_Init_Widget,  X_Final_Widget, Y_Final_Widget
   
   IF KEYWORD_SET(Parent_Widget) THEN BEGIN
      
      Initial_XY_X = 0
      Initial_XY_Y = 0
      
      Main_Initial_XY_Widget = Widget_Base(title='Click on the Image',  $
                                     Group_Leader=Parent_Widget)
      
      Initial_XY_Widget = Widget_Label(Main_Initial_XY_Widget, $
                                 Group_Leader=Main_Initial_XY_Widget, $
                                       Value='Click on the Starting Position on the Ring',  $
                                       Font =  '-adobe-helvetica-medium-r-normal--18-180-75-75-p-98-iso8859-1')
      
      Widget_Control, Main_Initial_XY_Widget, /Realize
      
      Initial_XY_Window_Alive = 1
      
   ENDIF ELSE BEGIN
      
      IF Initial_XY_Window_Alive EQ 1 THEN BEGIN
         
         Initial_XY_X = event.x / Mag
         Initial_XY_Y = event.y / Mag
         
         Widget_Control, Main_Initial_XY_Widget, /Destroy 
         Initial_XY_Window_Alive = 0
         
         TVSCL, SIGRANGE(REBIN(Image, Mag*FxPar(Header, 'NAXIS1'), $
                               Mag*FxPar(Header, 'NAXIS2')))
         PLOTS, FINDGEN(Mag*Fxpar(Header, 'NAXIS1')), $
          REPLICATE(Mag*Fxpar(Header, 'NAXIS2')/2.0,$
                    Mag*Fxpar(Header, 'NAXIS1')), $
          COLOR=12000,  /DEVICE
         PLOTS, REPLICATE(Mag*Fxpar(Header, 'NAXIS1')/2.0,$
                          Mag*Fxpar(Header, 'NAXIS2')), $
          FINDGEN(Mag*Fxpar(Header, 'NAXIS2')), $
          COLOR=12000,  /DEVICE

         IF X_cent NE 0 THEN BEGIN 
            PLOTS, Mag*[X_cent, Y_cent], PSYM=4, SYMSIZE=2.0, $
             COLOR=500000, /DEVICE
         ENDIF
         
         PLOTS, Mag*[Initial_XY_X, Initial_XY_Y], $
          PSYM=6, SYMSIZE=2.0, COLOR=600000, /DEVICE
         
         X_init = Initial_XY_X
         Y_init = Initial_XY_Y
         
         Widget_Control,  X_Init_Widget,  Set_Value=X_Init
         Widget_Control, Y_Init_Widget, Set_Value=Y_Init
         
      ENDIF
      
   ENDELSE
   
END

PRO Find_Center_PRO
   
   COMMON Find_Center_Data
   
   COMMON Widget_Data
   
   i = 0
   
   Widget_Control, Width_Widget, Get_Value=W
   Widget_Control, X_Cent_Widget, Get_Value=X_Cent
   Widget_Control, Y_Cent_Widget, Get_Value=Y_Cent
   Widget_Control, X_Init_Widget, Get_Value=X_Init
   Widget_Control, Y_Init_Widget, Get_Value=Y_Init
   
   X_i = X_init
   Y_i = Y_init
   
   X_circ = INTARR(5000)
   Y_circ = INTARR(5000)
   
   X_cent = FxPar(Header, 'NAXIS1') / 2.0
   Y_cent = FxPar(Header, 'NAXIS2') / 2.0
   
   m = DBLARR(2*W+1, 2*W+1)
   
   Phi_Init = Get_Phi(X_i, Y_i, X_cent, Y_cent)
   
   delta_Phi = !Pi/180
   
   Phi = Phi_init
   
   WHILE( (ABS(Phi-Phi_init) GT delta_Phi) OR (i LT 10) ) DO BEGIN
   
;      PRINT, X_i, X_cent, Y_i, Y_cent
         
      FOR a=0, 2*W DO BEGIN
         FOR b=0, 2*W DO BEGIN
            IF ((X_i+(a-W))-X_cent) EQ 0 THEN BEGIN
               IF Y_i GT Y_cent THEN m(a, b) = FxPar(Header, 'NAXIS1') ELSE $
                m(a, b) = -1.0*FxPar(Header, 'NAXIS1')
            ENDIF ELSE $
             m(a, b) = ((Y_i+(b-W))-Y_cent) / ((X_i+(a-W))-X_cent) 
         ENDFOR
      ENDFOR
      
;      PRINT,  m

      Bad_Coords = WHERE(m GE m(W, W))
      
      IF ABS(X_i-X_cent) LE W THEN BEGIN
         IF Y_i LT Y_cent THEN BEGIN
            IF X_i GE X_cent THEN BEGIN
               Bad_Coords = WHERE( (m GE m(W, W)) AND (m LT 0) )
            ENDIF
            IF X_i LT X_cent THEN BEGIN
               Bad_Coords = WHERE( (m GE m(W, W)) OR  (m LT 0) )
            ENDIF
         ENDIF
         IF Y_i GT Y_cent THEN BEGIN
            IF X_i LT X_cent THEN BEGIN
               Bad_Coords = WHERE( (m GE m(W, W)) AND (m LT 0) )
            ENDIF
            IF X_i GE X_cent THEN BEGIN
               Bad_Coords = WHERE( (m GE m(W, W)) OR  (m LT 0) )
            ENDIF
            
         ENDIF
         
      ENDIF
      
;      PRINT,  'Bad_Coords = ',  Bad_Coords,  m(1, 1)
      
      temp_Image = Image(X_i-W:X_i+W, Y_i-W:Y_i+W)
;      PRINT, temp_Image
      temp_Image(Bad_Coords) = 0.0
;      PRINT
;      PRINT, temp_Image
;      PRINT
;      PRINT
      
      
      Next_Coord = WHERE(temp_Image EQ MAX(temp_Image))
      
      IF N_ELEMENTS(Next_Coord) GT 1 THEN BEGIN
         Next_Coord = Next_Coord(0)
;         PRINT,  'Multiple Match Found'
      ENDIF
      
      ;; Find next x_i,y_i
      
      Next_X = Next_Coord MOD (2*W+1)
      Next_Y = (Next_Coord-Next_X) / (2*W+1)
      
      X_i = Next_X(0) - W + X_i 
      Y_i = Next_Y(0) - W + Y_i
      
      ;; Find how far of an angle we have made around the ring
      
      Phi = Get_Phi(X_i, Y_i, X_cent, Y_cent)
      
;      PRINT,  'Next phi=', Phi*180. / !pi, delta_Phi*180/!Pi, $
;       ABS(Phi-Phi_init)*180/!pi
      
      ;; Update the Image on the screen
      
      PLOTS, Mag*[X_i, Y_i], PSYM=6, SYMSIZE=0.5, /device, color=600000
      
      X_circ(i) = X_i
      Y_circ(i) = Y_i
      
      i = i + 1
      
   ENDWHILE
   
   N_pts = i-1
   
   X_circ = X_circ(0:N_pts-1)
   Y_circ = Y_circ(0:N_pts-1)
   
   WINDOW, 2
   WSET, 2
   KPLOT, FINDGEN(N_Pts), Image[X_Circ, Y_Circ], psym = -2
   Mom = MOMENT(Image[X_Circ, Y_Circ])
   Str = 'Mean = ' +STRTRIM(Mom[0], 2) + ' Sig = ' +STRTRIM(SQRT(Mom[1]), 2)
   XYOUTS, 0.5, 0.2, Str, /normal, charsize = 1.5
   
   WSET, Winnum
   
   delta_R = [.2, .01, .001]
   N_r = [25, 20, 10]
   
   X_Final = TOTAL(X_circ)/N_pts
   Y_Final = TOTAL(Y_circ)/N_pts
   
   FOR k=0, N_ELEMENTS(delta_R)-1 DO BEGIN
      
;      PRINT,  "We started out at ",  X_Final, Y_final
      
      R = FLTARR(N_r(k)*2., N_r(k)*2., N_pts)
      
;      Stats = DBLARR((2.*N_r(k))^2, 4)
      Stats = DBLARR(2.*N_r(k), 2.*N_r(k), 4)

      FOR i=0, 2*N_r(k)-1 DO BEGIN
         FOR j=0, 2*N_r(k)-1 DO BEGIN
            R(i, j, *) = SQRT((X_circ-(X_Final + $
                                       (delta_R(k)*(i-N_r(k)))))^2 + $
                              (Y_circ-(Y_Final + (delta_R(k)*(j-N_r(k)))))^2)
            Stats(i, j, *) = MOMENT(R(i, j, *),  /DOUBLE)
         ENDFOR
      ENDFOR
;      PRINT,  'Done'
      
      Z = WHERE(Stats(*, *, 1) EQ MIN(Stats(*, *, 1)))
      Z =  FLOAT(Z(0))
;      PRINT,  "Z = ",  Z
      X_temp = Z MOD (2*N_r(k))
      Y_temp = (Z-X_temp) / (2.*N_r(k))
      
      PRINT, "Shift = ", ABS(X_temp-N_r(k)), ABS(Y_temp-N_r(k)), $
       FORMAT='(A10,2I)'
      X_Final = (X_temp - N_r(k))*delta_R(k) + X_Final
      Y_Final = (Y_temp - N_r(k))*delta_R(k) + Y_Final

      PRINT,  "R = ",  Stats(X_temp, Y_temp, 0),  $
       "  Sigma = ",  SQRT(Stats(X_temp, Y_temp, 1)),  $
       "X = ",  X_final,  "Y = ", Y_final,  $
       FORMAT='(A6,F15.7,A15,F15.10,A6,F10.3,A6,F10.3)'
      
   ENDFOR
   
   Widget_Control, X_Final_Widget, Set_Value=X_Final
   Widget_Control,  Y_Final_Widget, Set_Value=Y_Final
   
   Mom_final = MOMENT( Sqrt( (X_circ-X_Final)^2+(Y_circ-Y_Final)^2) )
   Sig_Final = Sqrt(Mom_final(1))
   
   PRINT,  'Final X,Y = ',  X_Final, Y_Final, $
    ABS(X_Final-X_cent), ABS(Y_Final-Y_cent), W, Sig_Final, $
    FORMAT='(A13,2F10.4,2F7.4,I,F10.7)'
   
   
   PRINT,  " Dumb Guess = ",  TOTAL(X_circ)/N_pts, TOTAL(Y_circ)/N_pts
   
   PRINT,  "Off by: ",  ABS(TOTAL(X_circ)/N_pts-X_final), $
    ABS(TOTAL(Y_circ)/N_pts-Y_final)
   
   
;; Output the data   
;   OPENW, unit, 'findcenter_'+STRTRIM(STRING(X_init), 2) +'.txt', /Get_Lun
;   FOR i=0, N_pts-1 DO PRINTF, unit, X_circ(i), Y_circ(i)
;   CLOSE, unit
;   FREE_LUN, unit
   
END
