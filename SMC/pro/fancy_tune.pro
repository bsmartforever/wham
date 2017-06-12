@kplot
@tune
PRO FANCY_TUNE, Output_Tune = Output_Tune, Filter = Filter, obs=obs
  
  ;; A facny utility to predict the tune of a line of interest
  ;; Most useful for high-velocity stuff
  ;; includes a nifty interactive 'line-dragging' feature
  COMMON Widget, Widget
  COMMON Params, Params

  ;; Get Atm. Template File Names
  SPAWN, 'pwd', pwd
  CD, "/d/wham/lib/whamspect2/atmos/"
  SPAWN, "/bin/ls -1 *.txt", List
  CD, pwd
  Temp = STRARR(N_ELEMENTS(List)+1)
  Temp[1:N_ELEMENTS(Temp)-1] = List
  Temp[0] = 'None'
  List = Temp
  FOR i = 1, N_ELEMENTS(List)-1 DO List[i] = STRMID(List[i], 0, STRLEN(List[i])-4)
  
  Widget = { Lon: 0L, Lat: 0L, Filter:0L, Template:0L, $
             Use_Survey:0L, VLSR:0L, Width:0L, Area:0L, $
             Geo_0:0L, LSR_0:0L, Line_Vel:0L, $
             Predict_Tune:0L, Tune:0L, Quit:0L, $
             Window: 0L, WinNum:0L}
  
  Tune_Ref = FLTARR(9, 2)
  
  IF NOT keyword_set(obs) THEN obs = 'ctio'
  
  IF obs EQ 'kpno' THEN BEGIN
	  Tune_Ref[0, *] = [146.0, 150.0]
	  Tune_Ref[1, *] = [109.1, 160.6] 
	  Tune_Ref[2, *] = [126.6, 80.6]
	  Tune_Ref[3, *] = [105.4, 136.4]
	  Tune_Ref[4, *] = [108.0, 173.4]
	  Tune_Ref[5, *] = [94.7, 107.2]
	  Tune_Ref[6, *] = [131.5, 97.5]
	  Tune_Ref[7, *] = [86.4, 123.4]
	  Tune_Ref[8, *] = [135.4, 198.6]
	  l_arr = [4861.3, 5006.9, 5754.6, 5875.6, $
                      6300.3, 6562.8, 6583.4, 6716.4, 7320.0]
  ENDIF ELSE IF obs EQ 'ctio' THEN BEGIN
  	readcol,'/d/wham/pro/calibration/tunes.txt', $
  	filterlist, l_arr, pa, pb, format='A,F,F,F', silent = silent
    FOR i=0, n_elements(filterlist)-1 DO tune_ref[i,*] = [pa[i], pb[i]]
  ENDIF ELSE message, 'Observatory must be "ctio" or "kpno"!'

  Params = { Lon:0.0, Lat:0.0, JD: 0D, $
             Filter:'', Template:'', $ 
             Lambda: l_arr, Lambda_Index:0, $
             Template_Files: List, Template_Index: 0, $
             Use_Survey:0, VLSR:0.0, Width:25, Area:1.0, $
             Geo_0:50.0, LSR_0:0.0, $
             Tune:FLTARR(2), Tune_Ref: Tune_Ref, $
             X1:0L, X2:0L}
  
  IF NOT(KEYWORD_SET(Filter)) THEN Filter =  'hb'
  
  IF obs EQ 'kpno' THEN BEGIN
	  IF Filter EQ 'hb' THEN Params.Lambda_Index = 0
	  IF Filter EQ 'oiii' THEN Params.Lambda_Index = 1
	  IF Filter EQ 'hei' THEN Params.Lambda_Index = 2
	  IF Filter EQ 'nii_blue' THEN Params.Lambda_Index = 3
	  IF Filter EQ 'oi' THEN Params.Lambda_Index = 4
	  IF Filter EQ 'ha' OR Filter EQ 'ha_barr' OR Filter EQ 'ha_omega' THEN $
		 Params.Lambda_Index = 5
	  IF Filter EQ 'nii' OR Filter EQ 'nii_red' THEN Params.Lambda_Index = 6
	  IF Filter EQ 'sii' THEN Params.Lambda_Index = 7
	  IF Filter EQ 'oii' THEN Params.Lambda_Index = 8
  ENDIF ELSE IF obs EQ 'ctio' THEN $
  	  params.lambda_index = where(filter EQ filterlist)

  PRINT, Filter, Params.Lambda_Index
  

  ;; Get Julian Date for LSR
  Params.JD = SYSTIME(/JUL)
  
  ;; Create Widgets
  
  Widget_Main = Widget_Base(title = 'Fancy Tune', /Row)
  
  Widget_Col_1 = Widget_Base(Widget_Main, /Col, Frame = 5)
  
  Widget.Lon = CW_Field(Widget_Col_1, /Float, $
                        Title = 'Longitude', Value = 0.0, $
                        XSIZE = 15, /Return_Events)
  
  Widget.Lat = CW_Field(Widget_Col_1, /Float, $
                        Title = 'Latitude ', $
                        Value = 0.0, XSIZE = 15, $
                        /Return_Events)
  
  Widget_Sub_Col_1 = Widget_Base(Widget_Col_1, /Row)
  
  Widget_Label = WIDGET_LABEL(Widget_Sub_Col_1, Value = 'Filter:  ')
  
  Widget.Filter = WIDGET_COMBOBOX(Widget_Sub_Col_1, Value = filterlist, $
                                  UValue = Params.Lambda, XSIZE = 108)
  
  
  Widget_Sub2_Col_1 = Widget_Base(Widget_Col_1, /Row)
  
  Widget_Label = WIDGET_LABEL(Widget_Sub2_Col_1, Value = 'Template:')
  
  Widget.Template = WIDGET_COMBOBOX(Widget_Sub2_Col_1, Value = List, XSIZE = 108)  
  
  Widget.Use_Survey = WIDGET_BUTTON(Widget_Col_1, Value = 'Overplot Survey Spectra', $
                                    UValue = "Don't Overplot Survey Spectra")
  
  Widget_Sub3_Col_1 = Widget_Base(Widget_Col_1, /Col, Frame = 3)
  
  Widget_Label = WIDGET_LABEL(Widget_Sub3_Col_1, Value = 'Emission Line Params')
  
  Widget.VLSR = CW_Field(Widget_Sub3_Col_1, /Float, $
                         Title = 'Vlsr [km/s] ', Value = 0.0, $
                         XSIZE = 11, /RETURN_EVENTS)
  Widget.Width = CW_Field(Widget_Sub3_Col_1, /Int, $
                          Title = 'Width [km/s]', Value = 25, $
                          XSIZE = 11, /RETURN_EVENTS)
  Widget.Area = CW_Field(Widget_Sub3_Col_1, /Float, $
                         Title = 'I [R]       ', Value = 1.0, $
                         XSIZE = 11, /RETURN_EVENTS)
  
  Widget_Sub4_Col_1 = Widget_Base(Widget_Col_1, /Col, Frame = 3)
  
  ;; Results
  
  Params.LSR_0 = vlsr(lon = Params.Lon, lat = Params.Lat, JD = Params.JD)
  
  Widget_Label = WIDGET_LABEL(Widget_Sub4_Col_1, Value = 'Results')
  
  Widget.Geo_0 = WIDGET_LABEL(Widget_Sub4_Col_1, $
                              Value = STRING('Geo. Zero is at', Params.Geo_0, $
                                             ' km/s', FORMAT = '(A,F7.2,A)'))
  
  Widget.LSR_0 = WIDGET_LABEL(Widget_Sub4_Col_1, $
                              Value = STRING('LSR Zero is at ', Params.Geo_0+Params.LSR_0, $
                                             ' km/s', FORMAT = '(A,F7.2,A)'))
  
  Widget.Line_Vel = WIDGET_LABEL(Widget_Sub4_Col_1, $
                                 Value = STRING('Emission is at ', $
                                                Params.Geo_0+Params.LSR_0+Params.VLSR, $
                                                ' km/s', FORMAT = '(A,F7.2,A)'))
  
  Params.Tune[0] = Params.Tune_Ref[Params.Lambda_Index, 0]
  Params.Tune[1] = Params.Tune_Ref[Params.Lambda_Index, 1]
  
  Widget.Tune = WIDGET_LABEL(Widget_Sub4_Col_1, $
                             Value = STRING('Tune is   ', $
                                            Params.Tune[0], ',', $
                                            Params.Tune[1], ' cm Hg', $
                                            FORMAT = '(A,F5.1,A,F5.1,A)'))
  
  Widget.Predict_Tune = WIDGET_BUTTON(Widget_Sub4_Col_1, $
                                      Value = 'Predict Tune!')
  
  Widget.Quit = Widget_Button(Widget_Col_1, Value = 'Insert Tune into Tuner & Quit')
  
  Widget_Col_2 = Widget_Base(Widget_Main, /Col, Frame = 5)
  
  Widget.Window = Widget_Draw(Widget_Col_2, XSize = 500, $
                                    YSize = 500, Retain = 2, $
                                    /Button_Events)
  
  Widget_Control, Widget_Main, /Realize
  Widget_Control, Widget.Window, Get_Value = Temp
  Widget.WinNum = Temp
  Widget_Control, Widget.Filter, SET_COMBOBOX_SELECT = Params.Lambda_Index
  
  ;; Plot up initial Line and scale
  Plot_It
  
  XManager, 'FANCY_TUNE', Widget_Main
  
  Output_Tune = STRING(Params.Tune[0], '  ', Params.Tune[1], $
                       FORMAT = '(F5.1,A,F5.1)')
  

END

PRO FANCY_TUNE_EVENT, EVENT
  
  COMMON Widget
  COMMON Params
  COMMON Map, WhamNSS
  
  CASE Event.ID OF
     Widget.Lon: BEGIN
        WIDGET_CONTROL, Event.ID, Get_Value = Temp
        Params.Lon = Temp
        WIDGET_CONTROL, Widget.Lat, Get_Value = Temp
        Params.Lat = Temp
        Params.LSR_0 = vlsr(lon = Params.Lon, lat = Params.Lat, JD = Params.JD)
        Widget_Update
        Plot_IT
     END
     Widget.Lat: BEGIN
        WIDGET_CONTROL, Event.ID, Get_Value = Temp
        Params.Lat = Temp
        WIDGET_CONTROL, Widget.Lon, Get_Value = Temp
        Params.Lon = Temp
        Params.LSR_0 = vlsr(lon = Params.Lon, lat = Params.Lat, JD = Params.JD)
        Widget_Update
        Plot_IT
     END
     Widget.Filter: BEGIN
        WIDGET_CONTROL, Widget.Tune, Sensitive = 0
        Params.Lambda_Index = Event.Index
        Params.Tune[0] = Params.Tune_Ref[Event.Index, 0]
        Params.Tune[1] = Params.Tune_Ref[Event.Index, 1]
        Widget_Update
        PLOT_IT
     END
     Widget.Template: BEGIN
        Params.Template_Index = Event.Index
        PLOT_IT
     END
     Widget.Use_Survey: BEGIN
        Params.Use_Survey = (Params.Use_Survey +1) MOD 2
        Widget_Control, event.id, Get_Value = Value, $
           Get_UValue = UValue
        Widget_Control, event.id, Set_Value = UValue, $
           Set_UValue = Value
        IF Params.Use_Survey THEN BEGIN
           Widget_Control, Event.ID, /HourGlass
           IF NOT(KEYWORD_SET(WhamNSS)) THEN RESTORE, '/d/wham/survey/whamnss.dat'
        ENDIF
        PLOT_IT
     END
     Widget.VLSR: BEGIN
        WIDGET_CONTROL, Event.ID, Get_Value = Temp
        Params.VLSR = Temp
        Widget_Update
        PLOT_IT
     END
     Widget.Width: BEGIN
        WIDGET_CONTROL, Event.ID, Get_Value = Temp
        Params.Width = Temp
        PLOT_IT
     END
     Widget.Area: BEGIN
        WIDGET_CONTROL, Event.ID, Get_Value = Temp
        Params.Area = Temp
        PLOT_IT
     END
     Widget.Predict_Tune: BEGIN
        IF Params.Geo_0 NE 50.0 THEN BEGIN ;; Predict crashes when delta=0
           L_Ref = Params.Lambda[Params.Lambda_Index]
           L_Targ = L_Ref + L_Ref * (50-Params.Geo_0)/3.0e5
           P_Ref = Params.Tune_Ref[Params.Lambda_Index, 0]
           Params.Tune[0] = Predict_Tune(L_Ref, P_Ref, L_Targ, $
                                         Space = 0.04707, /air)
           P_Ref = Params.Tune_Ref[Params.Lambda_Index, 1]
           Params.Tune[1] = Predict_Tune(L_Ref, P_Ref, L_Targ, $
                                         Space = 0.02013, /air)
           Widget_Update
        ENDIF
        Widget_Control, Widget.Tune, /Sensitive
     END
     Widget.Window: BEGIN
        WIDGET_CONTROL, Widget.Tune, Sensitive = 0
        IF Event.Press THEN BEGIN
           Coords = Convert_Coord([Event.X, Event.Y], /Dev, /To_Data)
           Params.X1 = Coords[0]
        ENDIF
        IF Event.Release THEN BEGIN
           Coords = Convert_Coord([Event.X, Event.Y], /Dev, /To_Data)
           Params.X2 =  Coords[0]
           Delta_X = Params.X2-Params.X1
           Params.Geo_0 = Params.Geo_0 + Delta_X
           Widget_Update
           PLOT_IT
        ENDIF
     END
     Widget.Quit: WIDGET_CONTROL, Event.Top, /Destroy
     
  ENDCASE
  
  
END
PRO PLOT_IT
  
  COMMON Widget
  COMMON Params
  COMMON Map
  
  ;; Grab Coords and Line Params
  WIDGET_CONTROL, Widget.Lon, Get_Value = Temp
  Params.Lon = Temp
  WIDGET_CONTROL, Widget.Lat, Get_Value = Temp
  Params.Lat = Temp
  WIDGET_CONTROL, Widget.VLSR, Get_Value = Temp
  Params.VLSR = Temp
  WIDGET_CONTROL, Widget.Width, Get_Value = Temp
  Params.Width = Temp
  WIDGET_CONTROL, Widget.Area, Get_Value = Temp
  Params.Area = Temp


  WSET, Widget.WinNum
  ;; Get velocity
  V = Vel_Stretch(Params.Lambda[Params.Lambda_Index])
  ;; Get Line
  Data = WHAMGAUSS(V, [0, Params.Geo_0+Params.VLSR+Params.LSR_0, $
                       Params.Width, Params.Area*22.8], $
                   1, ip = 'smorn_ip.dat')
  PLOT, V[33:132], Data[33:132], $
     ytitle = 'I [ADU/s]', xtitle = 'Standard Velocity Scale [km/s]', $
     xstyle = 9, position = [.15, .1, .9, .87], $
     yrange = [0, Params.Area]
  AXIS, /xaxis,  xstyle = 1,  $
     xtitle = 'V!DLSR!N [km/s]', $
     xrange = [V[33]-Params.Geo_0-Params.LSR_0, $
               V[132]-Params.Geo_0-Params.LSR_0]
  title_loc = convert_coord(avg(!x.crange), 0, /data, /to_norm)
  XYOUTS, title_loc[0], .95, $
     'Use Mouse to Drag Position of Galactic Emission',  /normal, $
     align = 0.5, charsize = 1.5
  
  ;; Plot up a template
  IF Params.Template_Index GT 0 THEN BEGIN
     dir = '/d/wham/lib/whamspect2/atmos/'
     RESTORE, dir+'atmosfile_template.dat'
     Atmos_template = read_ascii(dir+Params.Template_Files[Params.Template_Index]+'.txt', $
                                 template = atmosfile_template)
     D = FLTARR(N_ELEMENTS(V))
     FOR i = 0, N_ELEMENTS(Atmos_Template.Mean)-1 DO BEGIN
        D =  D + $
           WHAMGAUSS(V, [0, Atmos_Template.Mean[i]+Params.Geo_0, $
                         Atmos_Template.FWHM[i], Atmos_Template.Area[i]*2], $
                     1, ip = 'smorn_ip.dat')
     ENDFOR
     OPLOT, V, D, Color = 39
     KLEGEND, [.75, .85], [-0, 0], ['Template'], Color = 39
  ENDIF
  
  ;; Plot up Spectra from WHAM Map
  IF Params.Use_Survey THEN BEGIN
     Near = SpectNear(WhamNSS, Params.Lon, Params.Lat, 0.6, Count)
     IF Count GT 0 THEN BEGIN
        FOR i = 0, Count-1 DO BEGIN
           N = N_ELEMENTS(WhamNSS[Near[i]].Data) 
           OPLOT, WhamNSS[Near[i]].Vel[N-100:N-1]+Params.Geo_0+Params.LSR_0, $
              WhamNSS[Near[i]].Data[N-100:N-1]/30, Color = 78
           KLEGEND, [.75, .8], [-0, 0], ['Survey'], Color = 78
        ENDFOR
     ENDIF
  ENDIF
  
 ;; Plot up Geo_0 and LSR_0
  OPLOT, [Params.Geo_0, Params.Geo_0], [-1.0e8, 1.0e8], LINESTYLE = 1
  OPLOT, [Params.Geo_0+Params.LSR_0, Params.Geo_0+Params.LSR_0], $
     [-1.0e8, 1.0e8], LINESTYLE = 2
  KLEGEND, [.75, .95], [[-1, 0], [-2, 0]], ['Geo. Zero', 'LSR Zero']

END

PRO WIDGET_UPDATE
  
  
  COMMON Widget
  COMMON Params
  
  Widget_Control, Widget.Tune, $
     Set_Value = STRING('Tune is   ', $
                        Params.Tune[0], ',', $
                        Params.Tune[1], ' cm Hg', $
                        FORMAT = '(A,F5.1,A,F5.1,A)')
  
  Widget_Control, Widget.Geo_0, $
     Set_Value = STRING('Geo. Zero is at', Params.Geo_0, $
                        ' km/s', FORMAT = '(A,F7.2,A)')
  
  Widget_Control, Widget.LSR_0, $
     Set_Value = STRING('LSR Zero is at ', Params.Geo_0+Params.LSR_0, $
                        ' km/s', FORMAT = '(A,F7.2,A)')
  
  Widget_Control, Widget.Line_Vel, $
     Set_Value = STRING('Emission is at ', $
                        Params.Geo_0+Params.LSR_0+Params.VLSR, $
                        ' km/s', FORMAT = '(A,F7.2,A)')
  
END


