@/d/wham/pro/whamview/greg_xloadct
@/d/wham/pro/whamview/greg_intmap
@/d/wham/pro/whamview/greg_whammap
@/d/wham/pro/whamview/greg_xinteranimate
@/d/wham/pro/whamview/greg_cw_animate

PRO WHAMVIEW
  
  COMMON WHAMVIEW_Widgets, Widget_Main,  Widget_Bar_1, Widget_Bar_2, Widget_Bar_4, $
     Widget_Window_1, $
     Widget_Window_1_Label, Widget_Window_1_Field, $
     Widget_Window_1_Buttons, $
     Widget_Window_1_Slider, Widget_Window_2, $
     Options_Widget
  
  
  COMMON WHAMVIEW_Data, Map, Map_Int, Map_FileName, Rcd_FileName, Par_Flags, Map_Click_Flags, $
     Options_Flags, Display_Flags, Zoom_Box, Add_Sub_Box, CurSpect, Manip_Flags, Print_Flags, $
     Plot_Flags, Plots_Flags, N_Max_Manip, Add_Indices, Add_i, Sub_Indices, Sub_i
  
  LOADCT, 3
  
  Par_Flags = { Map_Path:"/d/wham/survey/", Rcd_Path:"~", $
                LMin:0, LMax:0, BMin:0, BMax:0, $
                Center:0, Vcen:0, DeltaV:0, $
                Use_HI:0, Use_CO:0, Use_Hi_Res_CO:0, Ignore_Center:1, Ra_Dec:0}
  Map_FileName = ' '
  Rcd_FileName = ' '
  Map_Click_Flags = { Zoom_In:0, Zoom_Out:0, CurSpect:0, Add_Sub:0}
  Manip_Flags = { Add:0, Subtract:0, $
                  Save:0, Save_File_Base:'', Path:'/d/wham2/'}
  Print_Flags = { To_Printer:0, To_File:0, Cancel:0, Name:"floor4", $
                  PS_Name:"idl.ps", Path : ""}
  Plot_Flags = { X_Min:0.0, X_Max:1.0, Y_Min:0.0, Y_Max:1.0, $
                 X:FLTARR(1000), Y:FLTARR(1000), $
                 title: "", xtitle:"", ytitle:"", $
                 Oplot_X:FLTARR(100, 1000), Oplot_Y:FLTARR(100, 1000), $
                 N_Oplot:0, Use_White:INTARR(100)}
  Options_Flags = {Linear:0, Log:0, Convert_NSS:0, Convert_ProcSpec:0, $
                   Smooth:0, Fill:0, NoStars_Fast:0, $
                   NoStars_Slow:0, NoGrid:0, Color_Bar:0, $
                   Fix_Missing:0, N_Frames:0, Track_Negs:0, Yellow_Bkg:0}
  Display_Flags = {Default:1, ForPapers:0}
  
  Zoom_Box = FLTARR(2, 3)
  Add_Sub_Box = FLTARR(2, 3)
  CurSpect = { X_Win_1:!X, Y_Win_1:!Y, X_Win_2:!X, Y_Win_2:!Y }
  
  
  ;; Max number of pluses or minuses for Manip spectra
  N_Max_Manip = 5000
  Add_Indices = LONARR(N_Max_Manip) &  Add_Indices(*) = 99999
  Sub_Indices = LONARR(N_Max_Manip) &  Sub_Indices(*) = 99999
  Add_i = 0 &  Sub_i =  0
  Plots_Flags = { N_Plus:0, N_Minus:0, $
                  Plus_Lon:FLTARR(N_Max_Manip), Plus_Lat:FLTARR(N_Max_Manip), $
                  Minus_Lon:FLTARR(N_Max_Manip), Minus_Lat:FLTARR(N_Max_Manip) }

  Widget_Bar_1 = { Base:0L, Load_Map:0L, Draw_Map:0L, $
                   Print_Map:0L, Print_Spectra:0L, Quit:0L}
  Widget_Bar_2 = { Base:0L, Label:0L }
  Widget_Window_1 = {Base:0L, Label:0L, Window:0L, WinNum:0L }
  Widget_Window_1_Label = { Base:0L, Label:0L, LMin:0L, LMax:0L, BMin:0L, BMax:0L, $
                            Center:0L, VCen:0L, DeltaV:0L }
  Widget_Window_1_Field  = { Base:0L, Label:0L, LMin:0L, LMax:0L, BMin:0L, BMax:0L, $
                             Center:0L, VCen:0L, DeltaV:0L }
  Widget_Window_1_Buttons = { Use_Map:0L, Grab_Map:0L, Ignore:0L, Ra_Dec:0L,  Other:0L }
  Widget_Window_1_Slider = { Base:0L, Button:0L }
  Widget_Bar_3 = { Base:0L, Label:0L }
  Widget_Bar_4 = { Base:0L, Label:0L, Zoom_In:0L,  Zoom_Out:0L, Zoom_Factor:0L, $
                   CurSpect:0L, Add_Sub:0L}
  Widget_Window_2 = { Base:0L, Sub_Base:0L, Y1:0L, Y2:0L, X1:0L, X2:0L, $
                      Window:0L, Winnum:0L, Add:0L, Subtract:0L, $
                      Load:0L, Plot:0L, Save:0L, $
                      Reset:0L, Default:0L, ForPapers:0L}
  ;;YSize = 1000
  ;;XSize = 1200
  ;; Set window size
  Dims = Get_Screen_Size() 
  YSize = ROUND(450 * Dims[1] / 1200. )
  XSize = ROUND(589 * Dims[0] / 1600.)
  ;; Set button size
  Button_YSize = (ROUND(Dims[0]) EQ 1600) ? 0 : 20

  ;; Set charsize
  SPAWN, 'echo $HOSTTYPE', hosttype
  IF hosttype EQ 'mips' THEN CHARSIZE = 0.75

  Widget_Main = Widget_Base(title = 'WhamView', /Col)

  Widget_Bar_1.Base = Widget_Base(Widget_Main, /Row,  Frame = 5)
  
  Widget_Bar_1.Load_Map = Widget_Button(Widget_Bar_1.Base, Value = 'Load Map', $
                                       YSize = Button_YSize)

  Widget_Bar_1.Draw_Map = Widget_Button(Widget_Bar_1.Base, Value = 'Draw Map', $
                                       YSize = Button_YSize)
  
  Widget_Bar_1.Print_Map = Widget_Button(Widget_Bar_1.Base, Value = 'Print Top', $
                                       YSize = Button_YSize)
  
  Widget_Bar_1.Print_Spectra = Widget_Button(Widget_Bar_1.Base, Value = 'Print Bottom', $
                                       YSize = Button_YSize)
  
  Widget_Bar_1.Quit = Widget_Button(Widget_Bar_1.Base, Value = 'Quit', $
                                       YSize = Button_YSize)

  Widget_Bar_2.Base = Widget_Base(Widget_Main, /Row)
  
  Widget_Bar_2.Label = Widget_Label(Widget_Bar_2.Base, $
                                    Value = ' Map Params for EMPTY MAP', /dyn)
  

  Widget_Window_1.Base = Widget_Base(Widget_Main, /Row, Frame = 2)
  

  Sub_Base = Widget_Base(Widget_Window_1.Base, /Col)

  Sub_1_Base = Widget_Base(Sub_Base, /Row, Frame = 5)
  

  Widget_Window_1_Label.Base = Widget_Base(Sub_1_Base, /Col)

  Widget_Window_1_Field.Base = Widget_Base(Sub_1_Base, /Col)

  Widget_Window_1_Slider.Base = Widget_Base(Widget_Window_1.Base, /Col)

  Widget_Window_1_Label.LMin = Widget_Button(Widget_Window_1_Label.Base, $
                                             Value = '    Lmin:', $
                                             UValue = '  * Lmin:', $
                                             YSize = 37, /align_right)
  Widget_Window_1_Label.LMax = Widget_Button(Widget_Window_1_Label.Base, $
                                             Value = '    Lmax:', $
                                             UValue = '  * Lmax:', $
                                             YSize = 37, /Align_Right)
  Widget_Window_1_Label.BMin = Widget_Button(Widget_Window_1_Label.Base, $
                                             Value = '    Bmin:', $
                                             UValue = '  * Bmin:',  $
                                             YSize = 37, /Align_Right)
  Widget_Window_1_Label.BMax = Widget_Button(Widget_Window_1_Label.Base, $
                                             Value = '    Bmax:', $
                                             UValue = '  * Bmax:',  $
                                             YSize = 37, /Align_Right)
  Widget_Window_1_Label.Center = Widget_Button(Widget_Window_1_Label.Base, $
                                               Value = '  Center:', $
                                               UValue = '* Center:',  $
                                               YSize = 37, /Align_Right)
  Widget_Window_1_Label.Vcen = Widget_Button(Widget_Window_1_Label.Base, $
                                             Value = '    Vcen:', $
                                             UValue = '  * Vcen:',  $
                                             YSize = 37, /Align_Right)
  Widget_Window_1_Label.DeltaV = Widget_Button(Widget_Window_1_Label.Base, $
                                               Value = '  DeltaV:', $
                                               UValue = '* DeltaV:',  $
                                               YSize = 37, /Align_Right)

  Widget_Window_1_Field.LMin = CW_Field(Widget_Window_1_Field.Base, $
                                        /All_Events, $
                                        /Integer, $
                                        Title = '', Value = 0, $
                                        XSize = 5)
  Widget_Window_1_Field.LMax = CW_Field(Widget_Window_1_Field.Base, $
                                        /All_Events, $
                                        /Integer, $
                                        Title = '', Value = 360, $
                                        XSize = 5)
  Widget_Window_1_Field.BMin = CW_Field(Widget_Window_1_Field.Base, $
                                        /All_Events, $
                                        /Integer, $
                                        Title = '', Value = -90, $
                                        XSize = 5)
  Widget_Window_1_Field.BMax = CW_Field(Widget_Window_1_Field.Base, $
                                        /All_Events, $
                                        /Integer, $
                                        Title = '', Value = 90, $
                                        XSize = 5)
  Widget_Window_1_Field.Center = CW_Field(Widget_Window_1_Field.Base, $
                                          /All_Events, /Integer, $
                                          Title = '', Value = 120, $
                                          XSize = 5)
  Widget_Window_1_Field.VCen = CW_Field(Widget_Window_1_Field.Base, $
                                        /All_Events, /Integer, $
                                        Title = '', Value = 0, $
                                        XSize = 5)
  Widget_Window_1_Field.DeltaV = CW_Field(Widget_Window_1_Field.Base, $
                                          /All_Events, /Integer, $
                                          Title = '', Value = 0, $
                                          XSize = 5)

  Widget_Window_1_Slider.Button = CW_FSlider(Widget_Window_1_Slider.Base, Title = '', $
                                             YSize = YSize, $
                                             Minimum = 0, Maximum = 1.0, Value = 0.0, $
                                             /Vert, /Suppress, /Drag)
  
  Widget_Window_1.Window = Widget_Draw(Widget_Window_1.Base, XSize = XSize, $
                                       YSize = YSize, Retain = 2, $
                                       /Button_Events)
  
  ;;Sub_2_Base = Widget_Base(Sub_Base, /Col, Frame = 5)
  Sub_2_Base = Widget_Base(Widget_Window_1.Base, /Col)
  
  Sub_2_Base_Top = Widget_Base(Sub_2_Base, /Col, Frame = 5,  /align_center)
  Sub_2_Base_Bot = Widget_Base(Sub_2_Base, /Col, Frame = 5)
  
  Widget_Window_1_Buttons.Use_Map = Widget_DropList(Sub_2_Base_Top, $
                                                    Value = ['Use WHAM Map', $
                                                             ' Use HI Map ', $
                                                             ' Use CO Map ', $
                                                             'Use Hi Res CO'], $
                                                    UValue = ['WHAM', 'HI', 'CO', 'Hi_Res_CO'])

  
  Widget_Window_1_Buttons.Grab_Map = Widget_Button(Sub_2_Base_Top, $
                                                   Value = 'Grab Map', $
                                                   SENS = 0, YSize = Button_YSize)
  
  Widget_Window_1_Buttons.Ignore = Widget_Button(Sub_2_Base_Top, $
                                                 Value = '* Ignore Center *', $
                                                 UValue = '  Ignore Center  ', $
                                                 YSize = Button_YSize)

  Widget_Window_1_Buttons.Ra_Dec = Widget_Button(Sub_2_Base_Top, $
                                                 Value = 'RA/DEC Coords', $
                                                 UValue = '* RA/DEC Coords *', $
                                                 YSize = Button_YSize)

  Widget_Window_1_Buttons.Other = Widget_Button(Sub_2_Base_Top, $
                                                Value = '    Other Options  ', $
                                                 YSize = Button_YSize)

  
;; Bar 3 for future use

;  Widget_Bar_3.Base = Widget_Base(Widget_Main, /Row)
  
;  Widget_Bar_3.Label = Widget_Label(Widget_Bar_3.Base, Value = '          ')
  
  ;;Widget_Bar_4.Base = Widget_Base(Widget_Main, /Row,  Frame = 5)
  
  
  Widget_Bar_4.Label = Widget_Label(Sub_2_Base_Bot, Value = 'Clicking on Map will: ', $
                                    YSize = Button_YSize)
  
  Widget_Bar_4.Zoom_In = Widget_Button(Sub_2_Base_Bot, Value = '  Zoom In ', $
                                       UValue = '* Zoom In ', $
                                       YSize = Button_YSize)
  
  Sub_Base = Widget_Base(Sub_2_Base_Bot, /Col, Frame = 2)

  Widget_Bar_4.Zoom_Out = Widget_Button(Sub_Base, Value = '  Re-Centered and Zoom by:', $
                                        UValue = '* Re-Centered Zoom by', $
                                        YSize = Button_YSize)
  
  Widget_Bar_4.Zoom_Factor = CW_Field(Sub_Base, $
                                      /All_Events, /Float, $
                                      Title = '         ', Value = 2.0, $
                                      XSize = 4)
  
  Widget_Bar_4.CurSpect = Widget_Button(Sub_2_Base_Bot, Value = '  Examine Spectra ', $
                                        UValue = '*Examine Spectra*', $
                                        YSize = Button_YSize)

  Widget_Bar_4.Add_Sub = Widget_Button(Sub_2_Base_Bot, Value = ' Add/Subtract Spectra ', $
                                       UValue = '*Add/Subtract Spectra*', $
                                       YSize = Button_YSize)

  Widget_Window_2.Base = Widget_Base(Widget_Main,  /Row)

  Sub_Base = Widget_Base(Widget_Window_2.Base, /Col, Frame = 2)
  
  Widget_Label = Widget_Label(Sub_Base, Value = '  Manipulate Spectra ', YSize = 50)

  Widget_Window_2.Add = Widget_Button(Sub_Base, Value = 'Add Spectra', $
                                          UValue = '*Add Spectra*', Sensitive = 0)
  Widget_Window_2.Subtract = Widget_Button(Sub_Base, Value = 'Subtract Spectra', $
                                               UValue = '*Subtract Spectra*', Sensitive = 0)
  Widget_Window_2.Plot = Widget_Button(Sub_Base, Value = 'Plot Spectra', Sensitive = 0)
  Widget_Window_2.Load = Widget_Button(Sub_Base, Value = 'Load .rcd File', Sensitive = 0)
  Widget_Window_2.Save = Widget_Button(Sub_Base, Value = 'Save Spectra & Record', $
                                       Sensitive = 0)
  Widget_Window_2.Reset = Widget_Button(Sub_Base, Value = 'Reset/Redraw', Sensitive = 0)

  Widget_Window_2.Y1 = CW_FSlider(Widget_Window_2.Base, Title = '', $
                                  YSize = YSize, $
                                  Minimum = 0, Maximum = 1.0, Value = 0.0, $
                                  /Vert, /Suppress)
  Widget_Window_2.Y2 = CW_FSlider(Widget_Window_2.Base, Title = '', $
                                  YSize = YSize, $
                                  Minimum = 0, Maximum = 1.0, Value = 1.0, $
                                  /Vert, /Suppress)

  Widget_Window_2.Sub_Base = Widget_Base(Widget_Window_2.Base, /Col)
  
  Widget_Window_2.Window = Widget_Draw(Widget_Window_2.Sub_Base, $
                                       XSize = XSize, $
                                       YSize = YSize, $
                                       Retain = 2)

  Widget_Window_2.X1 = CW_FSlider(Widget_Window_2.Sub_Base, Title = '', $
                                  XSize = XSize, $
                                  Minimum = 0, Maximum = 1.0, Value = 0.0, $
                                  /Suppress)
  Widget_Window_2.X2 = CW_FSlider(Widget_Window_2.Sub_Base, Title = '', $
                                  XSize = XSize, $
                                  Minimum = 0, Maximum = 1.0, Value = 1.0, $
                                  /Suppress)
  Sub_Base = Widget_Base(Widget_Window_2.Base, /Col, Frame = 2)
  Widget_Label = Widget_Label(Sub_Base, Value = '  Display/Font Options ')
  
  Widget_Window_2.Default = Widget_Button(Sub_Base, Value = '*Default*', $
                                          UValue = ' Default ', Sensitive = 1)
  Widget_Window_2.ForPapers = Widget_Button(Sub_Base, Value = 'For Papers', $
                                               UValue = '*For Papers*', Sensitive = 0)

  
  Widget_Control, Widget_Main, /Realize
  Widget_Control, Widget_Window_1.Window, Get_Value = temp
  Widget_Window_1.WinNum = temp
  Widget_Control, Widget_Window_2.Window, Get_Value = temp
  Widget_Window_2.WinNum = temp

  XManager, 'WHAMVIEW', Widget_Main

END

PRO WHAMVIEW_Event, Event

  COMMON WHAMVIEW_Widgets

  COMMON WHAMVIEW_Data

  CASE Event.ID OF
     Widget_Bar_1.Load_Map: BEGIN
        Load_Map
        Widget_Control, Widget_Bar_2.Label, Set_Value = ' Map Params for ' + Map_FileName
     END
     Widget_Bar_1.Draw_Map: BEGIN
        Widget_Control, Widget_Window_1_Field.LMin, Get_Value = LMin
        Widget_Control, Widget_Window_1_Field.LMax, Get_Value = LMax
        Widget_Control, Widget_Window_1_Field.BMin, Get_Value = BMin
        Widget_Control, Widget_Window_1_Field.BMax, Get_Value = BMax
        IF LMin GE LMax OR BMin GE BMax THEN BEGIN
           Error = DIALOG_MESSAGE("Invalid Coordinate Range", TITLE = "Error!", $
                                  /ERROR)
           Print_Flags.Cancel = 1
        ENDIF
        IF NOT(Print_Flags.Cancel) THEN Draw_Map
        Print_Flags.Cancel = 0
     END
     Widget_Bar_1.Print_Map:BEGIN
        Get_Printer_Name
        Widget_Control, Widget_Window_1_Field.LMin, Get_Value = LMin
        Widget_Control, Widget_Window_1_Field.LMax, Get_Value = LMax
        Widget_Control, Widget_Window_1_Field.BMin, Get_Value = BMin
        Widget_Control, Widget_Window_1_Field.BMax, Get_Value = BMax
        IF LMin GE LMax OR BMin GE BMax THEN BEGIN
           Error = DIALOG_MESSAGE("Invalid Coordinate Range", TITLE = "Error!", $
                                  /ERROR)
           Print_Flags.Cancel = 1
        ENDIF
        IF NOT(Print_Flags.Cancel) THEN Draw_Map
        Print_Flags.Cancel = 0
     END
     Widget_Bar_1.Print_Spectra:BEGIN

        ;; Note that user can only print out manip. spectra and
        ;; individual curspect spectra
        Get_Printer_Name
        IF NOT(Print_Flags.Cancel) THEN BEGIN
           IF Map_Click_Flags.Add_Sub THEN Plot_Manip_Spectra
           IF Map_Click_Flags.CurSpect THEN BEGIN
              Set_Plot, 'ps'
              IF Print_Flags.To_Printer THEN Device, /Land, File = 'idl.ps'
              IF Print_Flags.To_File THEN Device,  /Land, File = Print_Flags.PS_Name
              CurSpect.X_Win_1 = !X
              CurSpect.Y_Win_1 = !Y
              Plot_Spect
              !x = CurSpect.X_Win_1
              !y = CurSpect.Y_Win_1
              Device,  /close
              Set_Plot, 'x'
              IF Print_Flags.To_Printer THEN BEGIN
                 PRINT, 'PRINTING1'
                 Spawn, 'lp -d '+Print_Flags.Name+' idl.ps'
                 Spawn, '/bin/rm idl.ps'
              ENDIF
              Print_Flags.To_Printer = 0
              Print_Flags.To_File = 0
           ENDIF
        ENDIF
        Print_Flags.Cancel = 0
     END
     Widget_Bar_1.Quit: Widget_Control, Event.Top,  /Destroy
     Widget_Window_1_Label.LMin:BEGIN
        Widget_Control, Event.ID, Get_Value = buttonValue, $
                        Get_UValue = buttonUValue
        Widget_Control, Event.ID, Set_Value = buttonUValue, $
                        Set_UValue = buttonValue
        Par_Flags.LMin = (Par_Flags.LMin+1) MOD 2
        Sensitize, Event.ID, Par_Flags.LMin
        IF Par_Flags.LMin THEN BEGIN
           Widget_Control, Widget_Window_1_Field.LMin, Get_Value = LMin
           Widget_Control, Widget_Window_1_Slider.Button, Set_Value = (LMin/360.)
        ENDIF
        
     END
     Widget_Window_1_Label.LMax:BEGIN
        Widget_Control, Event.ID, Get_Value = buttonValue, $
                        Get_UValue = buttonUValue
        Widget_Control, Event.ID, Set_Value = buttonUValue, $
                        Set_UValue = buttonValue
        Par_Flags.LMax = (Par_Flags.LMax+1) MOD 2
        Sensitize, Event.ID, Par_Flags.LMax
        IF Par_Flags.LMax THEN BEGIN
           Widget_Control, Widget_Window_1_Field.LMax, Get_Value = LMax
           Widget_Control, Widget_Window_1_Slider.Button, Set_Value = (LMax/360.)
        ENDIF
     END
     Widget_Window_1_Label.BMin:BEGIN
        Widget_Control, Event.ID, Get_Value = buttonValue, $
                        Get_UValue = buttonUValue
        Widget_Control, Event.ID, Set_Value = buttonUValue, $
                        Set_UValue = buttonValue
        Par_Flags.BMin = (Par_Flags.BMin+1) MOD 2
        Sensitize, Event.ID, Par_Flags.BMin
        IF Par_Flags.BMin THEN BEGIN
           Widget_Control, Widget_Window_1_Field.BMin, Get_Value = BMin
           Widget_Control, Widget_Window_1_Slider.Button, Set_Value = ((BMin+90)/180.)
        ENDIF
     END
     Widget_Window_1_Label.BMax:BEGIN
        Widget_Control, Event.ID, Get_Value = buttonValue, $
                        Get_UValue = buttonUValue
        Widget_Control, Event.ID, Set_Value = buttonUValue, $
                        Set_UValue = buttonValue
        Par_Flags.BMax = (Par_Flags.BMax+1) MOD 2
        Sensitize, Event.ID, Par_Flags.BMax
        IF Par_Flags.BMax THEN BEGIN
           Widget_Control, Widget_Window_1_Field.BMax, Get_Value = BMax
           Widget_Control, Widget_Window_1_Slider.Button, Set_Value = ((BMax+90)/180.)
        ENDIF
     END
     Widget_Window_1_Label.Center: BEGIN
        Widget_Control, Event.ID, Get_Value = buttonValue, $
                        Get_UValue = buttonUValue
        Widget_Control, Event.ID, Set_Value = buttonUValue, $
                        Set_UValue = buttonValue
        Par_Flags.Center = (Par_Flags.Center+1) MOD 2
        Sensitize, Event.ID, Par_Flags.Center
        IF Par_Flags.Center THEN BEGIN
           Widget_Control, Widget_Window_1_Field.Center, Get_Value = Center
           Widget_Control, Widget_Window_1_Slider.Button, Set_Value = (Center/360.)
        ENDIF
     END
     Widget_Window_1_Label.VCen: BEGIN
        Widget_Control, Event.ID, Get_Value = buttonValue, $
                        Get_UValue = buttonUValue
        Widget_Control, Event.ID, Set_Value = buttonUValue, $
                        Set_UValue = buttonValue
        Par_Flags.VCen = (Par_Flags.VCen+1) MOD 2
        Sensitize, Event.ID, Par_Flags.VCen
        IF Par_Flags.VCen THEN BEGIN
           Widget_Control, Widget_Window_1_Field.VCen, Get_Value = VCen
           Widget_Control, Widget_Window_1_Slider.Button, Set_Value = ((VCen+100)/200.)
        ENDIF
     END
     Widget_Window_1_Label.DeltaV: BEGIN
        Widget_Control, Event.ID, Get_Value = buttonValue, $
                        Get_UValue = buttonUValue
        Widget_Control, Event.ID, Set_Value = buttonUValue, $
                        Set_UValue = buttonValue
        Par_Flags.DeltaV = (Par_Flags.DeltaV+1) MOD 2
        Sensitize, Event.ID, Par_Flags.DeltaV
        IF Par_Flags.DeltaV THEN BEGIN
           Widget_Control, Widget_Window_1_Field.DeltaV, Get_Value = DeltaV
           Widget_Control, Widget_Window_1_Slider.Button, Set_Value = (DeltaV/200.)
        ENDIF
     END
     Widget_Window_1_Field.LMin:
     Widget_Window_1_Field.LMax:
     Widget_Window_1_Field.BMin:
     Widget_Window_1_Field.BMax:
     Widget_Window_1_Field.Center: 
     Widget_Window_1_Field.Vcen: 
     Widget_Window_1_Field.DeltaV:
     Widget_Window_1_Slider.Button:BEGIN
        IF Par_Flags.LMin THEN BEGIN
           Widget_Control, Widget_Window_1_Field.LMax, Get_Value = LMax
           Value = FIX(Event.Value*360) 
           IF  Value GT LMax THEN Value = LMax
           Widget_Control, Widget_Window_1_Field.LMin, Set_Value = Value
        ENDIF
        IF Par_Flags.LMax THEN BEGIN
           Widget_Control, Widget_Window_1_Field.LMin, Get_Value = LMin
           Value = FIX(Event.Value*360) 
           IF  Value LT LMin THEN Value = LMin
           Widget_Control, Widget_Window_1_Field.LMax, Set_Value = Value
        ENDIF
        IF Par_Flags.BMin THEN BEGIN
           Widget_Control, Widget_Window_1_Field.BMax, Get_Value = BMax
           Value = FIX(180*Event.Value-90) 
           IF  Value GT BMax THEN Value = BMax
           Widget_Control, Widget_Window_1_Field.BMin, Set_Value = Value
        ENDIF
        IF Par_Flags.BMax THEN BEGIN
           Widget_Control, Widget_Window_1_Field.BMin, Get_Value = BMin
           Value = FIX(180*Event.Value-90) 
           IF  Value LT BMin THEN Value = BMin
           Widget_Control, Widget_Window_1_Field.BMax, Set_Value = Value
        ENDIF
        IF Par_Flags.Center THEN BEGIN
           Value = FIX(Event.Value*360) 
           Widget_Control, Widget_Window_1_Field.Center, Set_Value = Value
        ENDIF
        IF Par_Flags.VCen THEN BEGIN
           Value = FIX(200*Event.Value-100) 
           Widget_Control, Widget_Window_1_Field.VCen, Set_Value = Value
        ENDIF
        IF Par_Flags.DeltaV THEN BEGIN
           Value = FIX(100*Event.Value) 
           Widget_Control, Widget_Window_1_Field.DeltaV, Set_Value = Value
        ENDIF
     END
     Widget_Window_1_Buttons.Use_Map:BEGIN
        Widget_Control, Event.ID, Get_UValue = buttonUValue
        CASE buttonUValue[Event.Index] OF
           'WHAM': BEGIN
              Widget_Control, Widget_Window_1_Buttons.Grab_Map, SENS = 0, $
                              SET_VALUE = 'Grab Map'
              
              Widget_Control, Widget_Bar_1.Load_Map, $
                              Sensitive = 1
              Par_Flags.Use_HI = 0
              Par_Flags.Use_CO = 0
              Par_Flags.Use_Hi_Res_CO = 0
           END
           'HI': BEGIN
              Widget_Control, Widget_Window_1_Buttons.Grab_Map, SENS = 1, $
                              SET_VALUE = 'Grab HI Map'
              Par_Flags.Use_HI = 1
              Par_Flags.Use_CO = 0
              Par_Flags.Use_Hi_Res_CO = 0
              Widget_Control, Widget_Bar_1.Load_Map, $
                              Sensitive = (Par_Flags.Use_HI+1) MOD 2
              Widget_Control, Widget_Bar_2.Label, Set_Value = ' Map Params for HI'
              
              Widget_Control, Options_Widget.Convert, Bad_ID = Bad_ID
              IF Bad_ID EQ 0 THEN BEGIN
                 Widget_Control, Options_Widget.Convert, Set_Value = 0
                 Options_Flags.Convert_NSS = 0 &  Options_Flags.Convert_ProcSpec = 0
              ENDIF
           END
           'CO': BEGIN
              Widget_Control, Widget_Window_1_Buttons.Grab_Map, SENS = 1, $
                              SET_VALUE = 'Grab CO Map'
              Par_Flags.Use_HI = 0
              Par_Flags.Use_CO = 1
              Par_Flags.Use_Hi_Res_CO = 0
              Widget_Control, Widget_Bar_1.Load_Map, $
                              Sensitive = (Par_Flags.Use_CO+1) MOD 2
              Widget_Control, Widget_Bar_2.Label, Set_Value = ' Map Params for CO'
              
              Widget_Control, Options_Widget.Convert, Bad_ID = Bad_ID
              IF Bad_ID EQ 0 THEN BEGIN
                 Widget_Control, Options_Widget.Convert, Set_Value = 0
                 Options_Flags.Convert_NSS = 0 &  Options_Flags.Convert_ProcSpec = 0
              ENDIF
           END
           'Hi_Res_CO': BEGIN
              Widget_Control, Widget_Window_1_Buttons.Grab_Map, SENS = 1, $
                              SET_VALUE = 'Grab Hi Res CO Map'
              Par_Flags.Use_HI = 0
              Par_Flags.Use_CO = 0
              Par_Flags.Use_Hi_Res_CO = 1
              Widget_Control, Widget_Bar_1.Load_Map, $
                              Sensitive = (Par_Flags.Use_Hi_Res_CO+1) MOD 2
              Widget_Control, Widget_Bar_2.Label, Set_Value = ' Map Params for Hi Res CO'
              Widget_Control, Options_Widget.Convert, Set_Value = 0
              Options_Flags.Convert_NSS = 0 &  Options_Flags.Convert_ProcSpec = 0
           END
        ENDCASE
     END
     Widget_Window_1_Buttons.Grab_Map:BEGIN
        Widget_Control, Widget_Window_1_Field.LMin, Get_Value = LMin
        Widget_Control, Widget_Window_1_Field.LMax, Get_Value = LMax
        Widget_Control, Widget_Window_1_Field.BMin, Get_Value = BMin
        Widget_Control, Widget_Window_1_Field.BMax, Get_Value = BMax
        Widget_Control, Widget_Window_1_Field.Center, Get_Value = Center
        IF NOT(KEYWORD_SET(V_Cen)) AND NOT(KEYWORD_SET(Delta_V)) THEN BEGIN
           Widget_Control, Widget_Window_1_Field.VCen, Get_Value = V_Cen
           Widget_Control, Widget_Window_1_Field.DeltaV, Get_Value = Delta_V
        ENDIF
        Vmin = FLOOR(V_Cen-(Delta_V)/2.)
        VMax = FLOOR(V_Cen+(Delta_V)/2.)
        IF Par_Flags.Use_HI THEN BEGIN
           IF VMin EQ VMax THEN BEGIN
              VMin = -500 &  VMax = 500
           ENDIF
           Map = EXTHI(LMin, LMax, BMin, BMax, vmin = vmin, vmax = vmax, /wham)
        ENDIF
        IF Par_Flags.Use_CO THEN BEGIN
           IF VMin EQ VMax THEN BEGIN
              VMin = -300 &  VMax = 300
           ENDIF
           Map = EXTCO(LMin, LMax, BMin, BMax, vmin = vmin, vmax = vmax, /wham)
        ENDIF
        IF Par_Flags.Use_Hi_Res_CO THEN BEGIN
           IF VMin EQ VMax THEN BEGIN
              VMin = -20 &  VMax = 180
           ENDIF
           RESTORE, '/d/wham2/madsen/InnerGalaxy/Hi_Res_CO_Survey/CO_Cube.dat'
        ENDIF
     END
     Widget_Window_1_Buttons.Ignore:BEGIN
        Widget_Control, Event.ID, Get_Value = buttonValue, $
                        Get_UValue = buttonUValue
        Widget_Control, Event.ID, Set_Value = buttonUValue, $
                        Set_UValue = buttonValue
        Par_Flags.Ignore_Center = (Par_Flags.Ignore_Center+1) MOD 2
        Widget_Control, Widget_Window_1_Label.Center, $
                        Sensitive = (Par_Flags.Ignore_Center+1) MOD 2
        Widget_Control, Widget_Window_1_Field.Center, $
                        Sensitive = (Par_Flags.Ignore_Center+1) MOD 2
     END
     Widget_Window_1_Buttons.Ra_Dec: BEGIN
        Widget_Control, Event.ID, Get_Value = buttonValue, $
                        Get_UValue = buttonUValue
        Widget_Control, Event.ID, Set_Value = buttonUValue, $
                        Set_UValue = buttonValue
        Par_Flags.Ra_Dec = (Par_Flags.Ra_Dec+1) MOD 2
     END
     Widget_Window_1_Buttons.Other:BEGIN
        Map_Options, Widget_Main
     END
     Widget_Bar_4.Zoom_In:BEGIN
        Widget_Control, Event.ID, Get_Value = buttonValue, $
                        Get_UValue = buttonUValue
        Widget_Control, Event.ID, Set_Value = buttonUValue, $
                        Set_UValue = buttonValue
        Map_Click_Flags.Zoom_In = (Map_Click_Flags.Zoom_In+1) MOD 2
        Widget_Control, Widget_Bar_4.Zoom_Out, $
                        Sensitive = (Map_Click_Flags.Zoom_In+1) MOD 2
        Widget_Control, Widget_Bar_4.Zoom_Factor, $
                        Sensitive = (Map_Click_Flags.Zoom_In+1) MOD 2
        Widget_Control, Widget_Bar_4.CurSpect, $
                        Sensitive = (Map_Click_Flags.Zoom_In+1) MOD 2
        Widget_Control, Widget_Bar_4.Add_Sub, $
                        Sensitive = (Map_Click_Flags.Zoom_In+1) MOD 2
     END
     Widget_Bar_4.Zoom_Out:BEGIN
        Widget_Control, Event.ID, Get_Value = buttonValue, $
                        Get_UValue = buttonUValue
        Widget_Control, Event.ID, Set_Value = buttonUValue, $
                        Set_UValue = buttonValue
        Map_Click_Flags.Zoom_Out = (Map_Click_Flags.Zoom_Out+1) MOD 2
        Widget_Control, Widget_Bar_4.Zoom_In, $
                        Sensitive = (Map_Click_Flags.Zoom_Out+1) MOD 2
        Widget_Control, Widget_Bar_4.CurSpect, $
                        Sensitive = (Map_Click_Flags.Zoom_Out+1) MOD 2
        Widget_Control, Widget_Bar_4.Add_Sub, $
                        Sensitive = (Map_Click_Flags.Zoom_Out+1) MOD 2
     END
     Widget_Bar_4.Zoom_Factor:
     Widget_Bar_4.CurSpect:BEGIN
        Widget_Control, Event.ID, Get_Value = buttonValue, $
                        Get_UValue = buttonUValue
        Widget_Control, Event.ID, Set_Value = buttonUValue, $
                        Set_UValue = buttonValue
        Map_Click_Flags.CurSpect = (Map_Click_Flags.CurSpect+1) MOD 2
        Widget_Control, Widget_Bar_4.Zoom_In, $
                        Sensitive = (Map_Click_Flags.CurSpect+1) MOD 2
        Widget_Control, Widget_Bar_4.Zoom_Out, $
                        Sensitive = (Map_Click_Flags.CurSpect+1) MOD 2
        Widget_Control, Widget_Bar_4.Zoom_Factor, $
                        Sensitive = (Map_Click_Flags.CurSpect+1) MOD 2
        Widget_Control, Widget_Bar_4.Add_Sub, $
                        Sensitive = (Map_Click_Flags.CurSpect+1) MOD 2
     END
     Widget_Bar_4.Add_Sub:BEGIN
        WSET, Widget_Window_1.WinNum
        Widget_Control, Event.ID, Get_Value = buttonValue, $
                        Get_UValue = buttonUValue
        Widget_Control, Event.ID, Set_Value = buttonUValue, $
                        Set_UValue = buttonValue
        Map_Click_Flags.Add_Sub = (Map_Click_Flags.Add_Sub+1) MOD 2
        Add_Indices = LONARR(N_Max_Manip) &  Add_Indices(*) = 99999
        Sub_Indices = LONARR(N_Max_Manip) &  Sub_Indices(*) = 99999
        Add_i = 0 & Sub_i = 0
        Plots_Flags.N_Plus = 0 & Plots_Flags.N_Minus = 0
        Widget_Control, Widget_Bar_4.Zoom_In, $
                        Sensitive = (Map_Click_Flags.Add_Sub+1) MOD 2
        Widget_Control, Widget_Bar_4.Zoom_Out, $
                        Sensitive = (Map_Click_Flags.Add_Sub+1) MOD 2
        Widget_Control, Widget_Bar_4.Zoom_Factor, $
                        Sensitive = (Map_Click_Flags.Add_Sub+1) MOD 2
        Widget_Control, Widget_Bar_4.CurSpect, $
                        Sensitive = (Map_Click_Flags.Add_Sub+1) MOD 2
        Widget_Control, Widget_Window_2.Add, $
           Sensitive = Map_Click_Flags.Add_Sub
        Widget_Control, Widget_Window_2.Subtract, $
           Sensitive = Map_Click_Flags.Add_Sub
        Widget_Control, Widget_Window_2.Plot, $
                        Sensitive = Map_Click_Flags.Add_Sub
        Widget_Control, Widget_Window_2.Load, $
                        Sensitive = Map_Click_Flags.Add_Sub
        Widget_Control, Widget_Window_2.Save, $
                        Sensitive = Map_Click_Flags.Add_Sub
        Widget_Control, Widget_Window_2.Reset, $
                        Sensitive = Map_Click_Flags.Add_Sub
     END
     Widget_Window_1.Window: BEGIN
        IF NOT Par_Flags.Use_Hi_Res_CO THEN $
           Rad = 0.5/(FLOAT(Par_Flags.Use_HI OR Par_Flags.Use_CO)+1.0) $
        ELSE Rad =  0.125
        IF Map_Click_Flags.Add_Sub AND (Manip_Flags.Add OR Manip_Flags.Subtract) THEN BEGIN
           RESTORE, '/d/wham/pro/sao.dat'  ;get SAO database
           IF (Event.Press+Event.Release LT 2) THEN BEGIN
              Add_Sub_Box(Event.Release, *) = $
                 Convert_Coord([Event.X, Event.Y], /Dev, /To_Data)
              IF Add_Sub_Box(Event.Release, 0) LE 0 THEN $
                 Add_Sub_Box(Event.Release, 0) = -Add_Sub_Box(Event.Release, 0) ELSE $
                 Add_Sub_Box(Event.Release, 0) = 360-Add_Sub_Box(Event.Release, 0)
              IF Event.Release THEN BEGIN
                 Lmin = MIN(Add_Sub_Box(*, 0))
                 Lmax = MAX(Add_Sub_Box(*, 0))
                 Bmin = MIN(Add_Sub_Box(*, 1))
                 Bmax = MAX(Add_Sub_Box(*, 1))
                 ;PRINT, Lmin, Lmax, Bmin, Bmax
                 ;; Get the indices within the box
                 Near_Inds = WHERE(Map.Glon GE LMin AND Map.Glon LE Lmax AND $
                              Map.Glat GE Bmin AND Map.Glat LE Bmax, Count)
                 ;PRINT, 'count', count
                 IF Count EQ 0 THEN Near_Inds = SpectNear(Map, Lmin, Bmin, Rad, Count)
                 
                 Good_Inds = LONARR(N_ELEMENTS(Near_Inds)) 
                 k = N_ELEMENTS(Good_Inds) 
                 IF Count GT 0 THEN BEGIN
                    ;; Remove 6th mag SAO Stars, only for WHAM Maps
                    ;; Also exclude stars only if No Stars flag is set
                    IF NOT(Par_Flags.Use_HI) AND NOT(Par_Flags.Use_CO) AND $
                       NOT(Par_Flags.Use_Hi_Res_CO) THEN BEGIN
                       
                       k = 0
                       FOR i = 0, Count-1 DO BEGIN
                          Star_Ind = WHERE(ABS(SAO6.Glon-Map[Near_Inds[i]].Glon) LE 0.5 AND $
                             ABS(SAO6.Glat-Map[Near_Inds[i]].Glat) LE 0.5, Star_Count)
                          IF Star_Count GT 0 AND $
                             (Options_Flags.NoStars_Fast OR Options_Flags.NoStars_Slow) THEN BEGIN
                             PRINT, 'Star Excluded at ', Map[Near_Inds[i]].Glon, $
                                Map[Near_Inds[i]].Glat
                          ENDIF ELSE BEGIN  ;; its a good coord, keep it
                             Good_Inds[k] = Near_Inds[i]
                             k = k+1
                          ENDELSE
                       ENDFOR  ;; Count
                    ENDIF ELSE Good_Inds = Near_Inds  ;; for HI or CO
                    IF k GT 0 THEN BEGIN  ;; still have some good ones left
                       Good_Inds = Good_Inds[0:k-1]  ;; Reduce map indices array
                       ;; Go through loop of good indices
                       ;; First, advance to the end of valid
                       ;;   Add/Sub_Indices numbers (from Load .rcd file!)
                       Temp = WHERE(Add_Indices NE 99999, Count)
                       Add_i = Count 
                       Temp = WHERE(Sub_Indices NE 99999, Count)
                       Sub_i = Count 
                       
                       FOR i = 0, N_ELEMENTS(Good_Inds)-1 DO BEGIN
                          ;; Put the indices in the right array
                          IF Manip_Flags.Add THEN BEGIN
                             PRINT, add_i
                             Add_Indices[Add_i] = Good_Inds[i]
                             Add_i =  Add_i + 1
                          ENDIF
                          IF Manip_Flags.Subtract THEN BEGIN
                             Sub_Indices[Sub_i] = Good_Inds[i]
                             Sub_i =  Sub_i + 1
                          ENDIF
                          ;; Record the plusses/minuses for plotting
                          Lon = Map[Good_Inds[i]].Glon
                          Lat = Map[Good_Inds[i]].Glat
                          PRINT, 'coords', Lon, Lat
                          IF Lon LE 360 AND Lon GE 180 THEN Lon = 360-Lon ELSE Lon = -Lon
                          IF Manip_Flags.Add THEN BEGIN
                             Plots_Flags.Plus_Lon[Plots_Flags.N_Plus] = Lon 
                             Plots_Flags.Plus_Lat[Plots_Flags.N_Plus] = Lat
                             Plots_Flags.N_Plus = Plots_Flags.N_Plus+1
                          ENDIF
                          IF Manip_Flags.Subtract THEN BEGIN
                             Plots_Flags.Minus_Lon[Plots_Flags.N_Minus] = Lon 
                             Plots_Flags.Minus_Lat[Plots_Flags.N_Minus] = Lat
                             Plots_Flags.N_Minus = Plots_Flags.N_Minus+1
                          ENDIF
                          PLOTS, [Lon-Rad, Lon+Rad], $
                             [Lat, Lat], /Data, THICK = 2
                          PLOTS, [100, 140], [20, 50], /Data, thick = 2
                          IF Manip_Flags.Add THEN PLOTS, [Lon, Lon], $
                             [Lat-Rad, Lat+Rad], /Data, THICK = 2
                          
                       ENDFOR ;; Good_Inds
                    ENDIF  ;; Good_Count > 0 (k)
                 ENDIF  ;; Count > 0
              ENDIF ;; Event.Release
           ENDIF ;; Mouse click
        ENDIF  ;; Map click
        
        IF Map_Click_Flags.Zoom_In THEN BEGIN
           IF (Event.Press+Event.Release LT 2) EQ 1 THEN BEGIN
              Zoom_Box(Event.Release, *) = $
                 Convert_Coord([Event.X, Event.Y], /Dev, /To_Data)
              IF Zoom_Box(Event.Release, 0) LE 0 THEN $
                 Zoom_Box(Event.Release, 0) = -Zoom_Box(Event.Release, 0) ELSE $
                 Zoom_Box(Event.Release, 0) = 360-Zoom_Box(Event.Release, 0)
              IF Event.Release THEN BEGIN
                 Widget_Control, Widget_Window_1_Field.LMin, Set_Value = MIN(Zoom_Box(*, 0))
                 Widget_Control, Widget_Window_1_Field.LMax, Set_Value = MAX(Zoom_Box(*, 0))
                 Widget_Control, Widget_Window_1_Field.BMin, Set_Value = MIN(Zoom_Box(*, 1))
                 Widget_Control, Widget_Window_1_Field.BMax, Set_Value = MAX(Zoom_Box(*, 1))
              ENDIF
           ENDIF
           IF Event.Release EQ 4 THEN Draw_Map
        ENDIF
        IF Map_Click_Flags.Zoom_Out THEN BEGIN
           IF Event.Release EQ 1 THEN BEGIN
              Widget_Control, Widget_Window_1_Field.LMin, Get_Value = LMin
              Widget_Control, Widget_Window_1_Field.LMax, Get_Value = LMax
              Widget_Control, Widget_Window_1_Field.BMin, Get_Value = BMin
              Widget_Control, Widget_Window_1_Field.BMax, Get_Value = BMax
              Widget_Control, Widget_Bar_4.Zoom_Factor, Get_Value = F_z  ;; Zoom factor
              Center = Convert_Coord([Event.X, Event.Y], /Dev, /To_Data)
              IF Center[0] LE 0 THEN Center[0] = -Center[0] ELSE $
                 Center[0] = 360-Center[0]
              Widget_Control, Widget_Window_1_Field.LMin, $
                              Set_Value = MAX([0, Center[0]-((LMax-LMin)/2.0)*F_z])
              Widget_Control, Widget_Window_1_Field.LMax, $
                              Set_Value = MIN([360, Center[0]+((LMax-LMin)/2.0)*F_z])
              Widget_Control, Widget_Window_1_Field.BMin, $
                              Set_Value = MAX([-90, Center[1]-((BMax-BMin)/2.0)*F_z])
              Widget_Control, Widget_Window_1_Field.BMax, $
                              Set_Value = MIN([90, Center[1]+((BMax-BMin)/2.0)*F_z])
           ENDIF
           IF Event.Release EQ 4 THEN Draw_Map
        ENDIF
        
        IF Map_Click_Flags.CurSpect THEN BEGIN
           IF (Event.Release GT 0) THEN BEGIN
              ;PRINT, Event.Release
              WSET, Widget_Window_1.WinNum
              CurSpect.X_Win_1 = !x
              CurSpect.Y_Win_1 = !y
              CurSpect_Pos = Convert_Coord([Event.X, Event.Y], /Dev, /To_Data)
              Lon = CurSpect_Pos[0]
              Lat = CurSpect_Pos[1]
              IF Lon LE 0 THEN  Lon = -Lon ELSE Lon = 360-Lon
              PRINT, "Lon = ", lon, "  Lat = ", lat, FORMAT = '(A,F6.2,A,F6.2)'
              IF Par_Flags.Ra_Dec THEN Euler, lon,  lat,  lon, lat,  1
              IF NOT Par_Flags.Use_Hi_Res_CO THEN $
                 Rad = 0.5/(FLOAT(Par_Flags.Use_HI OR Par_Flags.Use_CO)+1.0) $
              ELSE Rad =  0.125
              
              Near = SpectNear(Map, Lon, Lat, $
                               Rad, ncnt)
              IF ncnt EQ 0 THEN BEGIN
                 PRINT, 'No spectra within ', STRING(Rad, FORMAT = '(F4.2)'), $
                                                     ' degrees of click; click again'
              ENDIF ELSE BEGIN 
                 
                 
                 IF Par_Flags.Use_HI THEN BEGIN
                    title = STRING('HI toward (', Map[near[0]].glon, ',', $
                                   Map[near[0]].glat, ')', $
                                   format = '(A,F6.2,A,F6.2,A)')
                 ENDIF ELSE IF Par_Flags.Use_CO THEN BEGIN
                    title = STRING('CO toward (', Map[near[0]].glon, ',', $
                                   Map[near[0]].glat, ')', $
                                   format = '(A,F6.2,A,F6.2,A)')
                 ENDIF ELSE IF Par_Flags.Use_Hi_Res_CO THEN BEGIN
                    title = STRING('High Res CO toward (', Map[near[0]].glon, ',', $
                                   Map[near[0]].glat, ')', $
                                   format = '(A,F6.2,A,F6.2,A)')
                 ENDIF ELSE BEGIN
                    bpos = rstrpos(Map[near[0]].name, 'b')
                    shname = strmid(Map[near[0]].name, bpos + 1)
                    dotpos = rstrpos(shname, '.')
                    IF dotpos NE -1 THEN $
                       shname = strmid(shname, 0, dotpos)
                    title = STRING('Survey Block ' + shname + ' toward (', Map[near[0]].glon, $
                                   ',', Map[near[0]].glat, ')', $
                                   FORMAT = '(A,F6.2,A,F6.2,A)')
                 ENDELSE
                 
                 WSET, Widget_Window_2.WinNum
                 
                 IF Event.Release EQ 2 OR Event.Release EQ 4 THEN BEGIN
                    
                    !x = CurSpect.X_Win_2
                    !y = CurSpect.Y_Win_2
                    IF N_ELEMENTS(Map[Near[0]].Vel) EQ 133 THEN X = Map[near[0]].vel(29:*) ELSE $
                       X = Map[Near[0]].Vel
                    IF N_ELEMENTS(Map[Near[0]].Vel) EQ 133 THEN Y = Map[near[0]].data(29:*) ELSE $
                       Y =  Map[Near[0]].Data
                    IF Options_Flags.Convert_NSS THEN Y = Y / 684.1
                    IF Options_Flags.Convert_ProcSpec THEN Y =  Y / 22.8
                    Plot_Flags.Oplot_X[Plot_Flags.N_Oplot, 0:N_ELEMENTS(X)-1] = X
                    Plot_Flags.Oplot_Y[Plot_Flags.N_Oplot, 0:N_ELEMENTS(Y)-1]  = Y
                    Plot_Flags.Oplot_X[Plot_Flags.N_Oplot, N_ELEMENTS(X):999] = X[N_ELEMENTS(X)-1] 
                    Plot_Flags.Oplot_Y[Plot_Flags.N_Oplot, N_ELEMENTS(Y):999] = Y[N_ELEMENTS(Y)-1] 
                    Plot_Flags.Use_White[Plot_Flags.N_Oplot] = (Event.Release EQ 4) ? 1 : 0
                    Plot_Flags.N_Oplot = Plot_Flags.N_Oplot + 1
                    IF Plot_Flags.N_Oplot GE 100  THEN BEGIN
                       Dialog = Dialog_Message("Too Many Overplots; Start Over")
                       Plot_Flags.N_Oplot = 0
                    ENDIF
                    Plot_Spect

                 ENDIF ELSE IF (Event.Release EQ 1) THEN BEGIN 
                    IF Par_Flags.Use_HI OR Par_Flags.Use_CO THEN BEGIN
                       Widget_Control, Widget_Window_1_Field.VCen, Get_Value = V_Cen
                       Widget_Control, Widget_Window_1_Field.DeltaV, Get_Value = Delta_V
                       Vmin = FLOOR(V_Cen-(Delta_V)/2.)
                       VMax = FLOOR(V_Cen+(Delta_V)/2.)
                       IF VMin EQ VMax THEN BEGIN
                          VMin = MIN(Map[near[0]].vel)
                          VMax = MAX(Map[near[0]].vel)
                       ENDIF
                       X = Map[near[0]].Vel[WHERE( Map[near[0]].Vel GE VMin AND $
                                                   Map[near[0]].Vel LE VMax)]
                       Y = Map[near[0]].Data[WHERE( Map[near[0]].Vel GE VMin AND $
                                                    Map[near[0]].Vel LE VMax)]
                    ENDIF ELSE BEGIN
                       IF N_ELEMENTS(Map[Near[0]].Vel) EQ 133 THEN X = Map[near[0]].vel(29:*) ELSE $
                          X = Map[near[0]].Vel
                       IF N_ELEMENTS(Map[Near[0]].Vel) EQ 133 THEN Y = Map[near[0]].data(29:*) ELSE $ 
                          Y =  Map[near[0]].Data
                    ENDELSE
                    IF Par_Flags.Use_CO OR Par_Flags.Use_HI THEN BEGIN
                       ytitle = 'T!DA!N [K]'
                    ENDIF ELSE BEGIN
                       ytitle = "I [ADUs]"
                       IF Options_Flags.Convert_NSS THEN BEGIN
                          Y =  Y / 684.1
                          ytitle = 'I!DH!7a!3!N [R (km s!U-1!N)!U-1!N]'
                       ENDIF
                       IF Options_Flags.Convert_ProcSpec THEN BEGIN
                          Y =  Y / 22.8
                          ytitle = 'I!DH!7a!3!N [R (km s!U-1!N)!U-1!N]'
                       ENDIF
                    ENDELSE
                    Plot_Flags.X[0:N_ELEMENTS(X)-1] = X
                    Plot_Flags.Y[0:N_ELEMENTS(Y)-1]  = Y
                    Plot_Flags.X[N_ELEMENTS(X):999] = X[N_ELEMENTS(X)-1] 
                    Plot_Flags.Y[N_ELEMENTS(Y):999] = Y[N_ELEMENTS(Y)-1] 
                    Plot_Flags.Title = title
                    Plot_Flags.Xtitle = 'V!DLSR!N [km s!U-1!N]'
                    Plot_Flags.Ytitle = Ytitle
                    Plot_Flags.N_Oplot = 0
                    Plot_Spect
                 ENDIF
                 CurSpect.X_Win_2 = !x
                 CurSpect.Y_Win_2 = !y
                 WSET, Widget_Window_1.WinNum
                 !x = CurSpect.X_Win_1
                 !y = CurSpect.Y_Win_1
              ENDELSE
           ENDIF
        ENDIF
     END
     Widget_Window_2.Add:BEGIN
        Widget_Control, Event.ID, Get_Value = buttonValue, $
                        Get_UValue = buttonUValue
        Widget_Control, Event.ID, Set_Value = buttonUValue, $
                        Set_UValue = buttonValue
        Manip_Flags.Add = (Manip_Flags.Add+1) MOD 2
        Widget_Control, Widget_Window_2.Subtract, $
                        Sensitive = (Manip_Flags.Add+1) MOD 2
        Widget_Control, Widget_Window_2.Plot, $
                        Sensitive = (Manip_Flags.Add+1) MOD 2
        Widget_Control, Widget_Window_2.Load, $
                        Sensitive = (Manip_Flags.Add+1) MOD 2
        Widget_Control, Widget_Window_2.Save, $
                        Sensitive = (Manip_Flags.Add + 1) MOD 2
        Widget_Control, Widget_Window_2.Reset, $
                        Sensitive = (Manip_Flags.Add+1) MOD 2
     END
     Widget_Window_2.Subtract:BEGIN
        Widget_Control, Event.ID, Get_Value = buttonValue, $
                        Get_UValue = buttonUValue
        Widget_Control, Event.ID, Set_Value = buttonUValue, $
                        Set_UValue = buttonValue
        Manip_Flags.Subtract = (Manip_Flags.Subtract+1) MOD 2
        Widget_Control, Widget_Window_2.Add, $
                        Sensitive = (Manip_Flags.Subtract + 1) MOD 2
        Widget_Control, Widget_Window_2.Plot, $
                        Sensitive = (Manip_Flags.Subtract + 1) MOD 2
        Widget_Control, Widget_Window_2.Load, $
                        Sensitive = (Manip_Flags.Subtract + 1) MOD 2
        Widget_Control, Widget_Window_2.Save, $
                        Sensitive = (Manip_Flags.Subtract + 1) MOD 2
        Widget_Control, Widget_Window_2.Reset, $
                        Sensitive = (Manip_Flags.Subtract + 1) MOD 2
     END
     Widget_Window_2.Plot:BEGIN
        Manip_Flags.Save = 0
        Plot_Manip_Spectra
     END
     Widget_Window_2.Load:BEGIN
        Manip_Flags.Save = 0
        Rcd_FileName = DIALOG_PICKFILE(Dialog_Parent = Widget_Window_2.Load, $
                                       Title = 'Pick .rcd File', $
                                       File = '', $
                                       Filter = '*.rcd', $
                                       /MUST_EXIST, $
                                       PATH = Par_Flags.Rcd_Path, $
                                       Get_Path = Temp)
        Widget_Control, Widget_Window_2.Load, /HOURGLASS
        IF Rcd_FileName NE '' AND KEYWORD_SET(Map) THEN BEGIN
           Par_Flags.Rcd_Path = Temp
           line = ''
           OPENR, unit, Rcd_FileName, /Get_Lun
           READF, unit, line ;; first line
           Add = 1
           i_add = 0 &  i_sub =  0
           Add_Indices[*] = 99999 &  Sub_Indices[*] = 99999  ;; reset indices
           Plots_Flags.N_Plus =  0 &  Plots_Flags.N_Minus = 0  ;; reset plotting
           WHILE NOT(EOF(unit)) DO BEGIN
              READF, unit, line
              IF NOT(Add) THEN BEGIN
                 IF LONG(line) NE 99999 THEN BEGIN
                    Sub_Indices[i_sub] = LONG(line)
                    i_sub = i_sub + 1
                 ENDIF
              ENDIF
              IF Add AND STRMID(line, 0, 3) NE 'Map' THEN BEGIN
                 IF LONG(line) NE 99999 THEN BEGIN
                    Add_Indices[i_add] = LONG(line)
                    i_add = i_add + 1
                 ENDIF
              ENDIF ELSE Add = 0
           ENDWHILE
           CLOSE, unit
           FREE_LUN, unit
           IF i_add EQ 0 AND i_sub EQ 0 THEN BEGIN
              Temp = DIALOG_MESSAGE(".rcd File has no valid entries!", title = "Warning")
              GOTO, Label1  ;;yuck!
           ENDIF
           ;; Draw the map around the coords in the .rcd file
           IF i_add GT 0 AND i_sub GT 0 THEN BEGIN
              Lon_Arr = [Map[Add_Indices[0:i_add-1]].Glon, $
                         Map[Sub_Indices[0:i_sub-1]].Glon] 
              Lat_Arr = [Map[Add_Indices[0:i_add-1]].Glat, $
                         Map[Sub_Indices[0:i_sub-1]].Glat] 
           ENDIF
           IF i_add GT 0 AND i_sub LE 0 THEN BEGIN
              Lon_Arr = [Map[Add_Indices[0:i_add-1]].Glon]
              Lat_Arr = [Map[Add_Indices[0:i_add-1]].Glat]
           ENDIF
           IF i_add LE 0 AND i_sub GT 0 THEN BEGIN
              Lon_Arr = [Map[Sub_Indices[0:i_sub-1]].Glon]
              Lat_Arr = [Map[Sub_Indices[0:i_sub-1]].Glat]
           ENDIF
           Widget_Control, Widget_Window_1_Field.LMin, $
              Set_Value = MIN(Lon_Arr)-5
           Widget_Control, Widget_Window_1_Field.LMax, $
              Set_Value = MAX(Lon_Arr)+5
           Widget_Control, Widget_Window_1_Field.BMin, $
              Set_Value = MIN(Lat_Arr)-5
           Widget_Control, Widget_Window_1_Field.BMax, $
              Set_Value = MAX(Lat_Arr)+5
           Draw_Map
           IF NOT Par_Flags.Use_Hi_Res_CO THEN $
              Rad = 0.5/(FLOAT(Par_Flags.Use_HI OR Par_Flags.Use_CO)+1.0) $
           ELSE Rad =  0.125
           FOR i = 0, i_add-1 DO BEGIN
              Lon = Map[Add_Indices[i]].Glon
              Lat = Map[Add_Indices[i]].Glat
              IF Lon LE 360 AND Lon GE 180 THEN Lon = 360-Lon ELSE Lon = -Lon
              PLOTS, [Lon-Rad, Lon+Rad], [Lat, Lat], /Data, THICK = 2
              PLOTS, [Lon, Lon], [Lat-Rad, Lat+Rad], /Data, THICK = 2
              Plots_Flags.Plus_Lon[i] = Lon
              Plots_Flags.Plus_Lat[i] = Lat
              Plots_Flags.N_Plus =  Plots_Flags.N_Plus + 1
           ENDFOR
           FOR i = 0, i_sub-1 DO BEGIN
              Lon = Map[Sub_Indices[i]].Glon
              Lat = Map[Sub_Indices[i]].Glat
              IF Lon LE 360 AND Lon GE 180 THEN Lon = 360-Lon ELSE Lon = -Lon
              PLOTS, [Lon-Rad, Lon+Rad], [Lat, Lat], /Data, THICK = 2
              Plots_Flags.Minus_Lon[i] = Lon
              Plots_Flags.Minus_Lat[i] = Lat
              Plots_Flags.N_Minus =  Plots_Flags.N_Minus + 1
           ENDFOR
           
        ENDIF ELSE BEGIN
           IF Rcd_FileName NE '' THEN Temp = $
              Dialog_Message("Error: No Map Loaded!!", $
                             title = "Warning")
        ENDELSE
        Label1:
     END
     Widget_Window_2.X1: BEGIN
        Plot_Flags.X_Min = Event.Value
        CurSpect.X_Win_1 = !X
        CurSpect.Y_Win_1 = !Y
        WSET, Widget_Window_2.WinNum
        Plot_Spect
        WSET, Widget_Window_1.WinNum
        !x = CurSpect.X_Win_1
        !y = CurSpect.Y_Win_1
     END
     Widget_Window_2.X2: BEGIN
        Plot_Flags.X_Max = Event.Value
        CurSpect.X_Win_1 = !X
        CurSpect.Y_Win_1 = !Y
        WSET, Widget_Window_2.WinNum
        Plot_Spect
        WSET, Widget_Window_1.WinNum
        !x = CurSpect.X_Win_1
        !y = CurSpect.Y_Win_1
     END
     Widget_Window_2.Y1: BEGIN
        Plot_Flags.Y_Min = Event.Value
        CurSpect.X_Win_1 = !X
        CurSpect.Y_Win_1 = !Y
        WSET, Widget_Window_2.WinNum
        Plot_Spect
        WSET, Widget_Window_1.WinNum
        !x = CurSpect.X_Win_1
        !y = CurSpect.Y_Win_1
     END
     Widget_Window_2.Y2: BEGIN
        Plot_Flags.Y_Max = Event.Value
        CurSpect.X_Win_1 = !X
        CurSpect.Y_Win_1 = !Y
        WSET, Widget_Window_2.WinNum
        Plot_Spect
        WSET, Widget_Window_1.WinNum
        !x = CurSpect.X_Win_1
        !y = CurSpect.Y_Win_1
     END
     Widget_Window_2.Save: BEGIN
        Manip_Flags.Save_File_Base = DIALOG_PICKFILE(Dialog_Parent = Event.ID, $
                                                     Title = 'Pick File BASE', $
                                                     File = Manip_Flags.Save_File_Base, $
                                                     PATH = Manip_Flags.Path, $
                                                     Get_Path = Temp)
        Manip_Flags.Path = Temp
        Manip_Flags.Save = 1
        Plot_Manip_Spectra
        Manip_Flags.Save = 0
     END
     Widget_Window_2.Reset:BEGIN
        Add_Indices = LONARR(N_Max_Manip) &  Add_Indices(*) = 99999
        Sub_Indices = LONARR(N_Max_Manip) &  Sub_Indices(*) = 99999
        Add_i = 0 & Sub_i = 0
        Plots_Flags.N_Plus = 0 & Plots_Flags.N_Minus = 0
        Draw_Map
     END
     Widget_Window_2.Default: BEGIN
        Widget_Control, Event.ID, Get_Value = buttonValue, $
           Get_UValue = buttonUValue
        Widget_Control, Event.ID, Set_Value = buttonUValue, $
           Set_UValue = buttonValue
        Display_Flags.Default = (Display_Flags.Default+1) MOD 2
        Widget_Control, Widget_Window_2.ForPapers, $
                        Sensitive = (Display_Flags.Default+1) MOD 2
     END
     Widget_Window_2.ForPapers: BEGIN
        Widget_Control, Event.ID, Get_Value = buttonValue, $
           Get_UValue = buttonUValue
        Widget_Control, Event.ID, Set_Value = buttonUValue, $
           Set_UValue = buttonValue
        Display_Flags.ForPapers = (Display_Flags.ForPapers+1) MOD 2
        Widget_Control, Widget_Window_2.Default, $
                        Sensitive = (Display_Flags.ForPapers+1) MOD 2
        PRINT, Display_Flags.ForPapers
     END
  ENDCASE

END

FUNCTION Get_Map_Name, Vars
  
  COMMON WHAMVIEW_Widgets

  COMMON Map_Block, Map_Index
  Reduced_Vars = STRARR(N_ELEMENTS(Vars)-3)
  k = 0
  FOR i = 0, N_ELEMENTS(Vars)-1 DO BEGIN
     IF Vars[i] NE 'MAP_FILENAME' AND Vars[i] NE 'VARS' AND $
        Vars[i] NE 'MAP_INDEX' THEN BEGIN
        Reduced_Vars[k] = Vars[i]
        k = k+1
     ENDIF
  ENDFOR
  
  Main = Widget_Base(title = 'Pick a Map', Group_Leader = Widget_Main, /Modal)
  Menu = CW_BGroup(Main, Reduced_Vars, /Exclusive, /Col, $
                   XSize = 200)
  Widget_Control, Main, /REALIZE
  XManager, 'Get_Map_Name', Main, Event_Handler = 'Get_Map_Name_Index'
  RETURN, Reduced_Vars[Map_Index]
  
END

PRO Get_Map_Name_Index, Event
  
  COMMON Map_Block
  
  Widget_Control, Event.ID, Get_Value = Map_Index
  Widget_Control, Event.Top, /destroy
  
END

FUNCTION Get_Map, Map_Filename
  
  COMMON Map_Block, Map_Index

  Vars = ROUTINE_NAMES(variables = 0)
  RESTORE, Map_Filename
  
  Vars = ROUTINE_NAMES(variables = 0)
  RETURN, ROUTINE_NAMES(Get_Map_Name(Vars), FETCH = 0)

END

PRO Load_Map
  
  COMMON WHAMVIEW_Widgets

  COMMON WHAMVIEW_Data

  Map_FileName = DIALOG_PICKFILE(Dialog_Parent = Widget_Bar_1.Load_Map, $
                                 Title = 'Pick WHAM Map File', $
                                 File = 'whamnss.dat', $
                                 Filter = '*.dat', $
                                 /MUST_EXIST, $
                                 PATH = Par_Flags.Map_Path, $
                                 Get_Path = Temp)
  Widget_Control, Widget_Bar_1.Load_Map, /HOURGLASS
  IF Map_FileName NE '' THEN BEGIN
     Par_Flags.Map_Path = Temp
     Map = Get_Map(Map_Filename)
  ENDIF
END



PRO Map_Options,  Parent

  COMMON WHAMVIEW_Widgets

  COMMON WHAMVIEW_Data

  Options_Widget = {Main:0L, Lin_Log:0L, Convert:0L, Other:0L, $
                    V_Start:0L, V_End:0L, Step:0L, Make_Movie:0L, $
                    CT:0L, Quit:0L}

  Options_Widget.Main = Widget_Base(title = 'Map Options', /Col, Group_Leader = Parent)

  Lin_Log_Names = ["Linear", "Log", "Histogram Equalized"]
  Convert_Names = ["No Conversion", "Divide by 684.1 (NSS)", "Divide by 22.8 (PROCSPEC)"]
  
  Other_Names = ["Smoothed", "Fill Boundary", "No Stars (Fast,Whole Map Only)", $
                 "No Stars (Slow, Any Map)", "No Grid", $
                 "Color Bar", "Fix Blank Spots", $
                 "Track/Remove Negative Ptgs.", "Yellow Background"]
  
  Label = Widget_Label(Options_Widget.Main, Value = 'OTHER MAP OPTIONS')
  Label = Widget_Label(Options_Widget.Main, Value = ' ')
  
  Label = Widget_Label(Options_Widget.Main, Value = 'Scaling:', /ALIGN_LEFT)
  Options_Widget.Lin_Log = CW_BGroup(Options_Widget.Main, Lin_Log_Names, /Col, $
                                     /Exclusive, XSize = 200, Frame = 2)
  
  Label = Widget_Label(Options_Widget.Main, Value = ' ')
  Label = Widget_Label(Options_Widget.Main, Value = 'WHAM Data Conversion: ', /ALIGN_LEFT)
  Options_Widget.Convert = CW_BGroup(Options_Widget.Main, Convert_Names, /Col, $
                                     /Exclusive, XSize = 200, Frame = 2)
  
  Label = Widget_Label(Options_Widget.Main, Value = ' ')
  Label = Widget_Label(Options_Widget.Main, Value = 'Display:', /ALIGN_LEFT)
  Options_Widget.Other = CW_BGroup(Options_Widget.Main, Other_Names, /Col, $
                                   /Nonexclusive, XSize = 200, Frame = 2, $
                                   Set_Value = [Options_Flags.Smooth, $
                                                Options_Flags.Fill, $
                                                Options_Flags.NoStars_Fast, $
                                                Options_Flags.NoStars_Slow, $
                                                Options_Flags.NoGrid, $
                                                Options_Flags.Color_Bar, $
                                                Options_Flags.Fix_Missing, $
                                                Options_Flags.Track_Negs])
  
  
  Label = Widget_Label(Options_Widget.Main, Value = ' ')
  Label = Widget_Label(Options_Widget.Main, Value = 'Color Tables: ', /ALIGN_LEFT)
  Options_Widget.CT = CW_BGroup(Options_Widget.Main, ["Change Color Table", $
                                                      "Grab and Save Color Table", $
                                                      "Restore Color Table"], /Col, $
                                XSize = 200, Frame = 2)

  Label = Widget_Label(Options_Widget.Main, Value = ' ')
  Label = Widget_Label(Options_Widget.Main, Value = 'Movie Making:', /ALIGN_LEFT)
  Options_Widget_Movie_Base = Widget_Base(Options_Widget.Main, Frame = 2, /Col)
  Options_Widget.V_Start = CW_Field(Options_Widget_Movie_Base, /All_Events, $
                                    /Integer, Value = -50, XSize = 4, $
                                    Title = 'Starting Velocity: [km/s] ')
  Options_Widget.V_End = CW_Field(Options_Widget_Movie_Base, /All_Events, $
                                  /Integer, Value = 50, XSize = 4, $
                                  Title = 'Ending Velocity: [km/s]   ')
  Options_Widget.Step = CW_Field(Options_Widget_Movie_Base, /All_Events, $
                                 /Integer, Value = 10, XSize = 4, $
                                 Title = 'Step Size:  [km/s]        ')
  Options_Widget.Make_Movie = Widget_Button(Options_Widget_Movie_Base, $
                                            Value = 'Make the Movie')

  Label = Widget_Label(Options_Widget.Main, Value = ' ')
  Options_Widget.Quit = Widget_Button(Options_Widget.Main, Value = 'Quit')
  
  Widget_Control, Options_Widget.Main, /Realize
  IF Options_Flags.Linear THEN Widget_Control, Options_Widget.Lin_Log, Set_Value = 0
  IF Options_Flags.Log THEN Widget_Control, Options_Widget.Lin_Log, Set_Value = 1
  IF NOT(Options_Flags.Linear) AND NOT(Options_Flags.Log) THEN $
     Widget_Control, Options_Widget.Lin_Log, Set_Value = 2
  IF Options_Flags.Convert_NSS THEN Widget_Control, Options_Widget.Convert, Set_Value = 1 ELSE $
     IF Options_Flags.Convert_ProcSpec THEN Widget_Control, Options_Widget.Convert, Set_Value = 2 $
     ELSE Widget_Control, Options_Widget.Convert, Set_Value = 0
  
  XManager, 'Map_Options', Options_Widget.Main

END

PRO Map_Options_Event, Event

  COMMON WHAMVIEW_Widgets

  COMMON WHAMVIEW_Data

  CASE Event.ID OF
     Options_Widget.Lin_Log:BEGIN
        CASE Event.Value OF 
           0: Options_Flags.Linear = Event.Select
           1: Options_Flags.Log = Event.Select
           2: BEGIN
              Options_Flags.Linear = 0
              Options_Flags.Log = 0
           END
        ENDCASE
     END
     
     Options_Widget.Convert:BEGIN
        CASE Event.Value OF 
           0: BEGIN
              Options_Flags.Convert_NSS = 0
              Options_Flags.Convert_ProcSpec = 0
           END
           1: Options_Flags.Convert_NSS = Event.Select
           2: Options_Flags.Convert_ProcSpec = Event.Select
        ENDCASE
     END
     
     Options_Widget.Other:BEGIN
        CASE Event.Value OF
           0: Options_Flags.Smooth = Event.Select
           1: Options_Flags.Fill = Event.Select
           2: Options_Flags.NoStars_Fast = Event.Select
           3: Options_Flags.NoStars_Slow = Event.Select
           4: Options_Flags.NoGrid = Event.Select
           5: Options_Flags.Color_Bar = Event.Select
           6: Options_Flags.Fix_Missing = Event.Select
           7: Options_Flags.Track_Negs = Event.Select
           8: Options_Flags.Yellow_Bkg = Event.Select
        ENDCASE
     END
     Options_Widget.V_Start:
     Options_Widget.V_End:
     Options_Widget.Step:
     Options_Widget.Make_Movie:BEGIN
        Widget_Control, Options_Widget.V_Start, Get_Value = V_Start
        Widget_Control, Options_Widget.V_End, Get_Value = V_End
        Widget_Control, Options_Widget.Step, Get_Value = Step_Size
        Widget_Control, Widget_Window_1_Field.DeltaV, Get_Value = Delta_V
        N_Frames = (V_End-V_Start)/Step_Size
        Window, 10, XSize = 640, YSize = 480, /pixmap
        XINTERANIMATE, Set = [640, 480, N_Frames], $
                       /Showload, /Track, Title = 'WHAM Movie', Group = Event.Top
        FOR i = 0, N_Frames-1 DO BEGIN
           Draw_Map, V_Cen = V_Start+i*Step_Size, Delta_V = Delta_V, Window = 10
           XINTERANIMATE, Frame = i, Window = 10
        ENDFOR
        XINTERANIMATE, 50
     END
     Options_Widget.CT: BEGIN
        CASE Event.Value OF
           0: XLOADCT, /use_current, Group = Options_Widget.Main
           1: BEGIN
              TVLCT, R, G, B, /Get
              CT_FileName = DIALOG_PICKFILE(Dialog_Parent = Event.ID, $
                                            Title = 'Pick Color Table File', $
                                            File = 'temp.ctb', $
                                            Filter = '*.ctb', $
                                            PATH = '/d/wham2/madsen/Survey/' + $ $
                                            'Final_Pics/Color_Tables')
              SAVE, R, G, B, File = CT_FileName
           END
           2: BEGIN 
              CT_FileName = DIALOG_PICKFILE(Dialog_Parent = Event.ID, $
                                            Title = 'Pick Color Table File', $
                                            File = 'temp.ctb', $
                                            Filter = '*.ctb', $
                                            /MUST_EXIST, $
                                            PATH = '/d/wham2/madsen/Survey/' + $
                                            'Final_Pics/Color_Tables')
              RESTORE, CT_Filename
              TVLCT, R, G, B
           END
        ENDCASE
     END
     Options_Widget.Quit:Widget_Control, Event.Top, /Destroy
  ENDCASE
END

PRO Get_Printer_Name
  
  COMMON WHAMVIEW_Widgets

  COMMON Print_Widgets, Print_Widget
  
  COMMON WHAMVIEW_Data

  Print_Widget = { Main:0L, Label:0L, Field:0L, Print:0L, PrintToFile:0L, $
                   Cancel:0L }

  Print_Widget.Main = Widget_Base(title = "Printing", /Col,  /Modal,  $
                                  Group_Leader = Widget_Bar_1.Base)
  Row_1 = Widget_Base(Print_Widget.Main, /Row)
  Print_Widget.Label = Widget_Label(Row_1, Value = 'Name of Printer: ')
  Print_Widget.Field = CW_Field(Row_1, /All_Events, /String, Title = '', $
                                Value = Print_Flags.Name, XSize = 12)
  Row_2 = Widget_Base(Print_Widget.Main, /Col)
  Print_Widget.Print = Widget_Button(Row_2, Value = 'Print', XSize = 215)
  Print_Widget.PrintToFile = Widget_Button(Row_2, Value = 'Print to File', XSize = 215)
  Print_Widget.Cancel = Widget_Button(Row_2, Value = 'Cancel', XSize = 215)

  Widget_Control, Print_Widget.Main, /Realize
  XManager, 'Get_Printer_Name', Print_Widget.Main

END

PRO Get_Printer_Name_Event, Event
  
  COMMON Print_Widgets, Print_Widget
  
  COMMON WHAMVIEW_Data

  CASE Event.ID OF
     Print_Widget.Field: BEGIN
        Widget_Control, Event.ID, Get_Value = Temp
        Print_Flags.Name = Temp
     END
     Print_Widget.Print:BEGIN
        Widget_Control, Print_Widget.Field, Get_Value = Temp
        Print_Flags.Name = Temp
        Print_Flags.To_Printer = 1
        Widget_Control, Event.Top, /Destroy
     END
     Print_Widget.PrintToFile: BEGIN
        Print_Flags.PS_Name = DIALOG_PICKFILE(Dialog_Parent = Event.ID, $
                                              Title = 'Name of PostScript File', $
                                              File = Print_Flags.PS_Name, $
                                              Filter = '*.ps', $
                                              Display_Name = 'Pick a File', $
                                              PATH = Print_Flags.Path, $
                                              Get_Path = Temp)
        Print_Flags.Path = Temp   ;; Remembers most recent path

        ;; Check for valid filename/directory
        OPENW, Unit, Print_Flags.PS_Name, ERROR = No_File, /GET_LUN

        IF No_File NE 0 THEN BEGIN
           Print_Flags.To_File = 0
           Print_Flags.To_Printer = 0
           Print_Flags.Cancel = 1
           IF Print_Flags.PS_Name THEN BEGIN
              Error = DIALOG_MESSAGE("Invalid Filename or Directory: " + $
                                     Print_Flags.PS_Name, $
                                     DIALOG_PARENT = Event.ID, $
                                     TITLE = "Error!", $
                                     /ERROR)
           ENDIF
        ENDIF ELSE BEGIN
           CLOSE, Unit
           FREE_LUN, Unit
           Print_Flags.To_File = 1
           Widget_Control, Event.Top, /Destroy
        ENDELSE
     END
     Print_Widget.Cancel:BEGIN
        Print_Flags.To_Printer = 0
        Print_Flags.To_File = 0
        Print_Flags.Cancel = 1
        Widget_Control, Event.Top, /Destroy
     END
  ENDCASE

END


PRO Plot_Manip_Spectra

  COMMON WHAMVIEW_Widgets
  
  COMMON WHAMVIEW_Data

  WSET, Widget_Window_1.WinNum
  !P.POSITION = [.1, .1, .9, .9]
  CurSpect.X_Win_1 = !x
  CurSpect.Y_Win_1 = !y
  
  IF Par_Flags.Use_HI OR Par_Flags.Use_CO OR Par_Flags.Use_Hi_Res_CO THEN short_vel = 0 ELSE $
     short_vel = 1

  Plot_Add_Indices = Add_Indices[UNIQ(Add_Indices)]
  Good_Indices = WHERE(Plot_Add_Indices NE 99999, Add_Count)
  IF Add_Count EQ 0 THEN BEGIN
     Crap = Dialog_Message("Warning: No Spectra to Add", $
                           title = "Warning")
     Spect_Add = Map[0]
     Spect_Add.Data[*] = 0.0
  ENDIF ELSE BEGIN
     Plot_Add_Indices = Plot_Add_Indices[Good_Indices]
     IF N_ELEMENTS(Plot_Add_Indices) GT 1 THEN BEGIN
        Spect_Add = Greg_SpArith(Map[Plot_Add_Indices], /average, short_vel = short_vel) 
        ;PRINT, Map[Plot_Add_Indices[0]].Vel
     ENDIF ELSE Spect_Add = Map[Plot_Add_Indices]
  ENDELSE
  ;PRINT, Spect_Add.Vel
  
  Plot_Sub_Indices = Sub_Indices[UNIQ(Sub_Indices)]
  Good_Indices = WHERE(Plot_Sub_Indices NE 99999, Sub_Count)
  IF Sub_Count EQ 0 THEN BEGIN
     Crap = Dialog_Message("Warning: No Spectra to Subtract", $
                           title = "Warning")
     
     Spect_Sub = Spect_Add
     Spect_Sub.Data[*] = 0.0
  ENDIF ELSE BEGIN
     Plot_Sub_Indices = Plot_Sub_Indices[Good_Indices]
     IF N_ELEMENTS(Plot_Sub_Indices) GT 1 THEN BEGIN
        Spect_Sub = Greg_SpArith(Map[Plot_Sub_Indices], /average, short_vel = short_vel) 
     ENDIF ELSE Spect_Sub = Map[Plot_Sub_Indices]
  ENDELSE
  
;;  PRINT, "Adding: "
;;  FOR i = 0, N_ELEMENTS(Plot_Add_Indices)-1 DO BEGIN
;;     PRINT, '  i = ' + STRTRIM(Plot_Add_Indices(i), 2) + $
;;            STRING(' toward (', Map[Plot_Add_Indices[i]].glon, ',', $
;;                   Map[Plot_Add_Indices[i]].glat, ')', $
;;                   format = '(A,F6.2,A,F6.2,A)')
;;  ENDFOR
  
;;  PRINT, "Subtracting: "
;;  FOR i = 0, N_ELEMENTS(Plot_Sub_Indices)-1 DO BEGIN
;;     PRINT, '  i = ' + STRTRIM(Plot_Sub_Indices(i), 2)+ $
;;            STRING(' toward (', Map[Plot_Sub_Indices[i]].glon, ',', $
;;                   Map[Plot_Sub_Indices[i]].glat, ')', $
;;                   format = '(A,F6.2,A,F6.2,A)') 
;;  ENDFOR
  
  
;  WSET, Widget_Window_2.WinNum
;  PLOT, Map[Plot_Add_Indices[0]].Vel, Map[Plot_Add_Indices[0]].Data, xrange = [-200, 200]
;  FOR i = 1, N_ELEMENTS(Plot_Add_Indices)-1 DO BEGIN
;     OPLOT, Map[Plot_Add_Indices[i]].Vel, Map[Plot_Add_Indices[i]].Data
;  ENDFOR
;  FOR i = 0, N_ELEMENTS(Plot_Sub_Indices)-1 DO BEGIN
;     OPLOT, Map[Plot_Sub_Indices[i]].Vel, Map[Plot_Sub_Indices[i]].Data, linestyle = 1
;  ENDFOR
;  WAIT, 2
  
;  PLOT, Spect_Add.Vel, Spect_Add.Data, xrange = [-200, 200]
;  OPLOT, Spect_Sub.Vel, Spect_Sub.Data, linestyle = 1
;  WAIT, 2
  
  Final_Spect = Greg_SpArith(Spect_Add, Spect_Sub, /subtract, short_vel = short_vel)
;  PRINT, Final_Spect.Vel
  
  IF NOT(Manip_Flags.Save) THEN BEGIN
     WSET, Widget_Window_2.WinNum
     IF Print_Flags.To_Printer OR Print_Flags.To_File THEN BEGIN
        Set_Plot, 'ps'
        IF Print_Flags.To_Printer THEN Device, /Land, File = 'idl.ps'
        IF Print_Flags.To_File THEN Device,  /Land, File = Print_Flags.PS_Name
     ENDIF
     IF NOT(Print_Flags.Cancel) THEN BEGIN
        Plot_Flags.X = Final_Spect.Vel
        Plot_Flags.Y = Final_Spect.Data
        Plot_Flags.X[N_ELEMENTS(Final_Spect.Vel):999] = $
           Final_Spect.Vel[N_ELEMENTS(Final_Spect.Vel)-1]
        Plot_Flags.Y[N_ELEMENTS(Final_Spect.Data):999] = $
           Final_Spect.Data[N_ELEMENTS(Final_Spect.Data)-1]
        
        Plot_Flags.Xtitle = 'V!DLSR!N [km s!U-1!N]'  
        Plot_Flags.Ytitle = "I [ADUs]"
        IF Options_Flags.Convert_NSS THEN BEGIN
           Plot_Flags.Y =  Plot_Flags.Y / 684.1
           Plot_Flags.Ytitle = 'I!DH!7a!3!N [R (km s!U-1!N)!U-1!N]'
        ENDIF
        IF Options_Flags.Convert_ProcSpec THEN BEGIN
           Plot_Flags.Y =  Plot_Flags.Y / 22.8
           Plot_Flags.Ytitle = 'I!DH!7a!3!N [R (km s!U-1!N)!U-1!N]'
        ENDIF
        IF Par_Flags.Use_HI OR Par_Flags.Use_CO OR Par_Flags.Use_Hi_Res_CO THEN BEGIN
           Plot_Flags.Ytitle = 'T!DA!N [K]'
        ENDIF
        Plot_Flags.Title = 'Average of Multiple Spectra Added/Subtracted'
        Plot_Flags.N_Oplot = 0
        Plot_Spect
        CurSpect.X_Win_2 = !x
        CurSpect.Y_Win_2 = !y
     ENDIF
     IF Print_Flags.To_Printer OR Print_Flags.To_File THEN BEGIN
        Device,  /close
        Set_Plot, 'x'
        IF Print_Flags.To_Printer THEN BEGIN
           Spawn, 'lp -d '+Print_Flags.Name+' idl.ps'
           Spawn, '/bin/rm idl.ps'
        ENDIF
     ENDIF
     Print_Flags.To_Printer = 0
     Print_Flags.To_File = 0
     Print_Flags.Cancel = 0
  ENDIF ELSE BEGIN
     OPENW, unit, Manip_Flags.Save_File_Base+'.spe', /GET_LUN
     FOR i = 0, N_ELEMENTS(Final_Spect.Vel)-1 DO BEGIN
        Factor = 1.0
        IF Options_Flags.Convert_NSS THEN Factor = 684.1
        IF Options_Flags.Convert_ProcSpec THEN Factor = 22.8
        PRINTF, unit, Final_Spect.Vel[i], Final_Spect.Data[i]/Factor, $
           Final_Spect.Var[i]/Factor^2
     ENDFOR
     CLOSE, unit
     FREE_LUN, unit
     OPENW, unit, Manip_Flags.Save_File_Base+'.rcd', /GET_LUN
     PRINTF, unit, "Map Indices Averaged and Added"
     FOR i = 0, N_ELEMENTS(Plot_Add_Indices)-1 DO PRINTF, unit, Plot_Add_Indices(i)
     PRINTF, unit, "Map Indices Averaged and Subtracted"
     FOR i = 0, N_ELEMENTS(Plot_Sub_Indices)-1 DO PRINTF, unit, Plot_Sub_Indices(i)
     CLOSE, unit
     FREE_LUN, unit
     ;; A messy hack
     ;Print_Flags.To_Printer = 0
     ;Print_Flags.To_File = 0
     ;Print_Flags.Cancel = 0
  ENDELSE
  
  WSET, Widget_Window_1.WinNum
  !x = CurSpect.X_Win_1
  !y = CurSpect.Y_Win_1

END



PRO Draw_Map, V_Cen = V_Cen, Delta_V = Delta_V, Window = Window

  COMMON WHAMVIEW_Widgets
  
  COMMON WHAMVIEW_Data
  
  ;; Set up display options
  IF Display_Flags.Default THEN BEGIN
     !P.Font = -1
     !P.Charsize = 1
     Scale = 1.0
     Font = '!3'
  ENDIF 
  IF Display_Flags.ForPapers THEN BEGIN
     !P.Font = 1
     DEVICE, SET_FONT = 'Times', /tt
     !P.Charsize = 2
     Scale = 2
     Font = '!7'
  ENDIF
  IF NOT(KEYWORD_SET(Window)) THEN WSET, Widget_Window_1.WinNum ELSE $
     WSET, Window
  Widget_Control, Widget_Window_1_Field.LMin, Get_Value = LMin
  Widget_Control, Widget_Window_1_Field.LMax, Get_Value = LMax
  Widget_Control, Widget_Window_1_Field.BMin, Get_Value = BMin
  Widget_Control, Widget_Window_1_Field.BMax, Get_Value = BMax

  Widget_Control, Widget_Window_1_Field.Center, Get_Value = Center
  IF NOT(KEYWORD_SET(V_Cen)) AND NOT(KEYWORD_SET(Delta_V)) THEN BEGIN
     Widget_Control, Widget_Window_1_Field.VCen, Get_Value = V_Cen
     Widget_Control, Widget_Window_1_Field.DeltaV, Get_Value = Delta_V
  ENDIF
  Widget_Control, Widget_Bar_1.Draw_Map, /HOURGLASS
  IF Delta_V NE 0.0 THEN BEGIN
     Title = STRTRIM(FLOOR(V_Cen-(Delta_V)/2.), 2) + " km s!U-1!N < V!DLSR!N < " + $
             STRTRIM(FLOOR(V_Cen+(Delta_V)/2.), 2) + " km s!U-1!N"
     ;;IF V_Cen LT 10 AND V_Cen GT 0 THEN $
     ;; Title = 'V!DLSR!N =   ' + STRTRIM(V_Cen, 2) + ' km s!U-1!N'
     ;;IF V_Cen GT -10 AND V_Cen LT 0 THEN $
     ;; Title = 'V!DLSR!N =  ' + STRTRIM(V_Cen, 2) + ' km s!U-1!N'
     ;;IF V_Cen EQ 0 THEN Title = 'V!DLSR!N =   ' + STRTRIM(V_Cen, 2) + ' km s!U-1!N'
     ;;IF V_Cen GE 10 THEN Title = 'V!DLSR!N =  ' + STRTRIM(V_Cen, 2) + ' km s!U-1!N'
     ;;IF V_Cen LE -10 THEN Title = 'V!DLSR!N = ' + STRTRIM(V_Cen, 2) + ' km s!U-1!N' 
 ENDIF ELSE Title = "Integrated Over All Velocities"
  IF Print_Flags.To_Printer OR Print_Flags.To_File THEN BEGIN
     Set_Plot, 'ps'
     IF Print_Flags.To_Printer THEN Device,  /Color, /Land,  BITS = 24, File = 'idl.ps'
     IF Print_Flags.To_File THEN Device,  /Color,  /Land,  $
                                          BITS = 24, File = Print_Flags.PS_Name
  ENDIF
  ;; Take care of freaky duplicate coords by putting in the
  ;; 'right' coords from pointings.dat
  ;; TAKEN CARE OF IN LATER VERSIONS OF MAP STRUCTURE
;  Str_Arr = ['b1070_16', 'b964_4', 'b860_21', $
;             'b911_3.', 'b912_12', 'b966_35']
;  Lon_Orig = [194.412, 195.304, 210.392, 193.823, 199.652, 206.221]
;  Lat_Orig = [-50.0664, -40.7365, -26.3055, -34.7968, -33.9464, -35.6466]
;  Lon_Repl = [192.90, 196.58, 209.32, 195.01, 198.47, 207.42]
;  Lat_Repl = [-50.06, -40.74, -26.31, -34.80, -33.95, -35.65]
;  FOR i = 0, N_ELEMENTS(Str_Arr)-1 DO BEGIN
;    PRINT, Str_Arr(i), WHERE(STRPOS(WhamNSS.Name, Str_Arr[i]) NE -1)
;    IF Options_Flags.Fix_Dups THEN BEGIN
;      WhamNSS[WHERE(STRPOS(WhamNSS.Name, Str_Arr[i]) NE -1)].GLon = Lon_Repl[i]
;      WhamNSS[WHERE(STRPOS(WhamNSS.Name, Str_Arr[i]) NE -1)].GLat = Lat_Repl[i]
;    ENDIF ELSE BEGIN
;      WhamNSS[WHERE(STRPOS(WhamNSS.Name, Str_Arr[i]) NE -1)].GLon = Lon_Orig[i]
;      WhamNSS[WHERE(STRPOS(WhamNSS.Name, Str_Arr[i]) NE -1)].GLat = Lat_Orig[i]
;    ENDELSE
;  ENDFOR
  
  !P.POSITION = [.1, .05, .9, .95]
  IF Options_Flags.Color_Bar THEN !P.POSITION = [.1, .15, .9, .95]

  Sub_Map = MapSlice(Map, LMin, LMax, BMin, BMax, radec = Par_Flags.Ra_Dec)
  IF Options_Flags.NoStars_Fast OR Options_Flags.NoStars_Slow THEN BEGIN
     IF Options_Flags.NoStars_Slow THEN BEGIN
        IF NOT(KEYWORD_SET(SAO6)) THEN RESTORE, '/d/wham/pro/sao.dat'
        FindStars, Sub_Map, SAO6, Bad, window = 0.55
;        SAVE, Bad, file = 'bad_wholemap_r0.55.dat'
;        Help,  Bad
     ENDIF ELSE BEGIN
        RESTORE, '/d/wham2/madsen/pro/bad_wholemap_r0.55.dat'
        ;;RESTORE, '/d/wham2/madsen/pro/bad_0_240_r0.55.dat'
     ENDELSE
     IF Options_Flags.NoStars_Fast THEN $
        Map_Int = IntMap_Fast(Sub_Map, vmin = FLOOR(V_Cen-(Delta_V)/2.), $
                              vmax = FLOOR(V_Cen+(Delta_V)/2.), bad = bad) $ ; >  0.01 $
        ELSE $
        Map_Int = IntMap(Sub_Map, vmin = FLOOR(V_Cen-(Delta_V)/2.), $
                         vmax = FLOOR(V_Cen+(Delta_V)/2.), bad = bad ) ; >  0.01 $
  ENDIF ELSE BEGIN
     Map_Int = IntMap(Sub_Map, vmin = FLOOR(V_Cen-(Delta_V)/2.), $
                      vmax = FLOOR(V_Cen+(Delta_V)/2.), $
                      hi = (Par_Flags.Use_HI OR Par_Flags.Use_CO)) ; >  0.01
  ENDELSE
  Map_Int = DOUBLE(Map_Int)
  ;; Convert to column density, in units of 10^20 cm^-2
  IF Par_Flags.Use_HI THEN Map_Int = DOUBLE(Map_Int) * 1.823e-2 
  IF (Par_Flags.Use_CO OR Par_Flags.Use_Hi_Res_CO) THEN Map_Int = DOUBLE(Map_Int) * 1.8 ;; x factor from Dame et al 2001
  IF Options_Flags.Convert_NSS THEN Map_Int =  Map_Int / 684.1
  IF Options_Flags.Convert_ProcSpec THEN Map_Int =  Map_Int / 22.8
  
  IF Options_Flags.Fix_Missing THEN BEGIN
     Repl_Lon = [204.65, 211.54, 239.12, 96.17, 71.61, $
                 72.73, 73.85, 74.97, 198.05, 356.12, 349.21, $
                 6.20, 321.88, 334.73, 25.83, 348.67, 11.76, $
                 53.10, 334.93, 348.08, 355.59, 52.36, 59.04, $
                 66.60, 29.84, 66.53, 225.91, 238.93]
     Repl_Lat = [5.09, 5.09, 11.03, 20.37, 28.86, 28.86, 28.86, $
                 28.86, 28.86, 34.80, 40.74, 46.68, 46.68, 46.68, $
                 52.62, 52.62, 58.56, 58.56, 58.56, 58.56, 58.56, $
                 64.50, 70.44, 76.38, 82.32, 82.32, -0.85, -5.94]
     Extra_Name_Friend = ['b30_48', 'b31_48', 'b88_48', 'b174_26', $
                          'b223_45', 'b223_45', 'b223_45', 'b223_45', $
                          'b241_44', 'b317_41', 'b369_38', 'b372_34', $
                          'b418_33', 'b420_34', 'b428_30', 'b475_31', $
                          'b478_26', 'b484_26', 'b525_25', 'b527_26', $
                          'b528_29', 'b533_44', 'b563_35', 'b588_42', $
                          'b603_46', 'b604_43', 'b651_41', 'b705_48']
     Extra_Name = ['b30_49', 'b31_49', 'b88_49', 'b174_27', $
                   'b223_46', 'b223_47', 'b223_48', 'b223_49', $
                   'b241_45', 'b317_42', 'b369_39', 'b372_35', $
                   'b418_34', 'b420_35', 'b428_31', 'b475_32', $
                   'b478_27', 'b484_27', 'b525_26', 'b527_27', $
                   'b528_30', 'b533_45', 'b563_36', 'b588_43', $
                   'b603_47', 'b604_44', 'b651_42', 'b705_49']
     Extra = { XPOINTING }
     Extra_Map = REPLICATE(Extra,  28)
     Extra_Map_Int = FLTARR(28)
     j = 0
     FOR i = 0L, 27 DO BEGIN
        Ind = WHERE(STRPOS(Map.Name, Extra_Name_Friend[i]) NE -1, Count)
        IF Count GT 0 THEN BEGIN
           Near = SpectNear(Map, Map[Ind].Glon, Map[Ind].GLat, 1.0, Ncnt)
           IF Ncnt NE 0 THEN BEGIN
              Int = IntMap(Map[Near], vmin = FLOOR(V_Cen-(Delta_V)/2.), $
                           vmax = FLOOR(V_Cen+(Delta_V)/2.)) ;; /22.83 ;;/684.1 Never use this anymore
              Extra_Map_Int[j] = TOTAL(Int)/N_ELEMENTS(Int)
           ENDIF ELSE PRINT,  "You're screwed"
           Extra_Map[j].GLon = Repl_Lon[i]
           Extra_Map[j].GLat = Repl_Lat[i]
           j = j+1
        ENDIF
     ENDFOR
     IF j NE 0 THEN BEGIN 
        Map = [Map, Extra_Map[0:j-1]] 
        Map_Int = [Map_Int, Extra_Map_Int[0:j-1]] 
     ENDIF
  ENDIF
  IF Options_Flags.Track_Negs THEN BEGIN
     Map_Int = Map_Int >  0.0
     Negs = WHERE(Map_Int LT 0.0, Count)
     ;PRINT, "There are ", Count, " negative pointings",  MIN(Map_Int)
     ;OPENW, unit, '~madsen/wham/Neg_Pointings.txt', /get_lun
     ;OPENW, unit1, '~madsen/wham/Neg_Pointings_Indices.txt', /get_lun
     ;FOR i = 0, N_ELEMENTS(Negs)-1 DO PRINTF, unit1, Negs[i]
     ;CLOSE, unit1 &  FREE_LUN, unit1
     IF Count GT 0 THEN BEGIN
        FOR i = 0, N_ELEMENTS(Negs)-1 DO BEGIN
           Date = STRMID(Map[Negs[i]].Name, $
                         STRPOS(Map[Negs[i]].Name, 'y/')+2, $
                         STRPOS(Map[Negs[i]].Name, 'y/')+7)
           Pointing = STRMID(Map[Negs[i]].Name, $
                             STRPOS(Map[Negs[i]].Name, 'o/')+2, $
                             STRPOS(Map[Negs[i]].Name, '.fts'))
           PRINT, i, Map[Negs[i]].Name
           SPAWN, '/bin/rm ~madsen/wham/temp.txt'
           SPAWN, '/d/wham/bin/fitskeys ' + Map[Negs[i]].Name + $
                  ' -DATE -TIME-OBS -GAL-LON -GAL-LAT > ~madsen/wham/temp.txt'
           OPENR, unit2, '~madsen/wham/temp.txt', /get_lun
           string = ''
           READF, unit2, string
           READF, unit2, string
           READF, unit2, string
           CLOSE, unit2
           FREE_LUN, unit2
           EULER, Map[Negs[i]].GLon, Map[Negs[i]].GLat, Ecl_Lon, Ecl_Lat, 6
           PRINTF, unit, STRTRIM(String, 2), Ecl_Lon, Ecl_Lat, Map_Int[Negs[i]], $
                   FORMAT = '(A, 2F6.1,2X,F6.3)'
           Near_Neg = SpectNear(Map, Map[Negs[i]].Glon, Map[Negs[i]].Glat, 1.0, NCnt)
           IF Negs[i] NE 0 THEN Near_Neg = Negs[i]-1 ELSE Near_Neg = Negs[i]+1
           Print,  "Near_Neg", Near_Neg
           Good_Inds = WHERE(Map_Int[Near_Neg] GT 0.0, Good_Count)
           Print, "Good Inds", Near_Neg[Good_Inds]
           IF Good_Count GT 0 THEN BEGIN
              IF STRPOS(Map[Negs[i]].Name, 'b1203_') EQ -1 THEN $
                 Map_Int[Negs[i]] = TOTAL(Map_Int[Near_Neg[Good_Inds]])/Good_Count
           ENDIF ELSE PRINT,  "You're screwed again"
        ENDFOR
     ENDIF
     ;CLOSE, unit
     ;FREE_LUN, unit
  ENDIF
  Beam_Radius = 0.5 / (FLOAT(Par_Flags.Use_HI OR Par_Flags.Use_CO)+1.0)
  IF Options_Flags.Yellow_Bkg THEN BEGIN
     loadct, 3
     tvlct, r, g, b, /get
     r[255] = 206 & g[255] = 233 & b[255] = 255
     tvlct, r, g, b
     polyfill, [1, 1, 0, 0, 1], [1, 0, 0, 1, 1], /normal, color = 255
     noerase = 1
  ENDIF
  
  IF Par_Flags.Ignore_Center THEN BEGIN
     WHAMMAP, Sub_Map, 0, 0, Map_Int, /Useimage, $
        title = title,  log = Options_Flags.Log, $
        linear = Options_Flags.Linear, $
        smooth = Options_Flags.Smooth, nogrid = Options_Flags.NoGrid, $
        fill = Options_Flags.Fill, beamradius = Beam_Radius, NoErase = NoErase, $
        radec = Par_Flags.Ra_Dec,  bimage = Byte_Image, charthick = 3, $
        scale = Scale, font = Font
     
     ;; OVERPLOT CONTOURS AND LABELS FOR HAF_FIL

     ;;SAVE, Map_Int, file = '~/AAS_200/Haf_Fil_Map_Image.dat'
     ;;SAVE, Map, file = '~/AAS_200/Haf_Fil_Map.dat'
     ;; RESTORE, '~/AAS_200/Haf_Fil_Map_Image.dat'
     ;;RESTORE, '~/AAS_200/Haf_Fil_Map.dat'
     ;;lon = map.glon
     ;;lat = map.glat
     ;;LOADCT, 1
     ;;CONTOUR, map_int, -lon, lat, $
     ;;  levels = [1.2, 1.5, 2.0, 8, 20], c_labels = 0, /irr, /overplot, color = 0, thick = 2
     ;;LOADCT, 3
     ;;XYOUTS, -224.02-10, -1.74-2, "CMa R1", CHARTHICK = 10, CHARSIZE = 2.5, color = 175
     ;;XYOUTS, -250.85-6.3, 50.17-2+1, "sdB", CHARTHICK = 10, CHARSIZE = 2.5, color = 175
     ;;XYOUTS, -247.55-6.3, 47.75-2-1, "sdO", CHARTHICK = 10, CHARSIZE = 2.5, color = 175
     ;;XYOUTS, -236.30, 50.92-2, "A", CHARTHICK = 10, CHARSIZE = 2.5, color = 175
     ;;XYOUTS, -235.17-1, 29.70-2, "D", CHARTHICK = 10, CHARSIZE = 2.5, color = 175
     ;;XYOUTS, -226.18-1, 38.19-2, "B", CHARTHICK = 10, CHARSIZE = 2.5, color = 175
     ;;XYOUTS, -225.24-1, 22.07-2, "E", CHARTHICK = 10, CHARSIZE = 2.5, color = 175
     ;;XYOUTS, -226.02-1, 10.18-2, "F", CHARTHICK = 10, CHARSIZE = 2.5, color = 175
     ;;XYOUTS, -217.07-1, 28.86-2, "C", CHARTHICK = 10, CHARSIZE = 2.5, color = 175

     ;; OVERPLOT CONTOURS FOR LOCKMAN SPUR
;      LOADCT, 1
;      RESTORE, '~/AAS_200/spur_hi.dat'
;      nhk = reform(spur_hi.data[0, *, *])
;      FOR i = 0, n_elements(spur_hi.glon)-1 DO $ 
;        FOR j = 0, n_elements(spur_hi.glat)-1 DO $
;        nhk[i, j] = int_tabulated(spur_hi.vel, spur_hi.data[*, i, j]) * 1.8224e18
;      CONTOUR, nhk, -spur_hi.glon, spur_hi.glat, levels = [.75, 1.0, 2.0]*1e20, $
;        c_labels = 0, c_thick = [.75, 2, 5], /overplot, color = color(230)
;      LOADCT, 3
  ENDIF ELSE BEGIN
     ;; Below was for Ron's LOFAR talk
;      Map_Int(WHERE(Map_Int LE 0.6)) = 0
;      Map_Int(WHERE(Map_Int GT 0.6 AND Map_Int LE 2.5)) = 1
;      Map_Int(WHERE(Map_Int GT 2.5 AND Map_Int LE 5.9)) = 2
;      Map_Int(WHERE(Map_Int GT 5.9 AND Map_Int LE 70)) = 3
;      Map_Int(WHERE(Map_Int GT 70)) = 4
;      r = BYTARR(256)
;      g = BYTARR(256)
;      b = BYTARR(256)
;      r[0:101] = 0
;      r[102:204] = 255
;      r[205:254] = 131
;      r[255] = 255
;      g[0:101] = 0    
;      g[102:152] = 255
;      g[153:254] = 0
;      g[255] = 255
;      b[0:50] = 0 
;      b[51:101] = 255
;      b[102:254] = 0
;      b[255] = 255
;      TVLCT, r, g, b
;      title = 'Galactic Free-Free Opacity'
     WHAMMAP, Sub_Map, 0, 0, Map_Int, /UseImage, $
              full = FIX(Center[0]), title = title, $
              log = Options_Flags.Log, linear = Options_Flags.Linear, $
              smooth = Options_Flags.Smooth, nogrid = Options_Flags.NoGrid, $
              fill = Options_Flags.Fill,  $
              beamradius = Beam_Radius, NoErase = NoErase, radec = Par_Flags.Ra_Dec, $
              bimage = Byte_Image, charthick = 3, scale = Scale, font = Font
  ENDELSE
  ;plothist, byte_image
  ;wait, 2
  IF Options_Flags.Color_Bar THEN BEGIN     
     XTickName = STRARR(7)
     XTick_Numbers = DBLARR(7)
     ;; If we are smoothing, we need to recover the original
     ;; byte image from Map_Int
     IF Options_Flags.Fill THEN BEGIN
        HELP, Byte_Image
        IF !d.name EQ 'PS' THEN missing = byte(!d.table_size-1) ELSE $
           missing = byte(!p.background)
        Map_Int = Map_Int[WHERE(Byte_Image NE Missing)]
        Byte_Image = Byte_Image[WHERE(Byte_Image NE Missing)]
        HELP, Byte_Image
     ENDIF
     FOR i = 0, 6 DO BEGIN
        IF Options_Flags.Linear THEN $
           XTick_Numbers[i] = (MAX(Map_Int)-MIN(Map_Int))/6.0 * i + MIN(Map_Int) $
        ELSE IF Options_Flags.Log THEN $
           XTick_Numbers[i] = (ALOG10(MAX(Map_Int)) - $
                               ALOG10(MIN(Map_Int)))/6.0 * i + ALOG10(MIN(Map_Int)) $ 
        ELSE BEGIN
           j = 0
           Indices = WHERE(Byte_Image EQ (ROUND((i/6.0)*254)+j), Count)
           WHILE (Count EQ 0) DO BEGIN
              Indices = WHERE(Byte_Image EQ (ROUND((i/6.0)*254)+j), Count)
              j = j+1
           ENDWHILE
           ;PRINT, Count, 'color = ', (ROUND((i/6.0)*254)+j-1)
           
           XTick_Numbers[i] = MEAN(Map_Int[Indices])
        ENDELSE
        XTickName[i] = Options_Flags.Log ? STRTRIM(10.0^(XTick_Numbers[i]), 2) : STRTRIM(XTick_Numbers[i], 2)
        ;XTickName[i] = STRMID(XTickName[i], 0, STRPOS(XTickName[i], '.')+3)
        ;PRINT, XTickName[i]
        XTickName[i] = STRING(XTickName[i], FORMAT = '(F6.2)')
     ENDFOR
     Color_Bar_Title = 'Intensity [ADU * km s!U-1!N]'
     IF Par_Flags.Use_HI THEN Color_Bar_Title = 'HI Column Density [10!U20!N cm!U-2!N]'
     IF Par_Flags.Use_CO THEN Color_Bar_Title = 'H!D2!N Column Density [10!U20!N cm!U-2!N]'
     IF Options_Flags.Convert_NSS OR Options_Flags.Convert_ProcSpec THEN Color_Bar_Title = 'H!4a!3 Intensity [Rayleighs]'
     colorbar, ncolors = !d.table_size-1, $
               position = [0.05, 0.05, 0.95, 0.075], $
               title = Color_Bar_Title, $
               xtickname = xtickname, format = '', $
               charthick = 1
;; Below was for Ron's LOFAR talk
;      colorbar, ncolors = !d.table_size-1, $
;        position = [0.1, 0.05, 0.9, 0.075], $
;        title = '!7s!9'+ STRING("142B)+'!3 1', $
;        xtickname = [' ', ' ', '1 MHz', ' ', '2 MHz', ' ', $
;                     '3 MHz', ' ', '10 MHz', ' ', ' '], format = '', $
;        charsize = 2.0, divisions = 10

  ENDIF
  
  ;; Put pluses and minues on the screen
  IF NOT Par_Flags.Use_Hi_Res_CO THEN $
     Rad = 0.5/(FLOAT(Par_Flags.Use_HI OR Par_Flags.Use_CO)+1.0) $
  ELSE Rad =  0.125
  IF Plots_Flags.N_Plus GT 0 THEN BEGIN
     FOR i = 0, Plots_Flags.N_Plus-1 DO BEGIN
        PLOTS, [Plots_Flags.Plus_Lon[i]-Rad, Plots_Flags.Plus_Lon[i]+Rad], $
           [Plots_Flags.Plus_Lat[i], Plots_Flags.Plus_Lat[i]], /data, THICK = 2
        PLOTS, [Plots_Flags.Plus_Lon[i], Plots_Flags.Plus_Lon[i]], $
           [Plots_Flags.Plus_Lat[i]-Rad, Plots_Flags.Plus_Lat[i]+Rad], /data, THICK = 2
     ENDFOR
  ENDIF
  IF Plots_Flags.N_Minus GT 0 THEN BEGIN
     FOR i = 0, Plots_Flags.N_Minus-1 DO BEGIN
        PLOTS, [Plots_Flags.Minus_Lon[i]-Rad, Plots_Flags.Minus_Lon[i]+Rad], $
           [Plots_Flags.Minus_Lat[i], Plots_Flags.Minus_Lat[i]], /data, THICK = 2
     ENDFOR
  ENDIF
  IF Print_Flags.To_Printer OR Print_Flags.To_File THEN BEGIN
     Device,  /close
     Set_Plot, 'x'
     IF Print_Flags.To_Printer THEN BEGIN
        PRINT, 'PRINTING3'
        Spawn, 'lp -d '+Print_Flags.Name+' idl.ps'
        Spawn, '/bin/rm idl.ps'
     ENDIF
  ENDIF
  Print_Flags.To_Printer = 0
  Print_Flags.To_File = 0
  Print_Flags.Cancel = 0
END

PRO Sensitize, ID, Flag
  
  COMMON WHAMVIEW_Widgets
  
  COMMON WHAMVIEW_Data

  IF ID NE Widget_Window_1_Label.LMin THEN BEGIN
     Widget_Control, Widget_Window_1_Label.LMin, Sensitive = (Flag+1) MOD 2
     Widget_Control, Widget_Window_1_Field.LMin, Sensitive = (Flag+1) MOD 2
  ENDIF
  IF ID NE Widget_Window_1_Label.LMax THEN BEGIN
     Widget_Control, Widget_Window_1_Label.LMax, Sensitive = (Flag+1) MOD 2
     Widget_Control, Widget_Window_1_Field.LMax, Sensitive = (Flag+1) MOD 2
  ENDIF
  IF ID NE Widget_Window_1_Label.BMin THEN BEGIN
     Widget_Control, Widget_Window_1_Label.BMin, Sensitive = (Flag+1) MOD 2
     Widget_Control, Widget_Window_1_Field.BMin, Sensitive = (Flag+1) MOD 2
  ENDIF
  IF ID NE Widget_Window_1_Label.BMax THEN BEGIN
     Widget_Control, Widget_Window_1_Label.BMax, Sensitive = (Flag+1) MOD 2
     Widget_Control, Widget_Window_1_Field.BMax, Sensitive = (Flag+1) MOD 2
  ENDIF
  IF ID NE Widget_Window_1_Label.Center THEN BEGIN
     IF NOT Par_Flags.Ignore_Center THEN BEGIN
        Widget_Control, Widget_Window_1_Label.Center, Sensitive = (Flag+1) MOD 2
        Widget_Control, Widget_Window_1_Field.Center, Sensitive = (Flag+1) MOD 2
     ENDIF
  ENDIF
  IF ID NE Widget_Window_1_Label.VCen THEN BEGIN
     Widget_Control, Widget_Window_1_Label.VCen, Sensitive = (Flag+1) MOD 2
     Widget_Control, Widget_Window_1_Field.VCen, Sensitive = (Flag+1) MOD 2
  ENDIF
  IF ID NE Widget_Window_1_Label.DeltaV THEN BEGIN
     Widget_Control, Widget_Window_1_Label.DeltaV, Sensitive = (Flag+1) MOD 2
     Widget_Control, Widget_Window_1_Field.DeltaV, Sensitive = (Flag+1) MOD 2
  ENDIF

END

PRO PLOT_SPECT
  
  COMMON WHAMVIEW_Data
  ;; Set up display options
  IF Display_Flags.Default THEN BEGIN
     !P.Font = -1
     !P.Charsize = 1
     Scale = 1.0
     Font = '!3'
  ENDIF 
  IF Display_Flags.ForPapers THEN BEGIN
     !P.Font = 1
     DEVICE, SET_FONT = 'Times', /tt
     !P.Charsize = 2
     Scale = 2
     Font = '!7'
  ENDIF

  IF NOT(Par_Flags.Use_HI) AND NOT(Par_Flags.Use_CO) AND NOT(Par_Flags.Use_Hi_Res_CO) THEN BEGIN
     XMin = MAX(Plot_Flags.X)-200.
     XRange = [XMin, MAX(Plot_Flags.X)]
     YMax = MAX(Plot_Flags.Y[WHERE(Plot_Flags.X GE XMin)])
     YMin = MIN(Plot_Flags.Y[WHERE(Plot_Flags.X GE XMin)])
     YRange = [YMin, YMax]
  ENDIF ELSE BEGIN
     Xrange = [MIN(Plot_Flags.X), MAX(Plot_Flags.X)]
     Yrange = [MIN(Plot_Flags.Y), MAX(Plot_Flags.Y)]
  ENDELSE
  delta_y = yrange[1] - yrange[0]
  delta_x = xrange[1] - xrange[0]
  Yrange = [Yrange[0]-.1*Delta_Y, Yrange[1]+.1*Delta_y]
  Xrange = [Xrange[0]-.1*Delta_X, Xrange[1]+.1*Delta_x]
  
  Color_Array = [.5, .6, .7, .8, .9]*!D.Table_Size
  
  
  PLOT, Plot_Flags.X, Plot_Flags.Y, $
     ytitle = Plot_Flags.Ytitle, $
     xtitle = Plot_Flags.xtitle, $
     title = Plot_Flags.title, $
     YRANGE = [Yrange[0] + Plot_Flags.Y_Min*Delta_Y, $
               Yrange[1] - (1.-Plot_Flags.Y_Max)*Delta_Y], $
     XRANGE = [Xrange[0] + Plot_Flags.X_Min*Delta_X, $
               Xrange[1] - (1.-Plot_Flags.X_Max)*Delta_X], $
     XSTYLE = 1, YSTYLE = 1, POSITION = [.1, .1, .9, .9]

  
  SPAWN, 'whoami', username
  SPAWN, 'hostname', hostname
  
  XYOUTS, Xrange[1] - (1.-Plot_Flags.X_Max)*Delta_X + $
     .05 * ((Xrange[1] - $
             (1.-Plot_Flags.X_Max)*Delta_X) - $
            (Xrange[0] + Plot_Flags.X_Min*Delta_X)),  $
     Yrange[0] + Plot_Flags.Y_Min*Delta_Y, $
     username+"@"+hostname+" "+SYSTIME(), $
     CHARSIZE = .4, ORIENTATION = 90
  IF Plot_Flags.N_Oplot GT 0 THEN BEGIN
     FOR i = 0, Plot_Flags.N_Oplot-1 DO BEGIN
        Color =  (Plot_Flags.Use_White[i]) ? !D.Table_Size-1 : Color_Array[i MOD 5]
        OPLOT, Plot_Flags.Oplot_X[i, *], Plot_Flags.Oplot_Y[i, *], $
           Color = Color
        
     ENDFOR
  ENDIF
  
END
