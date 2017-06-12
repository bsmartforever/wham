PRO Read_Pixel_Motion, event

; Handles draw widget motion events. Display the
; cursor location and value of image at that location.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Display cursor location.

xloc = StrTrim(Fix(event.x / info.scale), 2)
yloc = StrTrim(Fix(event.y / info.scale), 2)
Widget_Control, info.xLocationID, Set_Value=xloc
Widget_Control, info.yLocationID, Set_Value=yloc

   ; Make sure value is not byte type.

value = (*info.image)[event.x, event.y]
thisType = Size(value, /TName)
IF thisType EQ 'BYTE' THEN value = Fix(value)

   ; Display image value.

Widget_Control, info.valueID, Set_Value=StrTrim(value, 2)
Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;----------------------------------------------------------



PRO Read_Pixel_Cleanup, tlb

; Clean up the image pointer.

Widget_Control, tlb, Get_UValue=info
IF N_Elements(info) GT 0 THEN Ptr_Free, info.image
END
;----------------------------------------------------------



PRO Read_Pixel, imagedata, Group_Leader=groupleader, Scale=scale

; Displays an image. Prints out cursor location and value
; of image at that location.

; imagedata -- The 2D array you want to display
; groupleader -- The group leader of this widget program.
; scale -- The image will be scaled by this amount. For
;          example, scale=2 will result in an image twice
;          as large as the original. Pixel locations will
;          also be scaled by a factor of 1/scale.

On_Error, 2

   ; Need an image argument.

IF N_Elements(imagedata) EQ 0 THEN BEGIN
   Message, 'Image argument is required. Returning...'
ENDIF

   ; Size of image.

nDimensions = Size(imagedata, /N_Dimensions)
IF nDimensions NE 2 THEN BEGIN
   Message, 'Image argument must be 2D image. Returning...'
ENDIF

   ; Is there a scaling factor?

IF N_Elements(scale) EQ 0 THEN BEGIN
   title = 'Read Pixel Program'
   scale = 1.0
ENDIF ELSE title = 'Read Pixel Program: Scaled at ' + String(scale, Format='(F5.2)')
scale = 10.0E-4 > Float(scale) ; Minimum scaling value.

s = Size(imagedata, /Dimensions)
xsize = s[0]
ysize = s[1]
IF Float(scale) NE 1.0 THEN BEGIN
   image  = Congrid(imagedata, xsize * scale, ysize * scale)
ENDIF ELSE image = imagedata

   ; Get size of image, so you know how big to make window.

s = Size(image, /Dimensions)
xsize = s[0]
ysize = s[1]

   ; Create widgets.

tlb = Widget_Base(Title=title, Column=1, TLB_Frame_Attr=1, $
   Base_Align_Center=1)

drawID = Widget_Draw(tlb, XSize=xsize, YSize=ysize, $
   Motion_Events=1, Event_Pro='Read_Pixel_Motion')

   ; Program information widgets.

buttonBaseID = Widget_Base(tlb, Row=1)
xLabelID = Widget_Label(buttonBaseID, Value=' X Loc: ')
xLocationID = Widget_Text(buttonBaseID, Scr_XSize=40)
yLabelID = Widget_Label(buttonBaseID, Value=' Y Loc: ')
yLocationID = Widget_Text(buttonBaseID, Scr_XSize=40)
valLabelID = Widget_Label(buttonBaseID, Value=' Value: ')
valueID = Widget_Text(buttonBaseID, Scr_XSize=60)

Widget_Control, tlb, /Realize

   ; Display image.

Widget_Control, drawID, Get_Value=wid
WSet, wid
TV, BytScl(image, Top=!D.Table_Size-1)

   ; Structure to hold program information.

info = { image:Ptr_New(image), $      ; The image data.
         scale:scale, $               ; The scaling factor.
         xLocationID:xLocationID, $   ; The X Location widget ID.
         yLocationID:yLocationID, $   ; The Y Location widget ID.
         valueID:valueID}             ; The Image Value widget ID>

Widget_Control, tlb, Set_UValue=info, /No_Copy

XManager, 'read_pixel', tlb, /No_Block, $
   Cleanup='Read_Pixel_Cleanup', Group_Leader=groupleader


END