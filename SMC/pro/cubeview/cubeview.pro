;BEGIN HEADER

; NOTE:  This is my private version to mess with (Steve Tufte 5/3/95).

; ****************Things left to do****************************************
; *********(a note for Glen Cooper, Steve Tufte, or who is playing with 
;                                                                        this.)
;
;  x    <- place an x next to the improvements as they are complteted
;---------------------------------------------------------------------
;       In true IDL fashion, I'll start the list with #0
;
;  	0) 	Get the Print Image and Print Spectrum buttons to work.
;		Steve Tufte agreed to do this, and he is the local expert
;		on IDL output, so talk to him if this isn't done.
;  	1)	The load_data procedure doesn't do much.  It could be 
;		programmed to read in the filename or variable name
;		from the user.  The format of the file could be customized
;		so that velocity information wouldn't have to be input 
;		every time.  Note that if these changes are made, the state
;		structure would have to be remade within load_data much like 
;		it is within make_image.pro.
;  	2)	Add a message box where all messages to and from the user 
;		would appear.  This would be much more convenient than the
;		current situation where the original IDL window must be 
;		visible behind the WHAM window.  The message box could 
;		either be a separate, independent widget (like xloadct is),
;		or a text box within the WHAM widget base.  I think there
;		is enough room under the two print buttons.
;  	3) 	IDL provides a procedure which changes the pointer to an 
;		hourglass.  I don't 
;		have a clue how to do this (I haven't tried), but I don't think
;		it will be too difficult.  This is definitely necessary for the
;		Make Image Cube button, which is the computation intensive part
;		of the whole job.
;	4)	Put the entire package (with called procedures, etc.) in
;		a new directory (e.g. /usr/local/lib/idl/lib/userlib).
;		It would be great to have the help file available in "?".




; $Id: lib_template.pro,v 1.2 1994/04/29 16:50:28 dan Exp $
;
; NAME:
;	cubeview
;
; PURPOSE:
;	This is the interactive data analysis package for WHAM.
;
;
; CATEGORY:
;	Widget.
;
; CALLING SEQUENCE:
;	cubeview, image
;
; INPUTS:
;	image - the name of the data cube which you wish to analyze
;
; MOUSE SPECIFICS:
;	The text windows at the bottom of the WHAM screen display information
;	about particular pixels.  The default setting is to have this 
;	information change as the user moves the mouse, however, this can 
;	be changed with the simple click of a button:
;		left button: freezes the display wherever the mouse is 
;			currently located.  
;		middle button: resets to continuous updating.
;
;	programming note: if the left button is pressed, the mouse is still
;		generating events.  I don't know any way around this.
;	
;
; PROCEDURES:
;	Several procedure were written to simplify the main code:
;
;	load_data
;	set_velocity
;	wham_curval
;	spectrum_roi
;       make_image
;       play_moview
;	
;
;	Other procedures used:
;	xloadct
;	shade_surf
;	box_cursor
;	several of the widget routines (like cw_bgroup)
;
; EXAMPLE:
;	Call the widget program to examine WHAM data (or any data cube).
;       image = fltarr(30,24,40)
;	Wham, image
;
; MODIFICATION HISTORY:
; 	Written by:	David Kung, July 1994
;-


PRO MAIN13_Event, Event

;set up the window as the display window
common  draw_comm, draw_id
wset,draw_id

;Dummy variable to know when the quit button has been pressed
quit = 0


;Get state information from the first child of the main widget's uvalue

child = widget_info(event.handler, /child)
widget_control, child, get_uvalue = state, /no_copy


IF state.widgetids(0) eq event.id then BEGIN

      CASE Event.Value OF 
      0: begin
;		Print, 'Button Load Data Pressed'
 		call_procedure,'load_data', state
  	 end
      1: begin
;		Print, 'Button Make Image Cube Pressed'	
		make_image, state		
		tv,state.image(*,*,state.conditions(2))
;	Set the velocity text window
      	        call_procedure,'set_velocity',state
	 end
      2: begin

;****************** STEVE LOOK! You can play a Movie!
	    if state.sos eq 1 then $
		call_procedure, 'play_movie', state  $
	    else print, 'ERROR: Data must be loaded first'
         end	
      3: begin
;		Print,'Button Choose Color Table Pressed'
		call_procedure, 'xloadct', group = state.widgetids(0)
	 end
      4: begin
;		Blank Button (for spacing)		
	 end
	
      5: begin
;		Print, 'Erase Button Pressed'
  		erase
         end
      6: begin
;		Print, 'Button Display Image Pressed'
		tv, state.image(*,*,state.conditions(2))

; 		Set the velocity text window
      	        call_procedure,'set_velocity',state

	 end
      7: begin
;		Print,'Button Show Spectrum Pressed'
	        Plot,state.vel_label, state.data(	$
		floor(state.conditions(0)/state.conditions(5)+.5),	$
		floor(state.conditions(1)/state.conditions(5)+.5),*), psym=5
	 end
      8: begin
;		Print,'Region of Interest Button Pressed'
	  if state.sos eq 1 then begin
		tv,state.image(*,*,state.conditions(2))
		state.conditions(11)=1
   		Print,'Left Button to move box'
		Print,'Center Button to change size'
		Print,'Right Button to see spectrum'
		call_procedure,'box_cursor',x0,y0,nx,ny

;		Save the position of the box
		state.conditions(7)=x0
		state.conditions(8)=y0
		state.conditions(9)=nx
		state.conditions(10)=ny
		call_procedure,'spectrum_roi',state 
   	    end else   $
            print, 'Please load the data before choosing a region of interest.'  
	 end
;
      9: begin
;		Print,'x vs v Plot Button Pressed'
;		This next line is the standard conversion from 
;		state.image coordinates to state.data.  If you don't
;		get it, ask Steve Tufte.
   		y0=floor(state.conditions(1)/state.conditions(5)+.5)
		if state.sos eq 1 then $
		     call_procedure,'shade_surf',reform(state.data(*,y0,*)),  $
			findgen(state.size(0)),  state.vel_label  $
		else print, 'ERROR: Data must be loaded first'
	 end
      10: begin
;		Print,'y vs v Plot Button Pressed'
   		x0=floor(state.conditions(0)/state.conditions(5)+.5)
		if state.sos eq 1 then     $
		     call_procedure,'shade_surf',reform(state.data(x0,*,*)),  $
			findgen(state.size(1)), state.vel_label  $
		else print, 'ERROR: Data must be loaded first'
	 end
      11: begin
		; Blank button
	  end
      12: begin
		print, 'You have just exited WHAM DATA ANALYSIS WIDGET'
		WIDGET_CONTROL, /destroy, event.top
		quit = 1
	 end

      ENDCASE
      ENDIF

IF state.widgetids(0) NE event.id THEN BEGIN

	  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

	CASE Ev OF

	  'DRAW': BEGIN
	     if state.sos eq 1 then begin
;	      	Print, 'Event for Picture' 
 		q = event.press      ; what button
		a = event.x          ; what x coord
   		b = event.y          ; what y coord

; Update the xpos, ypos, and data value fields
		call_procedure, 'wham_curval', state,a,b,q
                end
	     END  

	  'playf': BEGIN
;	      Print, 'Event for playf'
;		Advance the image one frame (and wrap around if at the end)
  	      if state.conditions(2) lt state.size(2)-1 then  $
		 state.conditions(2) = state.conditions(2)+1  $
	 	 else state.conditions(2) = 0
	      tv,state.image(*,*,state.conditions(2))	
; 	Set the z-value slider
	      widget_control,state.widgetids(1), 	$
		 set_value= state.conditions(2)	
; 	Set the velocity text window
      	      call_procedure,'set_velocity',state

	      END

 	  'playb': BEGIN
;              Print, 'Event for playb'		
;		Go back one fram (and wrap around if at the beginning)
		if state.conditions(2) gt 0 then  $
		state.conditions(2)=state.conditions(2)-1  $
		else state.conditions(2) = state.size(2)-1
	      tv,state.image(*,*,state.conditions(2))	
;	Set the z-value slider
 	       widget_control,state.widgetids(1), 	$
			set_value= state.conditions(2)	
; 	Set the velocity text window
      	      call_procedure,'set_velocity',state 	
     END

  	  'ZSLIDER': BEGIN
;     	      Print, 'Event for frame number'
		widget_control, state.widgetids(1),  $
			get_value = temp
		state.conditions(2) = temp
		tv,state.image(*,*,state.conditions(2)) 
		
; 	Set the velocity text window
	      call_procedure,'set_velocity',state
		     END
	  'SCALING':BEGIN
;		Store the choice of scaling type in state.conditions(6)
	      CASE Event.Value OF
		0:BEGIN
;		    Print, 'Event for Frame by Frame'
		    state.conditions(6)=1
		  END
		1:BEGIN
;		    Print, 'Event for None'
		    state.conditions(6)=0
		  END
	      ENDCASE
	      END


; *******************Fill me in Steve************************
	
	  'Prints':begin
		    Print, 'Print Spectrum Button Pressed'
		   end

	  'Printi':begin
	 	    Print, 'Print Image Button Pressed'
		   end

; *******************Fill me in Steve************************



	  'FIELD73': BEGIN
;	            Print, 'Event for filename'
	          END
	ENDCASE
      ENDIF


if quit eq 0 then widget_control, set_uvalue=state,child, /no_copy

END
;
;
; DO NOT REMOVE THIS COMMENT: END MAIN13
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.
;
;
;
PRO cubeview, initimage, GROUP = group

; Get the size of the image
b=size(initimage)

;Now create the data structure which will store all relevant information 
;about this widget.  Note that if you (as the programmer) change anything
;in this structure, YOU MUST ALSO CHANGE IT IN make_image.pro, since
;that procedure remakes the structure.
;
;
state = {sos:		0,		$
	widgetids:      intarr(11),	$
	conditions: 	intarr(15),	$
	filename:	strarr(30),	$
	velocity:	fltarr(2),	$
	data:		initimage,		$
	size:		[b(1),b(2),b(3)],	$
	image:		bytarr(200,200,b(3)),	$
	imsize:		intarr(3),	$
	vel_label:	fltarr(b(3))}

;The variables are as follows:
;	sos		State of the State.  = 0 if data has not yet
;			been read in, =1 otherwise
;	widgetids	An array of up to 10 widget's ids:
;			0: first child (to store the state variable)
;			1: z value slider
;			2: movie speed slider
;			3: color lower end slider
;			4: color high end slider
;			5: x position
;			6: y position
;			7: draw widget
;			8: data value text
;			9: Velocity text
;			10: filename
;	conditions	[0,..,14]
;			0: x position
;			1: y position
;			2: z value (from the slider)
;			3: speed of the animation (also from the slider)
;			4: condition of the choose pixel option
;			   1=pixel chosen, 0=pixel not chosen
;			5: enlargement factor (from make_image.pro)
;			6: scaling (1 = frame by frame, 0 = none)
;			  Region of Interest Conditions:
;			7: x value (lower left corner)
;			8: y value (    "      )
;			9: width
;			10: height
;			11: region of interest toggle (1 if it is defined)
;			12: Not Used Now
;			13:
;			14:
;
;	filename: 	the name of the data file
;	velocity:	0: Velocity of first image
;			1: change in velocity 
;	data:		the actual data array
;	size:		the size of the data array
;	image:		the display array (values 0 to 255)
;	imsize:		the size of the display array
;	vel_label:	an array containing velocities (for labeling plots)
;Initialize the structure	

;state.size=[b(1),b(2),b(3)]
state.conditions = [0,0,0,0,0,1,1,0,0,0,0,0,0,0,0]
state.velocity = [0.0,0.0]	


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }



; This Section of the code sets up the widgets.  Most of it was written 
; by the procedure WIDED, but it was significantly altered by Dave.

  MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=2, $
      MAP=1, $
      TITLE='WHAM Data Analysis', $
      UVALUE='MAIN13', $
;      XSIZE=860, $
;      YSIZE=600, $
      XOFFSET=0, $
      YOFFSET = 0)


; The Options button group:
  Btns1250 = [ $
    'Load Data', $
    'Make Image Cube', $
    'Play Movie ',  $
    'Change Color Table', $
    '  ', 	$
    'Erase', $
    'Display Image',  $
    'Show Spectrum', $
    'Choose ROI',   $
    'x vs vel.', $
    'y vs vel.', 	$
    ' ',	$
    'Quit' ]

  BGROUP92 = CW_BGROUP( MAIN13, Btns1250, $
      COLUMN=1, $
      SPACE=5, $
      XPAD=5, $
      YPAD=5, $
      FRAME=2, $
      LABEL_TOP='    Options:')
      state.widgetids(0) = BGROUP92

; The main display window:
  DRAW = WIDGET_DRAW( MAIN13, $
      BUTTON_EVENTS=1, $
      FRAME=2, $
      MOTION_EVENTS=1, $
      RETAIN=2, $
      UVALUE='DRAW', $
      XOFFSET=180, $
      XSIZE=400, $
      YOFFSET=180, $
      YSIZE=400)
      state.widgetids(7) = draw


; The base on the right side of the screen containing secondary options
  BASE53 = WIDGET_BASE(MAIN13, $
      COLUMN=1, $
      SPACE=5, $
      XPAD=5, $
      YPAD=5, $
      FRAME=2, $
      MAP=1, $
      TITLE='Other Options', $
      UVALUE='BASE53')


; Play forward button
  BMP129 = [ $
    [ 0b, 0b ], $
    [ 0b, 0b ], $
    [ 60b, 0b ], $
    [ 252b, 0b ], $
    [ 204b, 3b ], $
    [ 12b, 15b ], $
    [ 12b, 60b ], $
    [ 12b, 112b ], $
    [ 12b, 112b ], $
    [ 12b, 60b ], $
    [ 12b, 15b ], $
    [ 204b, 3b ], $
    [ 252b, 0b ], $
    [ 60b, 0b ], $
    [ 0b, 0b ], $
    [ 0b, 0b ]  $
  ]
  BMPBTN49 = WIDGET_BUTTON( BASE53,VALUE=BMP129, $
      FRAME=1, $
      UVALUE='playf')

; Play backward button
  BMP131 = [ $
    [ 0b, 0b ], $
    [ 0b, 0b ], $
    [ 0b, 60b ], $
    [ 0b, 63b ], $
    [ 192b, 51b ], $
    [ 240b, 48b ], $
    [ 60b, 48b ], $
    [ 14b, 48b ], $
    [ 14b, 48b ], $
    [ 60b, 48b ], $
    [ 240b, 48b ], $
    [ 192b, 51b ], $
    [ 0b, 63b ], $
    [ 0b, 60b ], $
    [ 0b, 0b ], $
    [ 0b, 0b ]  $
  ]
  BMPBTN50 = WIDGET_BUTTON( BASE53,VALUE=BMP131, $
      FRAME=1, $
      UVALUE='playb')

; The next two buttons are commented out.  I thought that if anyone ever
; wanted to get the movie option working, the stop and pause bitmap buttons
; might be useful.  
;
; Stop Button
;  BMP133 = [ $
;    [ 0b, 0b ], $
;    [ 0b, 0b ], $
;    [ 252b, 63b ], $
;    [ 252b, 63b ], $
;    [ 12b, 48b ], $
;    [ 12b, 48b ], $
;    [ 12b, 48b ], $
;    [ 12b, 48b ], $
;    [ 12b, 48b ], $
;    [ 12b, 48b ], $
;    [ 12b, 48b ], $
;    [ 12b, 48b ], $
;    [ 252b, 63b ], $
;    [ 252b, 63b ], $
;    [ 0b, 0b ], $
;    [ 0b, 0b ]  $
;  ]
;  BMPBTN51 = WIDGET_BUTTON( BASE53,VALUE=BMP133, $
;      FRAME=1, $
;      UVALUE='stop')

; Pause Button
;  BMP135 = [ $
;    [ 0b, 0b ], $
;    [ 112b, 14b ], $
;    [ 112b, 14b ], $
;    [ 112b, 14b ], $
;    [ 112b, 14b ], $
;    [ 112b, 14b ], $
;    [ 112b, 14b ], $
;    [ 112b, 14b ], $
;    [ 112b, 14b ], $
;    [ 112b, 14b ], $
;    [ 112b, 14b ], $
;    [ 112b, 14b ], $
;    [ 112b, 14b ], $
;    [ 112b, 14b ], $
;    [ 112b, 14b ], $
;    [ 0b, 0b ]  $
;  ]
;  BMPBTN52 = WIDGET_BUTTON( BASE53,VALUE=BMP135, $
;      FRAME=1, $
;      UVALUE='pause')

; Slider for the z- value:
  SLIDER58 = WIDGET_SLIDER( BASE53, $
      DRAG=1, $
      MAXIMUM=state.size(2)-1, $
      MINIMUM=0, $
      TITLE='Z value', $
      UVALUE='ZSLIDER', $
      VALUE=0)
      state.widgetids(1)=SLIDER58


; Scaling Options (exclusive buttons):
  Btns298 = [ $
    'Frame by Frame', $
    'Entire Cube' ]
  BGROUP80 = CW_BGROUP( BASE53, Btns298, $
      COLUMN=1, $
      EXCLUSIVE=1, $
      LABEL_LEFT='Scaling:', $
      SET_VALUE=0,$
      UVALUE='SCALING')

; Print Buttons:
  Prints = WIDGET_BUTTON( BASE53, $
      FRAME = 0,  $
      VALUE = 'Print Spectrum', $
      UVALUE = 'Prints')

  Printi = WIDGET_BUTTON( BASE53, $
      FRAME = 0,  $
      VALUE = 'Print Image', $
      UVALUE = 'Printi')  

; Filename:
  BASEfilename = WIDGET_BASE(MAIN13, $
	COLUMN = 1, 	$
	SPACE=5,	$	
	XPAD=5,		$
	YPAD=5,		$
	TITLE = 'Filename',	$
	UVALUE='BASEfilename')

  LABELfilename = WIDGET_LABEL( BASEfilename, 	$
	VALUE= 'Filename: ', $
	UVALUE = 'TEXTfilename')

  TEXTfilename = WIDGET_TEXT(BASEfilename, 	$
	VALUE ='No File Loaded',	$
	UVALUE = 'filename',	$
	XSIZE = 15,		$
	YSIZE = 1)
      state.widgetids(10)=TEXTfilename	

; Base for pixel information
  BASE67 = WIDGET_BASE(MAIN13, $
      ROW=1, $
      SPACE=5, $
      XPAD=5, $
      YPAD=5, $
      FRAME=2, $
      MAP=1, $
      TITLE='Pixel Junk', $
      UVALUE='BASE67')

  FieldVal142 = [ $
      0 ]

; X position:
  BASExpos = WIDGET_BASE(BASE67, $
	COLUMN = 1, 	$
	SPACE=5,	$	
	XPAD=5,		$
	YPAD=5,		$
	TITLE = 'XPOSITION',	$
	UVALUE='BASExpos')

  TextVal300 = [ 'X Position:' ]	

  LABELXPOS = WIDGET_LABEL( BASExpos, 	$
	VALUE= 'X Position:', $
	UVALUE = 'TEXTXPOS')

  TEXTXPOS = WIDGET_TEXT(BASExpos, 	$
	VALUE ='0',	$
	UVALUE = 'XPOS',	$
	XSIZE = 7,		$
	YSIZE = 1)
      state.widgetids(5)=TEXTXPOS	
;print, 'xpos id before: ',state.widgetids(5)

;Y position:
  BASEypos = WIDGET_BASE(BASE67, $
	COLUMN = 1, 	$
	SPACE=5,	$	
	XPAD=5,		$
	YPAD=5,		$
	TITLE = 'YPOSITION',	$
	UVALUE='BASEypos')


  TextVal310 = [ 'Y Position:' ]	

  LABELYPOS = WIDGET_LABEL( BASEypos, 	$
	VALUE= 'Y Position:', $
	UVALUE = 'TEXTYPOS')

  TEXTYPOS = WIDGET_TEXT(BASEypos, 	$
	VALUE ='0',	$
	UVALUE = 'yPOS',	$
	XSIZE = 7,		$
	YSIZE = 1)
      state.widgetids(6)=TEXTYPOS	

; Velocity:
  BASEvel = WIDGET_BASE(BASE67, $
	COLUMN = 1, 	$
	SPACE=5,	$	
	XPAD=5,		$
	YPAD=5,		$
	TITLE = 'VELOCITY',	$
	UVALUE='BASEvel')


  TextVal320 = [ 'Velocity' ]	

  LABELvel = WIDGET_LABEL( BASEvel, 	$
	VALUE= 'Velocity: ', $
	UVALUE = 'TEXTvel')

  TEXTvel = WIDGET_TEXT(BASEvel, 	$
	VALUE ='0',	$
	UVALUE = 'textvel',	$
	XSIZE = 12,		$
	YSIZE = 1)
      state.widgetids(9)=TEXTvel	

; Data Value:
  BASEdata = WIDGET_BASE(BASE67, $
	COLUMN = 1, 	$
	SPACE=5,	$	
	XPAD=5,		$
	YPAD=5,		$
	TITLE = 'datavalue',	$
	UVALUE='BASEdata')


  TextVal330 = [ 'Data Value' ]	

  LABELdata = WIDGET_LABEL( BASEdata, 	$
	VALUE= 'Data Value', $
	UVALUE = 'TEXTdata')

  TEXTdata = WIDGET_TEXT(BASEdata, 	$
	VALUE ='0',	$
	UVALUE = 'data',	$
	XSIZE = 12,		$
	YSIZE = 1)

	state.widgetids(8)=TEXTdata



;Put the state structure where it belongs
;i.e. in the uvalue of the first child

widget_control, BGROUP92, set_uvalue = state



  WIDGET_CONTROL, MAIN13, /REALIZE

; Get drawable window index

  COMMON DRAW_Comm, DRAW_Id
  WIDGET_CONTROL, DRAW, GET_VALUE=DRAW_Id
  WSET, DRAW_Id

;display something
  loadct,3
;  tvscl,state.data(*,*,0)

  XMANAGER, 'MAIN13', MAIN13
END




