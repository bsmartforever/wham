;------------------------------------------------------------------------
; FTV.PRO
;
; This is the FUSE version of Aaron J. Barth's ATV, for displaying FITS
; images.
;
; If you downloaded this file from the FUSE website, please send problem
; reports to fuse_support@pha.jhu.edu.
;
; History
; 02/22/99  emurphy  -I could not reasonably modify the program to view 
;                     16384 by 1024 images.  However, I found that by
;                     allowing a non-square pan window and increasing 
;                     the overall size to 1024x512 made, the viewable
;                     area was OK for looking at FUSE images.
;                     I edited the program to use a
;                     state.pan_window_xsize and state.pan_window_ysize
;                     instead of a single state.pan_window_size.
;                    -Replaced all occurences of "atv" with "ftv".
;                    -Added GRBW color table.  
;                    -I flagged all changes with a note (search on emurphy).
; 06/02/99  oegerle  Changed fits file filter to look for files '*.fit'
; 06/03/99  oegerle  Changed fits reading from pull-down menu to
;                    use D. Sahnow's "readit.pro" routine, which supports
;                    reading binary tables and FUSE images
; 06/05/99  emurphy  Changed all i4 to i5 in format statements.
;                    changed atv special to Aaron Barth special.
; 06/07/99  emurphy  Added feature which draws box around image in pan window.
; 06/10/99  awf      Replaced the call to "ftv_readfits" with a call
;                    to "ftv_getfile", which reads FUSE photon lists 
;                    (UCB and OPUS formats) or images located in
;                    user-specified extensions.
;                    - Search on **FUSE to see all the changes that have
;                      been made to ATV.
; 6/14/99  ajb       Modified to include most recent changes to atv,
;                    and modified ftv_makepan to show full image
;                    when image is elongated horizontally.
; 08/30/99 emurphy   Modified to include most recent changes to atv.
; 09/14/99 emurphy   Added ablility to do a cheap and dirtly PS landscape plot
; 2006-02-15     wvd     Replaced all calls to findfile with file_search.
;
; SUGGESTIONS FOR FURTHER FUSE UPGRADES
;  06/10/99 - awf : Add file current file name to main widget
;                   Add fields for global min, max
;                   Add utility to create PS version of plot
;
;------------------------------------------------------------------------
;
; Here is the full header for the original ATV program:
;
;+
; NAME:
;       ATV
; 
; PURPOSE: 
;       Interactive display of 2-D images.
;
; CATEGORY: 
;       Image display.
;
; CALLING SEQUENCE:
;       atv [,array_name OR fits_file] [,min = min_value] [,max=max_value] 
;           [,/autoscale] [,/linear] [,/log] [,/histeq] 
;
; REQUIRED INPUTS:
;       None.  If atv is run with no inputs, the window widgets
;       are realized and images can subsequently be passed to atv
;       from the command line or from the pull-down file menu.
;
; OPTIONAL INPUTS:
;       array_name: a 2-D data array to display
;          OR
;       fits_file:  a fits file name, enclosed in single quotes
;
; KEYWORDS:
;       min:        minimum data value to be mapped to the color table
;       max:        maximum data value to be mapped to the color table
;       autoscale:  set min and max to show a range of data values
;                      around the median value
;       linear:     use linear stretch
;       log:        use log stretch 
;       histeq:     use histogram equalization
;       
; OUTPUTS:
;       None.  
; 
; COMMON BLOCKS:
;       atv_state:  contains variables describing the display state
;       atv_images: contains the internal copies of the display image
;       atv_color:  contains colormap vectors
;       atv_pdata:  contains plot and text annotation information
;
; RESTRICTIONS:
;       Requires IDL version 5.1 or greater.
;       Requires the GSFC IDL astronomy library routines,
;         for fits input.
;       For a current list of atv's bugs and weirdnesses, go to
;              http://cfa-www.harvard.edu/~abarth/atv/atv.html
;
; SIDE EFFECTS:
;       Modifies the color table.
;
; EXAMPLE:
;       To start atv running, just enter the command 'atv' at the idl
;       prompt, either with or without an array name or fits file name 
;       as an input.  Only one atv window will be created at a time,
;       so if one already exists and another image is passed to atv
;       from the idl command line, the new image will be displayed in 
;       the pre-existing atv window.
;
; MODIFICATION HISTORY:
;       Written by Aaron J. Barth, first release 17 December 1998.
;
;       Modified 26 Jul 1999 by D. Schlegel to add overplotting via
;       the routines ATVPLOT, ATVXYOUTS, ATVCONTOUR, and ATVERASE.
;       At present, these overplots are only in the main draw window.
;       The arguments C_ORIENTATION and C_SPACING are not supported.
;       The default color for all overplots is 'red'.
;
;       This version is 1.0b5, last modified 24 August 1999.
;       For the most current version, revision history, instructions,
;       and further information, go to:
;              http://cfa-www.harvard.edu/~abarth/atv/atv.html
;
;-
;----------------------------------------------------------------------

pro ftv_startup

; This routine initializes the ftv internal variables, and creates and
; realizes the window widgets.  It is only called by the ftv main
; program level, when there is no previously existing ftv window.

common ftv_state, state
common ftv_images, $
  main_image, $
  display_image, $
  scaled_image, $
  blink_image, $
  unblink_image, $  
  pan_image
common ftv_color, r_vector, g_vector, b_vector
common ftv_pdata, pdata, ptext, pcon, phistory

state = {                   $
          version: '1.0b5', $            ; version # of this release
          base_id: 0L, $                 ; id of top-level base
          base_min_size: [512L, 512L], $ ; min size for top-level base
          draw_base_id: 0L, $            ; id of base holding draw window
          draw_window_id: 0L, $          ; window id of draw window
          draw_widget_id: 0L, $          ; widget id of draw widget
          track_window_id: 0L, $         ; widget id of tracking window
          pan_window_id: 0L, $           ; widget id of pan window
          location_bar_id: 0L, $         ; id of (x,y,value) label
          min_text_id: 0L,  $            ; id of min= widget
          max_text_id: 0L, $             ; id of max= widget
          menu_ids: lonarr(25), $        ; list of top menu items
          colorbar_base_id: 0L, $        ; id of colorbar base widget
          colorbar_widget_id: 0L, $      ; widget id of colorbar draw widget
          colorbar_window_id: 0L, $      ; window id of colorbar
          colorbar_height: 6L, $         ; height of colorbar in pixels
          brightness: 500L, $           
          contrast: 500L, $
          slidermax: 1000L, $            ; max val for sliders (min=1)
          keyboard_text_id: 0L, $        ; id of keyboard input widget
          image_min: 0.0, $              ; min(main_image)
          image_max: 0.0, $              ; max(main_image)
          min_value: 0.0, $              ; min data value mapped to colors
          max_value: 0.0, $              ; max data value mapped to colors
          mode: 'color', $                ; Zoom, Blink, or Color
;**FUSE: emurphy modified the following line
          draw_window_size: [1024L, 512L], $    ; size of main draw window
          track_window_size: 121L, $     ; size of tracking window
;**FUSE: emurphy modified the following 2 lines, changed _size to _xsize and _ysize
          pan_window_xsize: 605L, $      ; x size of pan window
          pan_window_ysize: 121L, $      ; y size of pan window
          pan_scale: 0.0, $              ; magnification of pan image
          image_size: [0L,0L], $         ; size of main_image
          invert_colormap: 0L, $         ; 0=normal, 1=inverted
          mouse: [0L, 0L],  $            ; cursor position in image coords
          scaling: 0L, $                 ; 0=linear, 1=log, 2=histeq
          offset: [0L, 0L], $            ; offset to viewport coords
          base_pad: [0L, 0L], $          ; padding around draw base
          zoom_level: 0L, $              ; integer zoom level, 0=normal
          zoom_factor: 1.0, $            ; magnification factor = 2^zoom_level
          centerpix: [0L, 0L], $         ; pixel at center of viewport
          pan_track: 0B, $               ; flag=1 while mouse dragging
          cstretch: 0B, $                ; flag = 1 while stretching colors
          pan_offset: [0L, 0L], $        ; image offset in pan window
          lineplot_widget_id: 0L, $      ; id of lineplot widget
          lineplot_window_id: 0L, $      ; id of lineplot window
          lineplot_base_id: 0L, $        ; id of lineplot top-level base
          lineplot_size: [600L, 450L], $ ; size of lineplot window
          lineplot_pad: [0L, 0L], $      ; padding around lineplot window
          cursorpos: lonarr(2), $        ; cursor x,y for photometry
          centerpos: fltarr(2), $        ; centered x,y for photometry
          cursorpos_id: 0L, $            ; id of cursorpos widget
          centerpos_id: 0L, $            ; id of centerpos widget
          centerbox_id: 0L, $            ; id of centeringboxsize widget
          radius_id: 0L, $               ; id of radius widget
          innersky_id: 0L, $             ; id of inner sky widget
          outersky_id: 0L, $             ; id of outer sky widget
          skyresult_id: 0L, $            ; id of sky widget
          photresult_id: 0L, $           ; id of photometry result widget
          centerboxsize: 11L, $          ; centering box size
          r: 5L, $                       ; aperture photometry radius
          innersky: 10L, $               ; inner sky radius
          outersky: 20L  $               ; outer sky radius
        }

nmax = 5000L
pdata = {                               $
          nplot: 0L,                    $ ; Number of line plots
          nmax:  nmax,                  $ ; Maximum number of line plots
          x: replicate(ptr_new(),nmax), $ ; X vector
          y: replicate(ptr_new(),nmax), $ ; Y vector
          color: lonarr(nmax),          $ ; COLOR for PLOT
          psym: lonarr(nmax),           $ ; PSYM for PLOT
          symsize: fltarr(nmax),        $ ; PSYM for PLOT
          thick: fltarr(nmax)           $ ; THICK for PLOT
        }

nmax = 500L
ptext = {                               $
          nplot: 0L,                    $ ; Number of text plots
          nmax:  nmax,                  $ ; Maximum number of text plots
          x: fltarr(nmax),              $ ; X position
          y: fltarr(nmax),              $ ; Y position
          string: replicate(ptr_new(),nmax), $ ; Text string
          alignment: fltarr(nmax),      $ ; ALIGNMENT for XYOUTS
          charsize: fltarr(nmax),       $ ; CHARSIZE for XYOUTS
          charthick: fltarr(nmax),      $ ; CHARTHICK for XYOUTS
          color: lonarr(nmax),          $ ; COLOR for XYOUTS
          font: lonarr(nmax),           $ ; FONT for XYOUTS
          orientation: fltarr(nmax)     $ ; ORIENTATION for XYOUTS
        }

nmax = 10L
pcon = {                                $
          nplot: 0L,                    $ ; Number of contour plots
          nmax:  nmax,                  $ ; Maximum number of contour plots
          z: replicate(ptr_new(),nmax), $ ; Z image
          x: replicate(ptr_new(),nmax), $ ; X vector
          y: replicate(ptr_new(),nmax), $ ; Y vector
          c_annotation: replicate(ptr_new(),nmax), $ ; C_ANNOTATION for CONTOUR
          c_charsize: fltarr(nmax),     $ ; C_CHARSIZE for CONTOUR
          c_charthick: fltarr(nmax),    $ ; C_CHARTHICK for CONTOUR
          c_colors: replicate(ptr_new(),nmax), $ ; C_COLORS for CONTOUR
          c_labels: replicate(ptr_new(),nmax), $ ; C_LABELS for CONTOUR
          c_linestyle: replicate(ptr_new(),nmax), $ ; C_LINESTYLE for CONTOUR
          c_orientation: fltarr(nmax),  $ ; C_ORIENTATION for CONTOUR
          c_spacing: fltarr(nmax),      $ ; C_SPACING for CONTOUR
          c_thick: replicate(ptr_new(),nmax), $ ; C_THICK for CONTOUR
          cell_fill: intarr(nmax),      $ ; CELL_FILL for CONTOUR
          closed: intarr(nmax),         $ ; CLOSED for CONTOUR
          downhill: intarr(nmax),       $ ; DOWNHILL for CONTOUR
          fill: intarr(nmax),           $ ; FILL for CONTOUR
          irregular: intarr(nmax),      $ ; IRREGULAR for CONTOUR
          levels: replicate(ptr_new(),nmax), $ ; LEVELS for CONTOUR
          max_value: fltarr(nmax),      $ ; MAX_VALUE for CONTOUR
          min_value: fltarr(nmax),      $ ; MIN_VALUE for CONTOUR
          nlevels: lonarr(nmax)         $ ; NLEVELS for CONTOUR
        }

phistory = intarr(pdata.nmax + ptext.nmax + pcon.nmax)

; Read in a color table to initialize !d.table_size
; As a bare minimum, we need the 8 basic colors used by FTV_ICOLOR(),
; plus 2 more for a color map.

loadct, 0, /silent
if (!d.table_size LT 10) then begin
    message, 'Too few colors available for color table'
    ftv_shutdown
endif

; Define the widgets.  For the widgets that need to be modified later
; on, save their widget ids in state variables

base = widget_base(title = 'ftv', $
                   /column, /base_align_right, $
                   app_mbar = top_menu, $
                   uvalue = 'ftv_base', $
                   /tlb_size_events)
state.base_id = base

tmp_struct = {cw_pdmenu_s, flags:0, name:''}

;**FUSE: emurphy added new color table GRBW below
;**FUSE: emurphy added {cw_pdmenu_s, 0, 'Write_PS_Landscape'},  $

top_menu_desc = [ $
                  {cw_pdmenu_s, 1, 'File'}, $         ; file menu
                  {cw_pdmenu_s, 0, 'ReadFits'}, $
                  {cw_pdmenu_s, 0, 'WriteEPS'},  $
                  {cw_pdmenu_s, 0, 'Write_PS_Landscape'},  $
                  {cw_pdmenu_s, 0, 'WriteTiff'}, $
                  {cw_pdmenu_s, 2, 'Quit'}, $
                  {cw_pdmenu_s, 1, 'ColorMap'}, $     ; color menu
                  {cw_pdmenu_s, 0, 'Grayscale'}, $
                  {cw_pdmenu_s, 0, 'Blue-White'}, $
                  {cw_pdmenu_s, 0, 'GRBW'}, $
                  {cw_pdmenu_s, 0, 'Red-Orange'}, $
                  {cw_pdmenu_s, 0, 'Rainbow'}, $
                  {cw_pdmenu_s, 0, 'BGRY'}, $
                  {cw_pdmenu_s, 0, 'Stern Special'}, $
                  {cw_pdmenu_s, 2, 'FTV Special'}, $
                  {cw_pdmenu_s, 1, 'Scaling'}, $      ; scaling menu
                  {cw_pdmenu_s, 0, 'Linear'}, $
                  {cw_pdmenu_s, 0, 'Log'}, $
                  {cw_pdmenu_s, 2, 'HistEq'}, $
                  {cw_pdmenu_s, 1, 'Labels'}, $       ; labels menu
                  {cw_pdmenu_s, 0, 'TextLabel'}, $
                  {cw_pdmenu_s, 0, 'Contour'}, $
                  {cw_pdmenu_s, 0, 'EraseLast'}, $
                  {cw_pdmenu_s, 2, 'EraseAll'}, $
                  {cw_pdmenu_s, 1, 'Help'}, $         ; help menu
                  {cw_pdmenu_s, 2, 'FTV Help'} $
                ]

top_menu = cw_pdmenu(top_menu, top_menu_desc, $
                     ids = state.menu_ids, $
                     /mbar, $
                     /help, $
                     /return_id, $
                     uvalue = 'top_menu')

track_base =    widget_base(base, /row)
info_base = widget_base(track_base, /column, /base_align_right)
button_base1 =  widget_base(base, /row, /base_align_bottom)
button_base2 =  widget_base(base, /row, /base_align_bottom)
;**FUSE emurphy added the following line.
track_base_2 =  widget_base(track_base, /row, /base_align_bottom)

state.draw_base_id = $
  widget_base(base, $
              /column, /base_align_left, $
              /tracking_events, $
              uvalue = 'draw_base', $
              frame = 2)

state.colorbar_base_id = $
  widget_base(base, $
              uvalue = 'colorbar_base', $
              /column, /base_align_left, $
              frame = 2)

state.min_text_id = $
  cw_field(info_base, $
           uvalue = 'min_text', $
           /floating,  $
           title = 'Min=', $
           value = state.min_value,  $
           /return_events, $
           xsize = 12)

state.max_text_id = $
  cw_field(info_base, $
           uvalue = 'max_text', $
           /floating,  $
           title = 'Max=', $
           value = state.max_value, $
           /return_events, $
           xsize = 12)

;**FUSE: emurphy changed the following lines from i4 to i5
tmp_string = string(1000, 1000, 1.0e-10, $
                    format = '("(",i5,",",i5,") ",g12.5)' )

state.location_bar_id = $
  widget_label (info_base, $
                value = tmp_string,  $
                uvalue = 'location_bar',  frame = 1)

;**FUSE: emurphy changed the following lines from _size to _xsize and _ysize
pan_window = $
  widget_draw(track_base, $
              xsize = state.pan_window_xsize, $
              ysize = state.pan_window_ysize, $
              frame = 2, uvalue = 'pan_window', $
              /button_events, /motion_events)

track_window = $
  widget_draw(track_base, $
              xsize=state.track_window_size, $
              ysize=state.track_window_size, $
              frame=2, uvalue='track_window')


modelist = ['Color', 'Zoom', 'Blink']
mode_droplist_id = widget_droplist(button_base1, $
                                   frame = 1, $
                                   title = 'MouseMode:', $
                                   uvalue = 'mode', $
                                   value = modelist)

invert_button = $
  widget_button(button_base1, $
                value = 'Invert', $
                uvalue = 'invert')

reset_button = $
  widget_button(button_base1, $
                value = 'ResetColor', $
                uvalue = 'reset_color')

autoscale_button = $
  widget_button(button_base1, $
                uvalue = 'autoscale_button', $
                value = 'AutoScale')

fullrange_button = $
  widget_button(button_base1, $
                uvalue = 'full_range', $
                value = 'FullRange')

state.keyboard_text_id = $
  widget_text(button_base2, $
              /all_events, $
              scr_xsize = 1, $
              scr_ysize = 1, $
              units = 0, $
              uvalue = 'keyboard_text', $
              value = '')

zoomin_button = $
  widget_button(button_base2, $
                value = 'ZoomIn', $
                uvalue = 'zoom_in')

zoomout_button = $ 
  widget_button(button_base2, $
                value = 'ZoomOut', $
                uvalue = 'zoom_out')

zoomone_button = $
  widget_button(button_base2, $
                value = 'Zoom1', $
                uvalue = 'zoom_one')

center_button = $
  widget_button(button_base2, $
                value = 'Center', $
                uvalue = 'center')

setblink_button = $
  widget_button(button_base2, $
                uvalue = 'set_blink', $
                value = 'SetBlink')

done_button = $
  widget_button(button_base2, $
                value = 'Done', $
                uvalue = 'done')

state.draw_widget_id = $
  widget_draw(state.draw_base_id, $
              uvalue = 'draw_window', $
              /motion_events,  /button_events, $
              scr_xsize = state.draw_window_size[0], $
              scr_ysize = state.draw_window_size[1]) 

state.colorbar_widget_id = $
  widget_draw(state.colorbar_base_id, $
              uvalue = 'colorbar', $
              scr_xsize = state.draw_window_size[0], $
              scr_ysize = state.colorbar_height)

; Create the widgets on screen

widget_control, base, /realize

; get the window ids for the draw widgets

widget_control, track_window, get_value = tmp_value
state.track_window_id = tmp_value
widget_control, state.draw_widget_id, get_value = tmp_value
state.draw_window_id = tmp_value
widget_control, pan_window, get_value = tmp_value
state.pan_window_id = tmp_value
widget_control, state.colorbar_widget_id, get_value = tmp_value
state.colorbar_window_id = tmp_value

; Find window padding sizes needed for resizing routines.
; Add extra padding for menu bar, since this isn't included in 
; the geometry returned by widget_info.
; Also add extra padding for margin (frame) in draw base.

basegeom = widget_info(state.base_id, /geometry)
drawbasegeom = widget_info(state.draw_base_id, /geometry)

state.base_pad[0] = basegeom.xsize - drawbasegeom.xsize $
  + (2 * basegeom.margin)
state.base_pad[1] = basegeom.ysize - drawbasegeom.ysize + 30 $
  + (2 * basegeom.margin)

state.base_min_size = [state.base_pad[0] + 512, state.base_pad[1] + 100]

; Initialize the vectors that hold the current color table.
; See the routine ftv_stretchct to see why we do it this way.

r_vector = bytarr(!d.table_size-8)
g_vector = bytarr(!d.table_size-8)
b_vector = bytarr(!d.table_size-8)

ftv_getct, 0
state.invert_colormap = 0

ftv_colorbar

xmanager, 'ftv', state.base_id, /no_block

end

;--------------------------------------------------------------------

pro ftv_colorbar

; Routine to tv the colorbar at the bottom of the ftv window
; Added by AJB 8/19/99

common ftv_state

wset, state.colorbar_window_id

xsize = (widget_info(state.colorbar_widget_id, /geometry)).xsize

d = !d.table_size - 8

b = congrid( findgen(d), xsize) + 8
c = replicate(1, state.colorbar_height)
a = b # c

tv, a

end

;--------------------------------------------------------------------

pro ftv_displayall

; Call the routines to scale the image, make the pan image, and
; re-display everything.  Use this if the scaling changes (log/
; linear/ histeq), or if min or max are changed, or if a new image is
; passed to ftv.  If the display image has just been moved around or
; zoomed without a change in scaling, then just call ftv_refresh
; rather than this routine.

ftv_scaleimage
ftv_makepan
ftv_refresh

end

;**FUSE: begin addition of Alex Fullerton's module to read FUSE files.

;----------------------------------------------------------------------
PRO FTV_GETFILE_EVENT, ev
;  
; Event handler for the file-specification widget "FTV_GETFILE".
; See it for more detailed documentation.
;
; The operations to be performed are specified by the user value, $
; which is passed as the structure "sstate."  These actions are
; summarized as follows:
;
; =========================================================================
;   UVALUE                  Purpose / Action
; -------------------------------------------------------------------------  
;   fselect      editable field to indicate current file selection
;   filetype     specify "Photon List" or "Image" format
;                   Photon List is specific to FUSE (a binary table)
;                   Image is any 2D image in a valid FITS file
;   extnum       specify the extension number where the image resides
;                   disabled if filetype='Photon List' (FUSE lists are 
;                                                       always in HDU1)
;                   enabled if filetype = 'Image'
;   sel_file     calls browser to select file to load
;                   filters for files with extension ".fit"
;   load_file    load the specified file into FTV
;                   converts photon lists to images first
; --------------------------------------------------------------------------
;
; COMMON BLOCKS:
;   ftv_state, ftv_images, ftv_filselect
;
; HISTORY:
; 1999-06-10: Written by Alex Fullerton (FUSE/JHU).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Define common blocks.  
    COMMON ftv_state
    COMMON ftv_images
    COMMON ftv_filselect, fval
  
    WIDGET_CONTROL, ev.top, GET_UVALUE = sstate
    WIDGET_CONTROL, ev.id,  GET_UVALUE = uval

; Use the event's user value to take the appropriate action.
     CASE uval OF
        'fselect': BEGIN
	
                     ;No-op

	           END

       'filetype': BEGIN
                     fnum = ev.index

                     IF ( fnum EQ 1) THEN BEGIN          ; image file

			 ; Enable HDU selection
			 WIDGET_CONTROL, sstate.enum, /SENSITIVE, $
			                 SET_DROPLIST_SELECT=0

		     ENDIF ELSE BEGIN                    ; TTAG list

			 ; Disable HDU selection
			 WIDGET_CONTROL, sstate.enum, SENSITIVE = 0, $
		        		 SET_DROPLIST_SELECT = 1
			 
		     ENDELSE
			
	           END

	 'extnum': BEGIN

                     enum = ev.index        ; No-Op

	           END
		  
       'sel_file': BEGIN
            
                     ; Select the file
                     fitsfile = DIALOG_PICKFILE( filter= '*.fit',  $
		        		  ;       GROUP =        , $
					          /MUST_EXIST,     $
					          /READ,           $
						  TITLE=' Select FITS File' )

		      ; Get the size of the file name
		      nn = STRLEN( fitsfile )
		      IF (nn LT 26) THEN nn = 26

		      ; Update the dialog screen: scroll to the right
		      WIDGET_CONTROL, sstate.cfile, SET_VALUE = fitsfile,$
			              SET_TEXT_SELECT = [nn-26, 26]

	           END
		  
      'load_file': BEGIN

                     ; Get current settings of filetype, extension, file
                     fnum = WIDGET_INFO( sstate.ftype, /DROPLIST_SELECT )
                     enum = WIDGET_INFO( sstate.enum, /DROPLIST_SELECT ) 
                     WIDGET_CONTROL, sstate.cfile, GET_VALUE=filin

                     filin = filin[0]
                       
                     ; Trap for errors due to undefined file.
                     IF ( (filin EQ '   No current selection   ') OR $
                          ( STRLEN( filin ) EQ 0 ) ) THEN BEGIN
                         
                           emess =  'ERROR: file not selected!'
                           etemp =  DIALOG_MESSAGE( emess,  /ERROR )

                     ENDIF ELSE BEGIN

                       IF ( STRTRIM(fval[fnum],2) EQ 'Photon List' ) THEN BEGIN

                           tmp_image = readit( filin, fheader = hd )

		       ENDIF ELSE BEGIN
			 
		           tmp_image = mrdfits( filin, enum, hd, /fscale )

		       ENDELSE

		       ; The rest of the program is copied from FTV.
		       IF ( (SIZE( tmp_image))[0] NE 2 ) THEN BEGIN

			    mesg = 'WARNING-- selected file is not a 2-D'
			    mesg = mesg + ' FITS image!'
                            tmp_result = DIALOG_MESSAGE( mesg, /ERROR )
		                        ; DIALOG_PARENT= ,             )
			    
		        ENDIF ELSE BEGIN

			    main_image = TEMPORARY( tmp_image )
                            tmp_image  = 0
			  
			    ftv_getstats
			    state.zoom_level  = 0
			    state.zoom_factor = 1.0
                            ftv_scaleimage
			    WIDGET_CONTROL, state.min_text_id, $
				          SET_VALUE = STRING(state.min_value)
			    WIDGET_CONTROL, state.max_text_id, $
				            SET_VALUE = STRING(state.max_value)
			    ftv_makepan
			    ftv_refresh
			    
		        ENDELSE

                        WIDGET_CONTROL, state.draw_base_id, /CLEAR_EVENTS
		        WIDGET_CONTROL, state.keyboard_text_id, SET_VALUE=''

		        ; Destroy the widget upon successful load.
		        WIDGET_CONTROL, ev.top ,/DESTROY
                     ENDELSE
	           END

    ENDCASE		  
      
    END 

;----------------------------------------------------------------------
PRO FTV_GETFILE
;+
; NAME: FTV_GETFILE
;
; PURPOSE:
;      Widget to select, specify, and load data from a FITS file into
;      FTV.  The binary tables associated with FUSE photon lists
;      are first converted to images.  2D images located in
;      arbitary FITS extensions can be also be accessed.
;
; CATEGORY:
;      Widgets: file selection.
;
; CALLING SEQUENCE:
;      FTV_GETFILE
;
; INPUTS:
;      None.
;
; KEYWORD PARAMETERS:
;      None.
;
; OUTPUTS:
;      None.
;  
; COMMON BLOCKS:
;      ftv_state, ftv_images, ftv_filselect (defined here)
;
; SIDE EFFECTS:
;      This widget permits the user to select a FITS file, $
;      specify its format ("Photon List" or "Image"), specify the extension
;      in which the desired image resides, and load the data
;      into FTV for analysis.
;
; RESTRICTIONS:
;      The maximum HDU that can be selected is 19, where the PDU = 0.
;
; OTHER ROUTINES CALLED:
;      Including calls made in the associated event handler, FTV_GETFILE_EVENT
;      IDL Astro User's Library:  mrdfits
;      David Sahnow's Library:    readit
;      FTV suite:                 ftv_getstats, ftv_scale_image, ftv_makepan,
;                                 ftv_refresh 
;
; PROCEDURE:
;      Straightforward use of native IDL widgets and auxiliary file
;      reading routines.
;
; MODIFICATION HISTORY
;      1999-06-10:   Written by Alex Fullerton (FUSE / JHU).
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Define the common blocks
    COMMON ftv_state
    COMMON ftv_images
    COMMON ftv_filselect, fval

; Define the base widget    
    base    = WIDGET_BASE( title = 'File Selection Menu', /COLUMN )

; Define a labelled field to indicate the current file selection.
; Although this field can be editted, it will usually be populated
; via the "pickfile" dialog box, which is activated by the
; "File->Readfits" menu.

    clab    = WIDGET_LABEL( base, VALUE='Selection:', /ALIGN_CENTER ) 

    cfile   = WIDGET_TEXT( base, XSIZE = 26, /SCROLL, /EDITABLE, $
			   VALUE = '   No current selection   ', $
                           UVALUE = 'fselect'                    )   

; Create a droplist widget to toggle between file formats.
; The default is "Photon List", which will be read by Dave Sahnow's 
; "readit" procedure.
    fval    = ['Photon List', 'Image      ']
    ftype   = WIDGET_DROPLIST( base, VALUE = fval,         $
			       TITLE = 'Format:    ',      $
			       UVALUE = 'filetype' )

; Create a droplist widget to specify the extension number to be read.
; This information will only be used when reading "IMAGE" formats.
     eval    = STRTRIM( STRING( INDGEN( 20 ) ), 2)
     enum    = WIDGET_DROPLIST( base, VALUE = eval,        $
		                TITLE = 'FITS HDU:  ',     $
			        UVALUE = 'extnum'          )

; Create a button to enable the "dialog_pickfile" browser.
     sbut    = WIDGET_BUTTON( base,                        $
			      VALUE = 'Select File',       $
			      UVALUE = 'sel_file'          )

; Create a button to load the specified extension of the selected
; file into ftv.  If the file type is "TTAG", then an image version
; of the photon list will be created first.
     lbut    = WIDGET_BUTTON( base,                        $
			      VALUE = 'Load Selection',    $
			      UVALUE= 'load_file'          )

; Define a structure that contains the widget identifiers associated with
; each of the fields/buttons.  This will be passed to the event handler.
    sstate = { cfile:cfile, $
               ftype:ftype, $
               enum: enum,  $
               sbut: sbut,  $
               lbut: lbut   }

; Realize the widget and initialize the current file selection window.
    WIDGET_CONTROL, base, /REALIZE, SET_UVALUE = sstate, $
                    TLB_SET_XOFFSET = 5

; Disable the extension specification if the file type is "Photon List".
    fnum = WIDGET_INFO( ftype, /DROPLIST_SELECT )
    IF ( fnum EQ 0 ) THEN WIDGET_CONTROL, enum, SENSITIVE = 0,   $
	                                  SET_DROPLIST_SELECT = 1
    
; Hand off control to the XMANAGER.
    XMANAGER, 'ftv_getfile', base, GROUP_LEADER = state.base_id
    
    END

;----------------------------------------------------------------------
;**FUSE: end addition of Alex Fullerton's module to read FUSE files.


pro ftv_readfits

; Read in a new image when user goes to the File->ReadFits menu.
; Can be modified to use mrdfits if fits extensions are used.

common ftv_state
common ftv_images

fitsfile = $
  dialog_pickfile(filter = '*.fits', $
                  group = state.base_id, $
                  /must_exist, $
                  /read, $
                  title = 'Select Fits Image')
                        
if (fitsfile NE '') then begin  ; 'cancel' button returns empty string

; note:  found that "readfits" chokes on some non-standard 
; fits files, but "fits_read" handles them ok.
    
;**FUSE: oegerle changed the FITS reader to understand FUSE TTAG files  
;    fits_read, fitsfile, tmp_image
     tmp_image = readit(fitsfile)

    if ( (size(tmp_image))[0] NE 2 ) then begin
        mesg = 'Warning-- selected file is not a 2-D fits image!'
        tmp_result = dialog_message(mesg, /error, $
                                    dialog_parent = state.base_id)

; If the new image is valid, put it into main_image

    endif else begin
        main_image = temporary(tmp_image)
        ftv_getstats
        ftv_settitle, fitsfile
        state.zoom_level =  0
        state.zoom_factor = 1.0
        ftv_set_minmax
        ftv_displayall
    endelse


    ftv_cleartext
endif

end

;----------------------------------------------------------------------

pro ftv_writetiff

; writes a tiff image of the current display

common ftv_state
common ftv_images

; Get filename to save image

filename = dialog_pickfile(filter = '*.tiff', $ 
                           file = 'ftv.tiff', $
                          group =  state.base_id, $
                          /write)

tmp_result = file_search(filename, count = nfiles)

result = ''
if (nfiles GT 0 and filename NE '') then begin
    mesg = strarr(2)
    mesg[0] = 'Overwrite existing file:'
    tmp_string = strmid(filename, rstrpos(filename, '/') + 1)
    mesg[1] = strcompress(tmp_string + '?', /remove_all)
    result =  dialog_message(mesg, $
                             /default_no, $
                             dialog_parent = state.base_id, $
                             /question)                 
endif

if ((nfiles EQ 0 OR result EQ 'Yes') AND filename NE '') then begin

    wset, state.draw_window_id

    tvlct, rr, gg, bb, /get

    rn = congrid(rr[8:!d.table_size-1], 248)
    gn = congrid(gg[8:!d.table_size-1], 248)
    bn = congrid(bb[8:!d.table_size-1], 248)

    rvec = bytarr(256)
    gvec = bytarr(256)
    bvec = bytarr(256)

    rvec[0] = rr  ; load in the first 8 colors
    gvec[0] = gg
    bvec[0] = bb

    rvec[8] = temporary(rn)
    gvec[8] = temporary(gn)
    bvec[8] = temporary(bn)

    write_tiff, filename, tvrd(), $
      red = temporary(rvec), $
      green = temporary(gvec), $
      blue = temporary(bvec)
endif

ftv_cleartext

end


;----------------------------------------------------------------------

pro ftv_writeps

; writes an encapsulated postscript file of the current display, set
; to a width of 6 inches

common ftv_state
common ftv_images

widget_control, /hourglass

filename = dialog_pickfile(filter = '*.eps', $ 
                           file = 'ftv.eps', $
                          group =  state.base_id, $
                          /write)

tmp_result = file_search(filename, count = nfiles)

result = ''
if (nfiles GT 0 and filename NE '') then begin
    mesg = strarr(2)
    mesg[0] = 'Overwrite existing file:'
    tmp_string = strmid(filename, rstrpos(filename, '/') + 1)
    mesg[1] = strcompress(tmp_string + '?', /remove_all)
    result =  dialog_message(mesg, $
                             /default_no, $
                             dialog_parent = state.base_id, $
                             /question)                 
endif

if ((nfiles EQ 0 OR result EQ 'Yes') AND filename NE '') then begin

    
    screen_device = !d.name
    tvlct, rr, gg, bb, 8, /get
    wset, state.draw_window_id

    aspect_ratio = $
      state.draw_window_size[1] / float(state.draw_window_size[0])
    
    set_plot, 'ps'
    device, $
      filename = filename, $
      /color, $
      bits_per_pixel = 8, $
      /encapsul, $
      /inches, $
      xsize = 6.0, $
      ysize = 6.0 * aspect_ratio
    
    rn = congrid(rr, 248)
    gn = congrid(gg, 248)
    bn = congrid(bb, 248)
    
    tvlct, temporary(rn), temporary(gn), temporary(bn), 8
    tv, bytscl(display_image, top = 247) + 8
    ftv_plotall

    device, /close
    set_plot, screen_device
    tvlct, temporary(rr), temporary(gg), temporary(bb), 8
endif

ftv_cleartext

end

;----------------------------------------------------------------------
;**FUSE: emm added the following subroutine ftv_write_ps_landscape

pro ftv_write_ps_landscape

; writes a Landscape postscript file of the current display, set
; to a width of 10 inches

common ftv_state
common ftv_images

widget_control, /hourglass

filename = dialog_pickfile(filter = '*.ps', $ 
                           file = 'ftv.ps', $
                          group =  state.base_id, $
                          /write)

tmp_result = file_search(filename, count = nfiles)

result = ''
if (nfiles GT 0 and filename NE '') then begin
    mesg = strarr(2)
    mesg[0] = 'Overwrite existing file:'
    tmp_string = strmid(filename, rstrpos(filename, '/') + 1)
    mesg[1] = strcompress(tmp_string + '?', /remove_all)
    result =  dialog_message(mesg, $
                             /default_no, $
                             dialog_parent = state.base_id, $
                             /question)                 
endif

if ((nfiles EQ 0 OR result EQ 'Yes') AND filename NE '') then begin

    
    screen_device = !d.name
    tvlct, rr, gg, bb, 8, /get
    wset, state.draw_window_id

    aspect_ratio = $
      state.draw_window_size[1] / float(state.draw_window_size[0])
    
    set_plot, 'ps'
    device, $
      filename = filename, $
      /color, $
      bits_per_pixel = 8, $
      /landscape, $
      /inches, $
      xsize = 10.0, $
      ysize = 10.0 * aspect_ratio
    
    rn = congrid(rr, 248)
    gn = congrid(gg, 248)
    bn = congrid(bb, 248)
    
    tvlct, temporary(rn), temporary(gn), temporary(bn), 8
    tv, bytscl(display_image, top = 247) + 8

    ftv_plotall

    device, /close
    set_plot, screen_device
    tvlct, temporary(rr), temporary(gg), temporary(bb), 8
endif

ftv_cleartext

end

;**FUSE: end of subroutine ftv_write_ps_landscape;
;----------------------------------------------------------------------

pro ftv_cleartext

; Routine to clear the widget for keyboard input when the mouse is in
; the text window.  This de-allocates the input focus from the text
; input widget.

common ftv_state

widget_control, state.draw_base_id, /clear_events
widget_control, state.keyboard_text_id, set_value = ''

end

;----------------------------------------------------------------------

pro ftv_getoffset
common ftv_state

; Routine to calculate the display offset for the current value of
; state.centerpix, which is the central pixel in the display window.

state.offset = $
  round( state.centerpix - $
         (0.5 * state.draw_window_size / state.zoom_factor) )

end

;----------------------------------------------------------------------

pro ftv_zoom, zchange, recenter = recenter
common ftv_state

; Routine to do zoom in/out and recentering of image

case zchange of
    'in':    state.zoom_level = (state.zoom_level + 1) < 6
    'out':   state.zoom_level = (state.zoom_level - 1) > (-6) 
    'one':   state.zoom_level =  0
    'none':  ; no change to zoom level: recenter on current mouse position
    else:  print,  'problem in ftv_zoom!'
endcase

state.zoom_factor = 2.^state.zoom_level

if (n_elements(recenter) GT 0) then begin
    state.centerpix = state.mouse
    ftv_getoffset
endif

ftv_refresh

if (n_elements(recenter) GT 0) then begin
    newpos = (state.mouse - state.offset + 0.5) * state.zoom_factor
    wset,  state.draw_window_id
    tvcrs, newpos[0], newpos[1], /device 
    ftv_gettrack
endif


end

;----------------------------------------------------------------------

function ftv_polycolor, p

; Routine to return an vector of length !d.table_size-8,
; defined by a 5th order polynomial.   Called by ftv_makect
; to define new color tables in terms of polynomial coefficients.

x = findgen(256)

y = p[0] + x * p[1] + x^2 * p[2] + x^3 * p[3] + x^4 * p[4] + x^5 * p[5]

w = where(y GT 255, nw)
if (nw GT 0) then y(w) = 255

w =  where(y LT 0, nw)
if (nw GT 0) then y(w) = 0

z = congrid(y, !d.table_size-8)

return, z
end

;----------------------------------------------------------------------

pro ftv_makect, tablename

; Define new color tables here, in terms of 5th order polynomials.
; To define a new color table, first set it up using xpalette,
; then load current color table into 3 256-element vectors, and
; do a 5th order poly_fit.  Store the coefficients and name
; the color table here.  Invert if necessary.

common ftv_state
common ftv_color

case tablename of
    'FTV Special': begin
 
        r = ftv_polycolor([39.4609, $
                           -5.19434, $
                           0.128174, $
                           -0.000857115, $
                           2.23517e-06, $
                           -1.87902e-09])
        
        g = ftv_polycolor([-15.3496, $
                           1.76843, $
                           -0.0418186, $
                           0.000308216, $
                           -6.07106e-07, $
                           0.0000])
        
        b = ftv_polycolor([0.000, $ 
                           12.2449, $
                           -0.202679, $
                           0.00108027, $
                           -2.47709e-06, $
                           2.66846e-09])
   end

; add more color table definitions here as needed...
    else:

endcase

tvlct, r, g, b, 8

if (state.invert_colormap EQ 1) then begin
    r = abs (r - 255)
    g = abs (g - 255)
    b = abs (b - 255)
endif

r_vector = temporary(r)
g_vector = temporary(g)
b_vector = temporary(b)
    
ftv_stretchct, state.brightness, state.contrast

end

;----------------------------------------------------------------------

pro ftv_getstats

; Get basic image stats: min and max, and size.

common ftv_state
common ftv_images

; this routine operates on main_image, which is in the
; ftv_images common block

widget_control, /hourglass

state.image_size = [ (size(main_image))[1], (size(main_image))[2] ]

state.image_min = min(main_image)
state.image_max = max(main_image)

state.min_value = state.image_min
state.max_value = state.image_max

if (state.min_value GE state.max_value) then begin
    state.min_value = state.min_value - 1
    state.max_value = state.max_value + 1
endif

; zero the current display position on the center of the image

state.mouse = round(state.image_size / 2.)
state.centerpix = round(state.image_size / 2.)
ftv_getoffset

; Clear all plot annotations
ftverase, /norefresh  

end
 
;---------------------------------------------------------------------

pro ftv_settitle, imagename
; Update title bar: added by AJB 8/20/99 

; Note to self: for future revisions, it will probably be better to
; keep imagename in a state variable, and to move the state structure
; definition to the main ftv.pro routine.  That way, fits header
; information can also be saved in a state variable at the same time.

common ftv_state

if (n_elements(imagename) EQ 0) then imagename = ''

if (imagename EQ '') then begin
    widget_control, state.base_id, tlb_set_title = 'ftv'
endif else begin
    slash = rstrpos(imagename, '/')
    if (slash NE -1) then name = strmid(imagename, slash+1) $
      else name = imagename
    title = strcompress('ftv:  '+name)
    widget_control, state.base_id, tlb_set_title = title
endelse

end

;----------------------------------------------------------------------

pro ftv_gettrack

; Create the image to display in the track window that tracks
; cursor movements.

common ftv_state
common ftv_images

; Get x and y for center of track window

zcenter = (0 > state.mouse < state.image_size)

track_image = $
  rebin(scaled_image[zcenter[0]:zcenter[0]+10,  $
                     zcenter[1]:zcenter[1]+10], $
        state.track_window_size, state.track_window_size, $
        /sample)

wset, state.track_window_id
tv, track_image

; Overplot an X on the central pixel in the track window, to show the
; current mouse position

; Changed central x to be green always
plots, [0.46, 0.54], [0.46, 0.54], /normal, color = 2
plots, [0.46, 0.54], [0.54, 0.46], /normal, color = 2

; update location bar with x, y, and pixel value

;**FUSE: emurphy changed the following lines from i4 to i5
loc_string = $
  string(state.mouse[0], $
         state.mouse[1], $
         main_image[state.mouse[0], $
                    state.mouse[1]], $
         format = '("(",i5,",",i5,") ",g12.5)') 
widget_control, state.location_bar_id, $
  set_value = loc_string

end

;----------------------------------------------------------------------

pro ftv_event, event

; Main event loop for FTV widgets.

common ftv_state
common ftv_images
common ftv_color

widget_control, event.id, get_uvalue = uvalue

case uvalue of

    'mode': case event.index of
        0: state.mode = 'color'
        1: state.mode = 'zoom'
        2: state.mode = 'blink'
        else: print, 'Unknown mouse mode!'
    endcase

    'ftv_base': begin  ; main window resize: preserve display center
        ftv_resize, event
        ftv_refresh
        ftv_cleartext
    end

    'top_menu': begin       ; selection from menu bar
        widget_control, event.value, get_value = event_name
        
        case event_name of
            
; File menu options:
;**FUSE: awf substituted "ftv_getfile" for "ftv_readfits"
;**FUSE: emm Added 'Write_PS_Landscape' :  ftv_write_ps_landscape

;           'ReadFits': ftv_readfits
            'ReadFits': ftv_getfile
            'WriteEPS' : ftv_writeps
            'Write_PS_Landscape' :  ftv_write_ps_landscape
            'WriteTiff': ftv_writetiff
            'Quit':     ftv_shutdown

;**FUSE: emurphy added GRBW color table
; ColorMap menu options:            
            'Grayscale': ftv_getct, 0
            'Blue-White': ftv_getct, 1
            'GRBW': ftv_getct, 2
            'Red-Orange': ftv_getct, 3
            'BGRY': ftv_getct, 4
            'Rainbow': ftv_getct, 13
            'Stern Special': ftv_getct, 15
            'FTV Special': ftv_makect, event_name
; Scaling options:
            'Linear': begin
                state.scaling = 0
                ftv_displayall
                ftv_cleartext
            end
            'Log': begin
                state.scaling = 1
                ftv_displayall
                ftv_cleartext
            end
            'HistEq': begin
                state.scaling = 2
                ftv_displayall
                ftv_cleartext
            end
; Label options:
            'TextLabel': ftv_textlabel
            'Contour': ftv_oplotcontour
            'Draw': ftv_draw
            'EraseLast': ftverase, 1
            'EraseAll': ftverase
; Help options:            
            'FTV Help': ftv_help

            else: print, 'Unknown event in file menu!'
        endcase
        
    end   ; end of file menu options




; If the mouse enters the main draw base, set the input focus to
; the invisible text widget, for keyboard input.
; When the mouse leaves the main draw base, de-allocate the input
; focus by setting the text widget value.

    'draw_base': begin
        case event.enter of
            0: begin
                widget_control, state.keyboard_text_id, set_value = ''
            end
            
            1: begin
                widget_control, state.keyboard_text_id, /input_focus
            end
        endcase      
    end

    'draw_window': begin  ; mouse movement or button press
        
        if (event.type EQ 2) then begin   ; motion event
            case state.cstretch of
                0: begin           ;update tracking image and status bar
                    tmp_event = [event.x, event.y]            
                    state.mouse = round( (0.5 > $
                         ((tmp_event / state.zoom_factor) + state.offset) $
                         < (state.image_size - 0.5) ) - 0.5)
                    ftv_gettrack
                end
                1: begin           ; update color table
                    ftv_stretchct, event.x, event.y, /getmouse
                end

            endcase
                
        endif

        if (state.mode EQ 'blink' AND event.type EQ 0) then begin
            wset, state.draw_window_id
            if n_elements(blink_image) GT 1 then $
              tv, blink_image
        endif

        if (state.mode EQ 'blink' AND event.type EQ 1) then begin
            wset, state.draw_window_id
;           tv, display_image        modified AJB 7/26/99
            tv, unblink_image
        endif            
 
        if (state.mode EQ 'zoom' and event.type EQ 0) then begin
            case event.press of
                1: ftv_zoom, 'in', /recenter
                2: ftv_zoom, 'none', /recenter
                4: ftv_zoom, 'out', /recenter
                else: print,  'trouble in ftv_event, mouse zoom'
            endcase
            
        endif
                
        if (state.mode EQ 'color' AND event.type EQ 0) then begin
            state.cstretch = 1
            ftv_stretchct, event.x, event.y, /getmouse
        endif
        if (state.mode EQ 'color' AND event.type EQ 1) then $
          state.cstretch = 0

        widget_control, state.keyboard_text_id, /input_focus
                    
    end

    'invert': begin                  ; invert the color table
        state.invert_colormap = abs(state.invert_colormap - 1)
        tvlct, r, g, b, 8, /get

        r = abs( r - 255 )
        g = abs( g - 255 )
        b = abs( b - 255 )
        r_vector = abs( r_vector - 255 )
        g_vector = abs( g_vector - 255 )
        b_vector = abs( b_vector - 255 )

        tvlct, r, g, b, 8
        
        ftv_cleartext
    end

    'reset_color': begin   ; set color sliders to default positions
        state.brightness = state.slidermax / 2
        state.contrast = state.slidermax / 2
        ftv_stretchct, state.brightness, state.contrast
        ftv_cleartext
    end


    'min_text': begin     ; text entry in 'min = ' box
        ftv_get_minmax, uvalue, event.value
        ftv_displayall
    end

    'max_text': begin     ; text entry in 'max = ' box
        ftv_get_minmax, uvalue, event.value
        ftv_displayall
    end

    'autoscale_button': begin   ; autoscale the image
        ftv_autoscale
        ftv_displayall
        ftv_cleartext
    end

    'full_range': begin    ; display the full intensity range
        state.min_value = state.image_min
        state.max_value = state.image_max
        if state.min_value GE state.max_value then begin
            state.min_value = state.max_value - 1
            state.max_value = state.max_value + 1
        endif
        ftv_set_minmax
        ftv_displayall
        ftv_cleartext
    end

    'set_blink': begin     ; store current display image for blinking
        wset, state.draw_window_id
        blink_image = tvrd()
    end
    
    'keyboard_text': begin  ; keyboard input with mouse in display window
        eventchar = string(event.ch)
        case eventchar of
            '1': ftv_move_cursor, eventchar
            '2': ftv_move_cursor, eventchar
            '3': ftv_move_cursor, eventchar
            '4': ftv_move_cursor, eventchar
            '6': ftv_move_cursor, eventchar
            '7': ftv_move_cursor, eventchar
            '8': ftv_move_cursor, eventchar
            '9': ftv_move_cursor, eventchar
            'r': ftv_rowplot
            'c': ftv_colplot
            's': ftv_surfplot
            't': ftv_contourplot
            'p': ftv_mapphot
            else:  ;any other key press does nothing
        endcase
        widget_control, state.keyboard_text_id, /clear_events
    end

    'zoom_in':  ftv_zoom, 'in'         ; zoom buttons
    'zoom_out': ftv_zoom, 'out'
    'zoom_one': ftv_zoom, 'one'

    'center': begin   ; center image and preserve current zoom level
        ftv_drawbox
        state.centerpix = round(state.image_size / 2.)
        ftv_getoffset
        ftv_drawbox
        ftv_getdisplay
    end

    'pan_window': begin    ; move the box around in the pan window
        case event.type of
            2: begin                     ; motion event
                if (state.pan_track EQ 1) then begin
                    ftv_pantrack, event
                endif
            end
            
            0: begin                     ; button press
                state.pan_track = 1
                ftv_pantrack, event
            end
            1: begin                     ; button release
                state.pan_track = 0
                ftv_getdisplay
            end
            else:
        endcase
    end

    'done':  ftv_shutdown

    else:  print, 'No match for uvalue....'  ; bad news if this happens

endcase

end

;----------------------------------------------------------------------

pro ftv_drawbox

; routine to draw the box on the pan window, given the current center
; of the display image.
;
; By using device, set_graphics = 6, the same routine can be used both
; to draw the box, and to remove the box by drawing it over again at
; the same position.  So, to move the box around, call this routine at
; the old position to erase the old box, then get the new box position
; and update state.centerpix, then call this routine again to draw the
; new box.  

common ftv_state

wset, state.pan_window_id

view_min = round(state.centerpix - $
        (0.5 * state.draw_window_size / state.zoom_factor))
view_max = round(view_min + state.draw_window_size / state.zoom_factor)

; Create the vectors which contain the box coordinates

box_x = float((([view_min[0], $
                 view_max[0], $
                 view_max[0], $
                 view_min[0], $
                 view_min[0]]) * state.pan_scale) + state.pan_offset[0]) 

box_y = float((([view_min[1], $
                 view_min[1], $
                 view_max[1], $
                 view_max[1], $
                 view_min[1]]) * state.pan_scale) + state.pan_offset[1]) 

; Plot the box

device, set_graphics = 6
plots, box_x, box_y, /device, thick = 2
device, set_graphics = 3

;**FUSE: emurphy added the following section to draw a box around the spectrum
box_x = float((([0, $
                 state.image_size[0], $
                 state.image_size[0], $
                 0, $
                 0]) * state.pan_scale) + state.pan_offset[0]) 

box_y = float((([0, $
                 0, $
                 state.image_size[1], $
                 state.image_size[1], $
                 0]) * state.pan_scale) + state.pan_offset[1]) 

device, set_graphics = 6
plots, box_x, box_y, /device, thick = 1
device, set_graphics = 3
;**FUSE: end addition

end

;----------------------------------------------------------------------

pro ftv_shutdown

; routine to kill the ftv window(s) and clear variables to conserve
; memory when quitting ftv.  Since we can't delvar the ftv internal
; variables, just set them equal to zero so they don't take up a lot
; of space.  Also clear the state and the color map vectors.

common ftv_images
common ftv_state
common ftv_color
common ftv_pdata

if (xregistered ('ftv')) then begin
    widget_control, state.base_id, /destroy
endif

if (n_elements(phistory) GT 1) then begin
    ftverase, /norefresh
    pdata = 0
    ptext = 0
    pcon = 0
    phistory = 0
endif

main_image = 0
display_image = 0
scaled_image = 0
blink_image = 0
unblink_image = 0
pan_image = 0
r_vector = 0
g_vector = 0
b_vector = 0
state = 0

end

;----------------------------------------------------------------------
pro ftv_pantrack, event

; routine to track the view box in the pan window during cursor motion

common ftv_state

; erase the old box
ftv_drawbox

; get the new box coords and draw the new box

tmp_event = [event.x, event.y] 

newpos = state.pan_offset > tmp_event < $
  (state.pan_offset + (state.image_size * state.pan_scale))

state.centerpix = round( (newpos - state.pan_offset ) / state.pan_scale)

ftv_drawbox
ftv_getoffset

end

;----------------------------------------------------------------------

pro ftv_resize, event

; Routine to resize the draw window when a top-level resize event
; occurs.

common ftv_state

widget_control, event.top, tlb_get_size=tmp_event

drawpad = (widget_info(state.draw_base_id,/geometry)).xsize - $
  state.draw_window_size[0]

window = (state.base_min_size > tmp_event)

newbase = window - state.base_pad

; modified by AJB 8/19/99
; Note: don't know why the (-4) is needed below, but it seems
; to work... without it the base becomes the wrong size when the
; colorbar draw widget is resized.

widget_control, state.colorbar_base_id, $
  xsize = newbase[0]-4, ysize = state.colorbar_height + 6

widget_control, state.draw_base_id, $
  xsize = newbase[0], ysize = newbase[1]

newxsize = (widget_info(state.draw_base_id,/geometry)).xsize - drawpad
newysize = (widget_info(state.draw_base_id,/geometry)).ysize - drawpad

widget_control, state.draw_widget_id, $
  xsize = newxsize, ysize = newysize
widget_control, state.colorbar_widget_id, $
  xsize = newxsize, ysize = state.colorbar_height

state.draw_window_size = [newxsize, newysize]

ftv_colorbar

end

;----------------------------------------------------------------------

pro ftv_scaleimage

; Create a byte-scaled copy of the image, scaled according to
; the state.scaling parameter.  Add a padding of 5 pixels around the
; image boundary, so that the tracking window can always remain
; centered on an image pixel even if that pixel is at the edge of the
; image.    

common ftv_state
common ftv_images

; Since this can take some time for a big image, set the cursor 
; to an hourglass until control returns to the event loop.

widget_control, /hourglass

case state.scaling of
    0: tmp_image = $                 ; linear stretch
      bytscl(main_image, $                           
             min=state.min_value, $
             max=state.max_value, $
             top = !d.table_size-8) + 8
    
    1: tmp_image = $                 ; log stretch
      bytscl( alog10 (bytscl(main_image, $                       
                             min=state.min_value, $
                             max=state.max_value) + 1),  $
            top = !d.table_size - 8) + 8
    
    2: tmp_image = $                 ; histogram equalization
      bytscl(hist_equal(main_image, $
                        minv = state.min_value, $    
                        maxv = state.max_value), $
             top = !d.table_size-8) + 8
    
endcase

scaled_image = bytarr(state.image_size[0] + 10, $
                             state.image_size[1] + 10)

scaled_image[5, 5] = temporary(tmp_image)

end

;----------------------------------------------------------------------
function ftv_icolor, color

; Routine to reserve the bottom 8 colors of the color table
; for plot overlays and line plots.

   if (n_elements(color) EQ 0) then return, 1

   ncolor = N_elements(color)

   ; If COLOR is a string or array of strings, then convert color names
   ; to integer values
   if (size(color,/tname) EQ 'STRING') then begin ; Test if COLOR is a string

      ; Detemine the default color for the current device
      if (!d.name EQ 'X') then defcolor = 7 $ ; white for X-windows
       else defcolor = 0 ; black otherwise

      icolor = 0 * (color EQ 'black') $
             + 1 * (color EQ 'red') $
             + 2 * (color EQ 'green') $
             + 3 * (color EQ 'blue') $
             + 4 * (color EQ 'cyan') $
             + 5 * (color EQ 'magenta') $
             + 6 * (color EQ 'yellow') $
             + 7 * (color EQ 'white') $
             + defcolor * (color EQ 'default')

   endif else begin
      icolor = long(color)
   endelse

   return, icolor
end 

;----------------------------------------------------------------------
pro ftv_plot1plot, iplot
common ftv_pdata

widget_control, /hourglass

oplot, *(pdata.x[iplot]), *(pdata.y[iplot]), $
 color=pdata.color[iplot], psym=pdata.psym[iplot], $
 symsize=pdata.symsize[iplot], thick=pdata.thick[iplot]

return
end

;----------------------------------------------------------------------
pro ftv_plot1text, iplot
common ftv_pdata

widget_control, /hourglass

xyouts, ptext.x[iplot], ptext.y[iplot], *(ptext.string[iplot]), $
 alignment=ptext.alignment[iplot], charsize=ptext.charsize[iplot], $
 charthick=ptext.charthick[iplot], color=ptext.color[iplot], $
 font=ptext.font[iplot], orientation=ptext.orientation[iplot]

return
end

;----------------------------------------------------------------------
pro ftv_plot1contour, iplot
common ftv_pdata

widget_control, /hourglass

xrange = !x.crange
yrange = !y.crange

; The following allows for 4 conditions, depending upon whether X and Y
; are set on whether LEVELS is set.
; In addition, I have commented out C_ORIENTATION and C_SPACING, since
; these seem to force FILL - is this an IDL bug?
dims = size(*(pcon.z[iplot]),/dim)
levels = *(pcon.levels[iplot])
if (size(*(pcon.x[iplot]),/N_elements) EQ dims[0] $
 AND size(*(pcon.y[iplot]),/N_elements) EQ dims[1] ) then begin
   if (N_elements(levels) GT 1) then begin
contour, *(pcon.z[iplot]), *(pcon.x[iplot]), *(pcon.y[iplot]), $
 c_annotation=*(pcon.c_annotation[iplot]), c_charsize=pcon.c_charsize[iplot], $
 c_charthick=pcon.c_charthick[iplot], c_colors=*(pcon.c_colors[iplot]), $
 c_labels=*(pcon.c_labels[iplot]), c_linestyle=*(pcon.c_linestyle[iplot]), $
; c_orientation=pcon.c_orientation[iplot], c_spacing=pcon.c_spacing[iplot], 
 c_thick=*(pcon.c_thick[iplot]), cell_fill=pcon.cell_fill[iplot], $
 closed=pcon.closed[iplot], downhill=pcon.downhill[iplot], $
 fill=pcon.fill[iplot], irregular=pcon.irregular[iplot], $
 levels=*(pcon.levels[iplot]), $
 max_value=pcon.max_value[iplot], min_value=pcon.min_value[iplot], $
 nlevels=pcon.nlevels[iplot], $
 position=[0,0,1,1], xrange=xrange, yrange=yrange, xstyle=5, ystyle=5, /noerase
   endif else begin
contour, *(pcon.z[iplot]), *(pcon.x[iplot]), *(pcon.y[iplot]), $
 c_annotation=*(pcon.c_annotation[iplot]), c_charsize=pcon.c_charsize[iplot], $
 c_charthick=pcon.c_charthick[iplot], c_colors=*(pcon.c_colors[iplot]), $
 c_labels=*(pcon.c_labels[iplot]), c_linestyle=*(pcon.c_linestyle[iplot]), $
; c_orientation=pcon.c_orientation[iplot], c_spacing=pcon.c_spacing[iplot], $
 c_thick=*(pcon.c_thick[iplot]), cell_fill=pcon.cell_fill[iplot], $
 closed=pcon.closed[iplot], downhill=pcon.downhill[iplot], $
 fill=pcon.fill[iplot], irregular=pcon.irregular[iplot], $
; levels=*(pcon.levels[iplot]), $
 max_value=pcon.max_value[iplot], min_value=pcon.min_value[iplot], $
 nlevels=pcon.nlevels[iplot], $
 position=[0,0,1,1], xrange=xrange, yrange=yrange, xstyle=5, ystyle=5, /noerase
   endelse
endif else begin
   if (N_elements(levels) GT 1) then begin
contour, *(pcon.z[iplot]), $
 c_annotation=*(pcon.c_annotation[iplot]), c_charsize=pcon.c_charsize[iplot], $
 c_charthick=pcon.c_charthick[iplot], c_colors=*(pcon.c_colors[iplot]), $
 c_labels=*(pcon.c_labels[iplot]), c_linestyle=*(pcon.c_linestyle[iplot]), $
; c_orientation=pcon.c_orientation[iplot], c_spacing=pcon.c_spacing[iplot], $
 c_thick=*(pcon.c_thick[iplot]), cell_fill=pcon.cell_fill[iplot], $
 closed=pcon.closed[iplot], downhill=pcon.downhill[iplot], $
 fill=pcon.fill[iplot], irregular=pcon.irregular[iplot], $
 levels=*(pcon.levels[iplot]), $
 max_value=pcon.max_value[iplot], min_value=pcon.min_value[iplot], $
 nlevels=pcon.nlevels[iplot], $
 position=[0,0,1,1], xrange=xrange, yrange=yrange, xstyle=5, ystyle=5, /noerase
         endif else begin
contour, *(pcon.z[iplot]), $
 c_annotation=*(pcon.c_annotation[iplot]), c_charsize=pcon.c_charsize[iplot], $
 c_charthick=pcon.c_charthick[iplot], c_colors=*(pcon.c_colors[iplot]), $
 c_labels=*(pcon.c_labels[iplot]), c_linestyle=*(pcon.c_linestyle[iplot]), $
; c_orientation=pcon.c_orientation[iplot], c_spacing=pcon.c_spacing[iplot], $
 c_thick=*(pcon.c_thick[iplot]), cell_fill=pcon.cell_fill[iplot], $
 closed=pcon.closed[iplot], downhill=pcon.downhill[iplot], $
 fill=pcon.fill[iplot], irregular=pcon.irregular[iplot], $
; levels=*(pcon.levels[iplot]), $
 max_value=pcon.max_value[iplot], min_value=pcon.min_value[iplot], $
 nlevels=pcon.nlevels[iplot], $
 position=[0,0,1,1], xrange=xrange, yrange=yrange, xstyle=5, ystyle=5, /noerase
   endelse
endelse

return
end

;----------------------------------------------------------------------
pro ftv_plotwindow
common ftv_state

; Set plot window
xrange=[state.offset[0], $
 state.offset[0] + state.draw_window_size[0] / state.zoom_factor] - 0.5
yrange=[state.offset[1], $
 state.offset[1] + state.draw_window_size[1] / state.zoom_factor] - 0.5

plot, [0], [0], /nodata, position=[0,0,1,1], $
 xrange=xrange, yrange=yrange, xstyle=5, ystyle=5, /noerase

return
end

;----------------------------------------------------------------------
pro ftv_plotall
common ftv_state
common ftv_pdata

; Routine to overplot line plots from FTVPLOT and text from FTVXYOUTS

if (pdata.nplot GT 0 OR ptext.nplot GT 0 OR pcon.nplot GT 0) then begin
   ftv_plotwindow

   for iplot=0, pcon.nplot-1 do $
    ftv_plot1contour, iplot

   for iplot=0, pdata.nplot-1 do $
    ftv_plot1plot, iplot

   for iplot=0, ptext.nplot-1 do $
    ftv_plot1text, iplot

endif

end

;----------------------------------------------------------------------

pro ftvplot, x, y, color=color, psym=psym, symsize=symsize, thick=thick
common ftv_pdata
common ftv_state

; Routine to overplot line plots

if (N_params() LT 1) then begin
   print, 'Too few parameters for FTVPLOT'
   return
endif

if (pdata.nplot LT pdata.nmax) then begin
   iplot = pdata.nplot

   if (N_params() EQ 1) then begin
      pdata.x[iplot] = ptr_new(findgen(N_elements(x)))
      pdata.y[iplot] = ptr_new(x)
   endif else begin
      pdata.x[iplot] = ptr_new(x)
      pdata.y[iplot] = ptr_new(y)
   endelse

   pdata.color[iplot] = ftv_icolor(color)
   if (keyword_set(psym)) then pdata.psym[iplot] = psym $
    else pdata.psym[iplot] = 0
   if (keyword_set(symsize)) then pdata.symsize[iplot] = symsize $
    else pdata.symsize[iplot] = 1.0
   if (keyword_set(thick)) then pdata.thick[iplot] = thick $
    else pdata.thick[iplot] = 1.0

   wset, state.draw_window_id
   ftv_plotwindow
   ftv_plot1plot, pdata.nplot
   pdata.nplot = pdata.nplot + 1
endif else begin
   print, 'Too many calls to FTVPLOT'
endelse

phistory[pdata.nplot + ptext.nplot + pcon.nplot - 1] = 1 ; points

end

;----------------------------------------------------------------------

pro ftvxyouts, x, y, string, alignment=alignment, charsize=charsize, $
 charthick=charthick, color=color, font=font, orientation=orientation
common ftv_pdata
common ftv_state

; Routine to overplot text

if (N_params() LT 3) then begin
   print, 'Too few parameters for FTVXYOUTS'
   return
endif

if (ptext.nplot LT ptext.nmax) then begin
   iplot = ptext.nplot

   ptext.x[iplot] = x
   ptext.y[iplot] = y
   ptext.string[iplot] = ptr_new(string)
   if (keyword_set(alignment)) then ptext.alignment[iplot] = alignment $
    else ptext.alignment[iplot] = 0.0
   if (keyword_set(charsize)) then ptext.charsize[iplot] = charsize $
    else ptext.charsize[iplot] = 1.0
   if (keyword_set(charthick)) then ptext.charthick[iplot] = charthick $
    else ptext.charthick[iplot] = 1.0
   ptext.color[iplot] = ftv_icolor(color)
   if (keyword_set(font)) then ptext.font[iplot] = font $
    else ptext.font[iplot] = 1
   if (keyword_set(orientation)) then ptext.orientation[iplot] = orientation $
    else ptext.orientation[iplot] = 0.0

   wset, state.draw_window_id
   ftv_plotwindow
   ftv_plot1text, ptext.nplot
   ptext.nplot = ptext.nplot + 1
endif else begin
   print, 'Too many calls to FTVPLOT'
endelse

phistory[pdata.nplot + ptext.nplot + pcon.nplot - 1] = 2 ; text

end

;----------------------------------------------------------------------

pro ftvcontour, z, x, y, c_annotation=c_annotation, c_charsize=c_charsize, $
 c_charthick=c_charthick, c_colors=c_colors, c_labels=c_labels, $
 c_linestyle=c_linestyle, c_orientation=c_orientation, c_spacing=c_spacing, $
 c_thick=c_thick, cell_fill=cell_fill, closed=closed, downhill=downhill, $
 fill=fill, irregular=irregular, levels=levels, max_value=max_value, $
 min_value=min_value, nlevels=nlevels
common ftv_pdata
common ftv_state

; Routine to overplot contours

if (N_params() LT 1) then begin
   print, 'Too few parameters for FTVCONTOUR'
   return
endif

if (pcon.nplot LT pcon.nmax) then begin
   iplot = pcon.nplot

   pcon.z[iplot] = ptr_new(z)
   if (keyword_set(x)) then pcon.x[iplot] = ptr_new(x) $
    else pcon.x[iplot] = ptr_new(0)
   if (keyword_set(y)) then pcon.y[iplot] = ptr_new(y) $
    else pcon.y[iplot] = ptr_new(0)
   if (keyword_set(c_annotation)) then $
    pcon.c_annotation[iplot] = ptr_new(c_annotation) $
    else pcon.c_annotation[iplot] = ptr_new(0)
   if (keyword_set(c_charsize)) then pcon.c_charsize[iplot] = c_charsize $
    else pcon.c_charsize[iplot] = 0
   if (keyword_set(c_charthick)) then pcon.c_charthick[iplot] = c_charthick $
    else pcon.c_charthick[iplot] = 0
   pcon.c_colors[iplot] = ptr_new(ftv_icolor(c_colors))
   if (keyword_set(c_labels)) then pcon.c_labels[iplot] = ptr_new(c_labels) $
    else pcon.c_labels[iplot] = ptr_new(0)
   if (keyword_set(c_linestyle)) then $
    pcon.c_linestyle[iplot] = ptr_new(c_linestyle) $
    else pcon.c_linestyle[iplot] = ptr_new(0)
   if (keyword_set(c_orientation)) then $
    pcon.c_orientation[iplot] = c_orientation $
    else pcon.c_orientation[iplot] = 0
   if (keyword_set(c_spacing)) then pcon.c_spacing[iplot] = c_spacing $
    else pcon.c_spacing[iplot] = 0
   if (keyword_set(c_thick)) then pcon.c_thick[iplot] = ptr_new(c_thick) $
    else pcon.c_thick[iplot] = ptr_new(0)
   if (keyword_set(cell_fill)) then pcon.cell_fill[iplot] = cell_fill $
    else pcon.cell_fill[iplot] = 0
   if (keyword_set(closed)) then pcon.closed[iplot] = closed $
    else pcon.closed[iplot] = 0
   if (keyword_set(downhill)) then pcon.downhill[iplot] = downhill $
    else pcon.downhill[iplot] = 0
   if (keyword_set(fill)) then pcon.fill[iplot] = fill $
    else pcon.fill[iplot] = 0
   if (keyword_set(irregular)) then pcon.irregular[iplot] = irregular $
    else pcon.irregular[iplot] = 0
   if (keyword_set(levels)) then pcon.levels[iplot] = ptr_new(levels) $
    else pcon.levels[iplot] = ptr_new(0)
   if (keyword_set(max_value)) then pcon.max_value[iplot] = max_value $
    else pcon.max_value[iplot] = !values.d_infinity
   if (keyword_set(min_value)) then pcon.min_value[iplot] = min_value $
    else pcon.min_value[iplot] = - !values.d_infinity
   if (keyword_set(nlevels)) then pcon.nlevels[iplot] = nlevels $
    else pcon.nlevels[iplot] = 0

   wset, state.draw_window_id
   ftv_plotwindow
   ftv_plot1contour, pcon.nplot
   pcon.nplot = pcon.nplot + 1
endif else begin
   print, 'Too many calls to FTVCONTOUR'
endelse

phistory[pdata.nplot + ptext.nplot + pcon.nplot - 1] = 3 ; contour

end

;----------------------------------------------------------------------

pro ftverase, nerase, norefresh = norefresh
common ftv_pdata

; Routine to erase line plots from FTVPLOT, text from FTVXYOUTS, and
; contours from FTVCONTOUR.

nplotall = pdata.nplot + ptext.nplot + pcon.nplot
if (N_params() LT 1) then nerase = nplotall $
else if (nerase GT nplotall) then nerase = nplotall

for ihistory=nplotall-nerase, nplotall-1 do begin
   if (phistory[ihistory] EQ 1) then begin
      ; Erase a point plot
      pdata.nplot = pdata.nplot - 1
      iplot = pdata.nplot
      ptr_free, pdata.x[iplot], pdata.y[iplot]
   endif else if (phistory[ihistory] EQ 2) then begin
      ; Erase a text plot
      ptext.nplot = ptext.nplot - 1
      iplot = ptext.nplot
      ptr_free, ptext.string[iplot]
   endif else if (phistory[ihistory] EQ 3) then begin
      ; Erase a contour plot
      pcon.nplot = pcon.nplot - 1
      iplot = pcon.nplot
      ptr_free, pcon.z[iplot], pcon.x[iplot], pcon.y[iplot], $
        pcon.c_annotation[iplot], pcon.c_colors[iplot], $
        pcon.c_labels[iplot], pcon.c_linestyle[iplot], $
        pcon.c_thick[iplot], pcon.levels[iplot]
   endif
endfor

if (NOT keyword_set(norefresh) ) then ftv_refresh

end

;---------------------------------------------------------------------

pro ftv_getdisplay

; make the display image from the scaled image by applying the zoom
; factor and matching to the size of the draw window, and display the
; image.

common ftv_state
common ftv_images

widget_control, /hourglass    ; added by AJB 7/26/99

display_image = bytarr(state.draw_window_size[0], state.draw_window_size[1])

view_min = round(state.centerpix - $
                  (0.5 * state.draw_window_size / state.zoom_factor))
view_max = round(view_min + state.draw_window_size / state.zoom_factor)

view_min = (0 > view_min < (state.image_size - 1)) + 5
view_max = (0 > view_max < (state.image_size - 1)) + 5

newsize = round( (view_max - view_min + 1) * state.zoom_factor) > 1
startpos = abs( round(state.offset * state.zoom_factor) < 0)

tmp_image = congrid(scaled_image[view_min[0]:view_max[0], $
                                            view_min[1]:view_max[1]], $
                                            newsize[0], newsize[1])

xmax = newsize[0] < (state.draw_window_size[0] - startpos[0])
ymax = newsize[1] < (state.draw_window_size[1] - startpos[1])

display_image[startpos[0], startpos[1]] = $
  temporary(tmp_image[0:xmax-1, 0:ymax-1])


; Display the image

wset, state.draw_window_id
erase
tv, display_image

; Overplot x,y plots from ftvplot
ftv_plotall

wset, state.draw_window_id
unblink_image = tvrd()     ; moved here by AJB 8/11/99 to fix bug

end

;--------------------------------------------------------------------


pro ftv_makepan

; Make the 'pan' image that shows a miniature version of the full image.

common ftv_state
common ftv_images

sizeratio = state.image_size[1] / state.image_size[0]

;**FUSE: emurphy changed the following lines from _sixe to _ysize and _xsize
;if (sizeratio GE 1) then begin
;    state.pan_scale = float(state.pan_window_size) / float(state.image_size[1])
;endif else begin
;    state.pan_scale = float(state.pan_window_size) / float(state.image_size[0])
;endelse

panratio = state.pan_window_ysize / float(state.pan_window_xsize)

if (sizeratio GE panratio) then begin
    state.pan_scale = float(state.pan_window_ysize) / float(state.image_size[1])
endif else begin
    state.pan_scale = float(state.pan_window_xsize) / float(state.image_size[0])
endelse
;**FUSE end changes

tmp_image = $
  scaled_image[5:state.image_size[0]+4, 5:state.image_size[1]+4]

pan_image = congrid(tmp_image, round(state.pan_scale * state.image_size[0]), $
                    round(state.pan_scale * state.image_size[1]) )

;**FUSE: emurphy changed the following lines
;state.pan_offset[0] = round((state.pan_window_size - (size(pan_image))[1]) / 2)
;state.pan_offset[1] = round((state.pan_window_size - (size(pan_image))[2]) / 2)

if (sizeratio GE panratio) then begin
   state.pan_offset[0] = round((state.pan_window_xsize - $
        (size(pan_image))[1]) / 2)
   state.pan_offset[1] = round((state.pan_window_ysize - $
        (size(pan_image))[2]) / 2)
endif else begin
   state.pan_offset[0] = round((state.pan_window_xsize - $
        (size(pan_image))[1]) / 2)
   state.pan_offset[1] = round((state.pan_window_ysize - $
        (size(pan_image))[2]) / 2)
endelse
;**FUSE: end changes

end


;----------------------------------------------------------------------

pro ftv_move_cursor, direction

; Use keypad arrow keys to step cursor one pixel at a time.
; Get the new track image, and update the cursor position.

common ftv_state

i = 1L

case direction of
    '2': state.mouse[1] = max([state.mouse[1] - i, 0])
    '4': state.mouse[0] = max([state.mouse[0] - i, 0])
    '8': state.mouse[1] = min([state.mouse[1] + i, state.image_size[1] - i])
    '6': state.mouse[0] = min([state.mouse[0] + i, state.image_size[0] - i])
    '7': begin
        state.mouse[1] = min([state.mouse[1] + i, state.image_size[1] - i])
        state.mouse[0] = max([state.mouse[0] - i, 0])
    end
    '9': begin
        state.mouse[1] = min([state.mouse[1] + i, state.image_size[1] - i])
        state.mouse[0] = min([state.mouse[0] + i, state.image_size[0] - i])
    end
    '3': begin
        state.mouse[1] = max([state.mouse[1] - i, 0])
        state.mouse[0] = min([state.mouse[0] + i, state.image_size[0] - i])
    end
    '1': begin
        state.mouse[1] = max([state.mouse[1] - i, 0])
        state.mouse[0] = max([state.mouse[0] - i, 0])
    end

endcase

newpos = (state.mouse - state.offset + 0.5) * state.zoom_factor

wset,  state.draw_window_id
tvcrs, newpos[0], newpos[1], /device

ftv_gettrack

; Prevent the cursor move from causing a mouse event in the draw window

widget_control, state.draw_widget_id, /clear_events

end


;----------------------------------------------------------------------

pro ftv_set_minmax

; Updates the min and max text boxes with new values.

common ftv_state

widget_control, state.min_text_id, set_value = string(state.min_value)
widget_control, state.max_text_id, set_value = string(state.max_value)

end

;----------------------------------------------------------------------

pro ftv_get_minmax, uvalue, newvalue

; Change the min and max state variables when user inputs new numbers
; in the text boxes. 

common ftv_state

case uvalue of
    
    'min_text': begin
        if (newvalue LT state.max_value) then begin
            state.min_value = newvalue
        endif
    end

    'max_text': begin
        if (newvalue GT state.min_value) then begin
            state.max_value = newvalue
        endif
    end
        
endcase

ftv_set_minmax

end

;--------------------------------------------------------------------

pro ftv_refresh

; Make the display image from the scaled_image, and redisplay the pan
; image and tracking image. 

common ftv_state
common ftv_images

ftv_getoffset
ftv_getdisplay

; redisplay the pan image and plot the boundary box

wset, state.pan_window_id
erase
tv, pan_image, state.pan_offset[0], state.pan_offset[1]
ftv_drawbox

; redisplay the tracking image

wset, state.track_window_id
ftv_gettrack

; Added by AJB 7/26/99 to prevent unwanted mouse clicks
widget_control, state.draw_base_id, /clear_events   

end

;--------------------------------------------------------------------

pro ftv_stretchct, brightness, contrast,  getmouse = getmouse

; modified by AJB 8/18/99
; routine to change color stretch for given values of 
; brightness and contrast.
; Brightness and contrast range from 1 up to state.slidermax

common ftv_state
common ftv_color

if (keyword_set(getmouse)) then begin
    contrast = 0 > (contrast / float(state.draw_window_size[1]) * $
                    state.slidermax) < (state.slidermax-1)
    contrast = long(abs(state.slidermax - contrast))

    brightness = 0 > $
      (brightness / float(state.draw_window_size[0]) * state.slidermax) < $
      (state.slidermax - 1)
    brightness = long(abs(brightness-state.slidermax))
endif

d = byte(!d.table_size - 8)

maxdp = 600
mindp = 4

if (contrast LT (state.slidermax / 2)) then begin
    dp = ((d - maxdp) / float((state.slidermax / 2) - 1)) * contrast + $
      ((maxdp * state.slidermax / 2) - d) / float(state.slidermax / 2)
endif else begin
    dp = ((mindp - d) / float(state.slidermax / 2)) * contrast + $
      ((d * state.slidermax) - (mindp * state.slidermax / 2)) / $
      float(state.slidermax / 2)
endelse

dp =  fix(dp)

r = replicate(r_vector(d-1), 2*d + dp)
g = replicate(g_vector(d-1), 2*d + dp)
b = replicate(b_vector(d-1), 2*d + dp)

r[0:d-1] = r_vector(0)
g[0:d-1] = g_vector(0)
b[0:d-1] = b_vector(0)

a = findgen(d)

r[d] = congrid(r_vector, dp)
g[d] = congrid(g_vector, dp)
b[d] = congrid(b_vector, dp)

bshift = round(brightness * (d+dp) / float(state.slidermax))

rr = r[a + bshift] 
gg = g[a + bshift]
bb = b[a + bshift]

tvlct, rr, gg, bb, 8

state.brightness = brightness
state.contrast = contrast

end


;--------------------------------------------------------------------

pro ftv_getct, tablenum

; Read in a pre-defined color table, and invert if necessary.

common ftv_color
common ftv_state

; Load a simple color table with the basic 8 colors in the lowest 8 entries
; of the color table
rtiny   = [0, 1, 0, 0, 0, 1, 1, 1]
gtiny = [0, 0, 1, 0, 1, 0, 1, 1]
btiny  = [0, 0, 0, 1, 1, 1, 0, 1]
tvlct, 255*rtiny, 255*gtiny, 255*btiny

loadct, tablenum, /silent,  bottom=8
tvlct, r, g, b, 8, /get

if (state.invert_colormap EQ 1) then begin
    r = abs (r - 255)
    g = abs (g - 255)
    b = abs (b - 255)
endif

r_vector = r
g_vector = g
b_vector = b

ftv_stretchct, state.brightness, state.contrast

end

;--------------------------------------------------------------------

pro ftv_autoscale

; Routine to auto-scale the image.

common ftv_state 
common ftv_images

widget_control, /hourglass

med = median(main_image)
sig = stdev(main_image)

state.max_value = (med + (10 * sig)) < max(main_image)

state.min_value = (med - (2 * sig))  > min(main_image)
if (state.min_value LT 0 AND med GT 0) then begin
  state.min_value = 0.0
endif

if (state.min_value GE state.max_value) then begin
    state.min_value = state.min_value - 1
    state.max_value = state.max_value + 1
endif

ftv_set_minmax

end  

;--------------------------------------------------------------------

pro ftv_lineplot_init

; This routine creates the window for line plots

common ftv_state

state.lineplot_base_id = $
  widget_base(/floating, $
              group_leader = state.base_id, $
              /column, $
              /base_align_right, $
              title = 'ftv plot', $
              /tlb_size_events, $
              uvalue = 'lineplot_base')

state.lineplot_widget_id = $
  widget_draw(state.lineplot_base_id, $
              frame = 0, $
              scr_xsize = state.lineplot_size[0], $
              scr_ysize = state.lineplot_size[1], $
              uvalue = 'lineplot_window')

lbutton_base = $
  widget_base(state.lineplot_base_id, $
              /base_align_bottom, $
              /row)

lineplot_done = $
  widget_button(lbutton_base, $
                value = 'Done', $
                uvalue = 'lineplot_done')

widget_control, state.lineplot_base_id, /realize
widget_control, state.lineplot_widget_id, get_value = tmp_value
state.lineplot_window_id = tmp_value

basegeom = widget_info(state.lineplot_base_id, /geometry)
drawgeom = widget_info(state.lineplot_widget_id, /geometry)

state.lineplot_pad[0] = basegeom.xsize - drawgeom.xsize
state.lineplot_pad[1] = basegeom.ysize - drawgeom.ysize
    
xmanager, 'ftv_lineplot', state.lineplot_base_id, /no_block

end

;--------------------------------------------------------------------

pro ftv_rowplot

common ftv_state
common ftv_images

if (not (xregistered('ftv_lineplot'))) then begin
    ftv_lineplot_init
endif

wset, state.lineplot_window_id
erase

plot, main_image[*, state.mouse[1]], $
  xst = 3, yst = 3, psym = 10, $
  title = strcompress('Plot of row ' + $
                      string(state.mouse[1])), $
  xtitle = 'Column', $
  ytitle = 'Pixel Value', $
  color = 7 

widget_control, state.lineplot_base_id, /clear_events

end

;--------------------------------------------------------------------

pro ftv_colplot

common ftv_state
common ftv_images

if (not (xregistered('ftv_lineplot'))) then begin
    ftv_lineplot_init
endif

wset, state.lineplot_window_id
erase

plot, main_image[state.mouse[0], *], $
  xst = 3, yst = 3, psym = 10, $
  title = strcompress('Plot of column ' + $
                      string(state.mouse[0])), $
  xtitle = 'Row', $
  ytitle = 'Pixel Value', $
  color = 7

widget_control, state.lineplot_base_id, /clear_events
        
end

;--------------------------------------------------------------------

pro ftv_surfplot

common ftv_state
common ftv_images

if (not (xregistered('ftv_lineplot'))) then begin
    ftv_lineplot_init
endif

wset, state.lineplot_window_id
erase

plotsize = $
  fix(min([50, state.image_size[0]/2., state.image_size[1]/2.]))
center = plotsize > state.mouse < (state.image_size - plotsize) 

tmp_string = $
  strcompress('Surface plot of ' + $
              strcompress('['+string(center[0]-plotsize)+ $
                          ':'+string(center[0]+plotsize-1)+ $
                          ','+string(center[1]-plotsize)+ $
                          ':'+string(center[1]+plotsize-1)+ $
                          ']', /remove_all))

surface, $
  main_image[center[0]-plotsize:center[0]+plotsize-1, $
             center[1]-plotsize:center[1]+plotsize-1], $
  title = temporary(tmp_string), $
  xtitle = 'X', ytitle = 'Y', ztitle = 'Pixel Value', $
  color = 7

widget_control, state.lineplot_base_id, /clear_events

end

;--------------------------------------------------------------------

pro ftv_contourplot

common ftv_state
common ftv_images

if (not (xregistered('ftv_lineplot'))) then begin
    ftv_lineplot_init
endif

wset, state.lineplot_window_id
erase

plotsize = $
  fix(min([50, state.image_size[0]/2., state.image_size[1]/2.]))
center = plotsize > state.mouse < (state.image_size - plotsize) 

contour_image =  main_image[center[0]-plotsize:center[0]+plotsize-1, $
                            center[1]-plotsize:center[1]+plotsize-1]
if (state.scaling EQ 1) then begin
    contour_image = alog10(contour_image)
    logflag = 'Log'
endif else begin
    logflag = ''
endelse

tmp_string =  $
  strcompress(logflag + $
              ' Contour plot of ' + $
              strcompress('['+string(round(center[0]-plotsize))+ $
                          ':'+string(round(center[0]+plotsize-1))+ $
                          ','+string(round(center[1]-plotsize))+ $
                          ':'+string(round(center[1]+plotsize-1))+ $
                          ']', /remove_all))

contour, temporary(contour_image), $
  nlevels = 10, $
  /follow, $
  title = temporary(tmp_string), $
  xtitle = 'X', ytitle = 'Y', color = 7

widget_control, state.lineplot_base_id, /clear_events
        
end

;----------------------------------------------------------------------

pro ftv_lineplot_event, event

common ftv_state

widget_control, event.id, get_uvalue = uvalue

case uvalue of
    'lineplot_done': widget_control, event.top, /destroy
    'lineplot_base': begin                       ; Resize event
        state.lineplot_size = [event.x, event.y]- state.lineplot_pad
        widget_control, state.lineplot_widget_id, $
          xsize = (state.lineplot_size[0] > 100), $
          ysize = (state.lineplot_size[1] > 100)
        wset, state.lineplot_window_id
    end    
else:
endcase

end

;----------------------------------------------------------------------

pro ftv_help
common ftv_state


;**FUSE: emm set h = strarr(83)
h = strarr(83)
i = 0
h[i] =  'FTV HELP'
i = i + 1
h[i] =  ''
i = i + 1
h[i] =  'MENU BAR:'
i = i + 1
h[i] =  'File->ReadFits:     read in a new fits image from disk'
i = i + 1
h[i] =  'File->WriteEPS:     write an encapsulated PS file of the current display'
i = i + 1
;**FUSE: emm added the following line and an extra i=i+1
h[i] =  'File->Write_PS_Landscape:     write a landscape PS file of the current display'
i = i + 1
h[i] =  'File->WriteTiff:    write a tiff image of the current display'
i = i + 1
h[i] =  'File->Quit:         quits ftv'
i = i + 1
h[i] =  'ColorMap Menu:      selects color table'
i = i + 1
h[i] =  'Scaling Menu:       selects linear, log, or histogram-equalized scaling'
i = i + 1
h[i] =  'Labels->TextLabel:  Brings up a dialog box for text input'
i = i + 1
h[i] =  'Labels->Contour:    Brings up a dialog box for overplotting contours'
i = i + 1
h[i] =  'Labels->EraseLast:  Erases the most recent plot label'
i = i + 1
h[i] =  'Labels->EraseAll:   Erases all plot labels'
i = i + 1
h[i] =  ''
i = i + 1
h[i] =  'CONTROL PANEL ITEMS:'
i = i + 1
h[i] = 'Min:             shows minimum data value displayed; click to modify'
i = i + 1
h[i] = 'Max:             shows maximum data value displayed; click to modify'
i = i + 1
h[i] = 'Pan Window:      use mouse to drag the image-view box around'
i = i + 1
h[i] = ''
i = i + 1
h[i] = 'MOUSE MODE SELECTOR:'
i = i + 1
h[i] =  'Color:          sets color-stretch mode:'
i = i + 1
h[i] = '                    With mouse button down, drag mouse to change the color stretch.  '
i = i + 1
h[i] = '                    Move vertically to change contrast, and'
i = i + 1
h[i] = '                    horizontally to change brightness.'
i = i + 1
h[i] = 'Zoom:            sets zoom mode:'
i = i + 1
h[i] = '                    button1 = zoom in & center'
i = i + 1 
h[i] = '                    button2 = center on current position'
i = i + 1
h[i] = '                    button3 = zoom out & center'
i = i + 1
h[i] = 'Blink:           sets blink mode:'
i = i + 1
h[i] = '                    press mouse button in main window to show blink image'
i = i + 2
h[i] = 'BUTTONS:'
i = i + 1
h[i] = 'Invert:          inverts the current color table'
i = i + 1
h[i] = 'ResetColor:      sets brightness and contrast back to default values'
i = i + 1
h[i] = 'AutoScale:       sets min and max to show data values around histogram peak'
i = i + 1
h[i] = 'FullRange:       sets min and max to show the full data range of the image'
i = i + 1
h[i] = 'ZoomIn:          zooms in by x2'
i = i + 1
h[i] = 'ZoomOut:         zooms out by x2'
i = i + 1
h[i] = 'Zoom1:           sets zoom level to original scale'
i = i + 1
h[i] = 'Center:          centers image on display window'
i = i + 1
h[i] = 'SetBlink:        saves current display as the blink image'
i = i + 1
h[i] = 'Done:            quits ftv'
i = i + 1
h[i] = ''
i = i + 1
h[i] = 'Keyboard commands in display window:'
i = i + 1
h[i] = '    Numeric keypad (with NUM LOCK on) moves cursor'
i = i + 1
h[i] = '    r: row plot'
i = i + 1
h[i] = '    c: column plot'
i = i + 1
h[i] = '    s: surface plot'
i = i + 1
h[i] = '    t: contour plot'
i = i + 1
h[i] = '    p: aperture photometry at current position'
i = i + 2
h[i] = 'IDL COMMAND LINE HELP:'
i = i + 1
h[i] =  'To pass an array to ftv:'
i = i + 1
h[i] =  '   ftv, array_name [, options]'
i = i + 1
h[i] = 'To pass a fits filename to ftv:'
i = i + 1
h[i] = '    ftv, fitsfile_name [, options] (enclose filename in single quotes) '
i = i + 1
h[i] = 'To overplot a contour plot on the draw window:'
i = i + 1
h[i] = '    ftvcontour, array_name [, options...]'
i = i + 1
h[i] = 'To overplot text on the draw window: '
i = i + 1
h[i] = '    ftvxyouts, x, y, text_string [, options]  (enclose string in single quotes)'
i = i + 1
h[i] = 'To overplot points or lines on the current plot:'
i = i + 1
h[i] = '    ftvplot, xvector, yvector [, options]'
i = i + 2
h[i] = 'The options for ftvcontour, ftvxyouts, and ftvplot are essentially'
i = i + 1
h[i] =  'the same as those for the idl contour, xyouts, and plot commands,'
i = i + 1
h[i] = 'except that data coordinates are always used.' 
i = i + 1
h[i] = 'The default color is red for overplots done from the idl command line.'
i = i + 2
h[i] = 'Other commands:'
i = i + 1
h[i] = 'ftverase [, N]:       erases all (or last N) plots and text'
i = i + 1
h[i] = 'ftv_shutdown:   quits ftv'
i = i + 1
h[i] = 'NOTE: If ftv should crash, type ftv_shutdown at the idl prompt.'
i = i + 5
h[i] = strcompress('FTV.PRO version '+state.version+' by Aaron J. Barth')
i = i + 1
h[i] = 'For full instructions, or to download the most recent version, go to:'
i = i + 1
h[i] = 'http://cfa-www.harvard.edu/~abarth/ftv/ftv.html'


if (not (xregistered('ftv_help'))) then begin
    help_base =  widget_base(/floating, $
                             group_leader = state.base_id, $
                             /column, $
                             /base_align_right, $
                             title = 'ftv help', $
                             uvalue = 'help_base')

    help_text = widget_text(help_base, $
                            /scroll, $
                            value = h, $
                            xsize = 85, $
                            ysize = 24)
    
    help_done = widget_button(help_base, $
                              value = 'Done', $
                              uvalue = 'help_done')

    widget_control, help_base, /realize
    xmanager, 'ftv_help', help_base, /no_block
    
endif

end

;----------------------------------------------------------------------

pro ftv_help_event, event

widget_control, event.id, get_uvalue = uvalue

case uvalue of
    'help_done': widget_control, event.top, /destroy
    else:
endcase

end

;----------------------------------------------------------------------

pro ftv_mapphot_refresh

; Aperture photometry routine by W. Colley, adapted for 
; inclusion in FTV by AJB

common ftv_state
common ftv_images

; coarse center on the star

xmin = (state.cursorpos[0] - ((state.centerboxsize - 1) / 2)) > 0
xmax = (xmin + state.centerboxsize) < (state.image_size[0] - 1)
ymin = (state.cursorpos[1] - ((state.centerboxsize - 1) / 2)) > 0
ymax = (ymin + state.centerboxsize) < (state.image_size[1] - 1)

small_image = main_image[xmin:xmax, ymin:ymax]

nx = (size(small_image))[1]
ny = (size(small_image))[2]

if (total(small_image) EQ 0.) then small_image = small_image + 1.

tt = findgen(nx)#(fltarr(ny)+1)
xcenter = round(total(tt*small_image)/float(total(small_image)))
tt = (fltarr(nx)+1)#findgen(ny)
ycenter = round(total(tt*small_image)/float(total(small_image)))

x = 0 > (xcenter + xmin) < (state.image_size[0] - 1)
y = 0 > (ycenter + ymin) < (state.image_size[1] - 1)

; calculate the sky

xmin = (x - state.outersky) > 0
xmax = (xmin + (2 * state.outersky + 1)) < (state.image_size[0] - 1)
ymin = (y - state.outersky) > 0
ymax = (ymin + (2 * state.outersky + 1)) < (state.image_size[1] - 1)

small_image = main_image[xmin:xmax, ymin:ymax]
nx = (size(small_image))[1]
ny = (size(small_image))[2]
i = lindgen(nx)#(lonarr(ny)+1)
j = (lonarr(nx)+1)#lindgen(ny)
xc = x - xmin
yc = y - ymin

w = where( (((i - xc)^2 + (j - yc)^2) GE state.innersky^2) AND $
           (((i - xc)^2 + (j - yc)^2) LE state.outersky^2),  nw)
if nw EQ 0 then begin
    print, 'No pixels in sky!!!!'
    xcent = -1.
    ycent = -1.
    sky = -1.
    flux = -1.
    goto, BADSKIP
endif

if nw GT 0 then sky = median(small_image(w))

; do the photometry

mag = 1.

s = size(main_image)
nxi = s[1]
nyi = s[2]

instr = string(0)

nx = ceil(state.r*2.)+4
pi = !pi
twopi = pi*2.

flux = float(x-x)
xcent = flux
ycent = flux
s = size(x)

i = findgen(nx)-float(nx)*0.5
ii0 = i # (i-i+1)
jj0 = (i-i+1) # i

i = (findgen(nx*mag)-(float(nx)*0.5)*mag - mag*0.5 + 0.5) / mag
ii1 = i # (i-i+1)
jj1 = (i-i+1) # i

str = string(0)
    
xcent = 0.
ycent = 0.

ix = floor(x)
iy = floor(y)

xi = (x-float(ix))
yi = (y-float(iy))

rx = ii0-xi
ry = jj0-yi

mask0 = (rx*rx + ry*ry le (state.r-0.5)^2)
clipmask = (rx*rx + ry*ry le (state.r+0.5)^2)

rx = ii1-xi
ry = jj1-yi

mask1 = (rx*rx + ry*ry le state.r^2)

bm0 = rebin(mask0,nx*mag,nx*mag,/sample)
mask2 = (mask1 eq 1) * (bm0 eq 0)

norm = total(mask1)

ix = floor(x)
iy = floor(y)

i1 = ix-nx/2
i2 = ix+nx/2-1
j1 = iy-nx/2
j2 = iy+nx/2-1

if ((i1 lt state.r) or (i2 gt nxi-state.r-1) or $
    (j1 lt state.r) or (j2 gt nyi-state.r-1)) then begin
        
    xcent = -1.
    ycent = -1.
    flux = -1.
    
endif else begin
    
    marr_sml = main_image[i1:i2,j1:j2] - sky
    marr = marr_sml
    t = marr*mask2+rebin(mask0*marr_sml,nx*mag,nx*mag,/sample)
    
    xcent = total(ii1*t)/total(t) + float(i1+nx/2)
    ycent = total(jj1*t)/total(t) + float(j1+nx/2)
    
    flux = total(marr_sml*mask0) + total(marr*mask2)
        
endelse

BADSKIP: begin
end

; output the results
  
state.centerpos = [xcent, ycent]

;**FUSE: emurphy changed the following lines from i4 to i5
tmp_string = string(state.cursorpos[0], state.cursorpos[1], $
                    format = '("Cursor position:  x=",i5,"  y=",i5)' )
tmp_string1 = string(state.centerpos[0], state.centerpos[1], $
                    format = '("Object centroid:  x=",f6.1,"  y=",f6.1)' )
tmp_string2 = string(flux, $
                    format = '("Object counts: ",g12.6)' )
tmp_string3 = string(sky, $
                    format = '("Sky level: ",g12.6)' )

widget_control, state.centerbox_id, set_value = state.centerboxsize
widget_control, state.cursorpos_id, set_value = tmp_string
widget_control, state.centerpos_id, set_value = tmp_string1
widget_control, state.radius_id, set_value = state.r 
widget_control, state.outersky_id, set_value = state.outersky
widget_control, state.innersky_id, set_value = state.innersky
widget_control, state.skyresult_id, set_value = tmp_string3
widget_control, state.photresult_id, set_value = tmp_string2

end

;----------------------------------------------------------------------

pro ftv_mapphot_event, event

common ftv_state
common ftv_images

widget_control, event.id, get_uvalue = uvalue

case uvalue of

    'centerbox': begin
        state.centerboxsize = long(event.value) > 0
        if ( (state.centerboxsize / 2 ) EQ $
             round(state.centerboxsize / 2.)) then $
          state.centerboxsize = state.centerboxsize + 1
        ftv_mapphot_refresh
    end
        
    'radius': begin
        state.r = 1 > long(event.value) < state.innersky
        ftv_mapphot_refresh
    end

    'innersky': begin
        state.innersky = state.r > long(event.value) < (state.outersky - 1)
        ftv_mapphot_refresh
    end

    'outersky': begin
        state.outersky = long(event.value) > (state.innersky + 1)
        ftv_mapphot_refresh
    end

    'mapphot_done': widget_control, event.top, /destroy
    else:
endcase

end

;----------------------------------------------------------------------

pro ftv_mapphot

; aperture photometry front end

common ftv_state

state.cursorpos = state.mouse

if (not (xregistered('ftv_mapphot'))) then begin

    mapphot_base = $
      widget_base(/floating, $
                  /base_align_left, $
                  group_leader = state.base_id, $
                  /column, $
                  title = 'ftv aperture photometry', $
                  uvalue = 'mapphot_base')
    
;**FUSE: emurphy changed the following lines from i4 to i5
    tmp_string = $
      string(1000, 1000, $
             format = '("Cursor position:  x=",i5,"  y=",i5)' )

    state.cursorpos_id = $
      widget_label(mapphot_base, $
                   value = tmp_string, $
                   uvalue = 'cursorpos')

    state.centerbox_id = $
      cw_field(mapphot_base, $
               /long, $
               /return_events, $
               title = 'Centering box size (pix):', $
               uvalue = 'centerbox', $
               value = state.centerboxsize, $
               xsize = 5)
    
    tmp_string1 = $
      string(99999.0, 99999.0, $
             format = '("Object centroid:  x=",f7.1,"  y=",f7.1)' )
    
    state.centerpos_id = $
      widget_label(mapphot_base, $
                   value = tmp_string1, $
                   uvalue = 'centerpos')
    
    state.radius_id = $
      cw_field(mapphot_base, $
               /long, $
               /return_events, $
               title = 'Aperture radius:', $
               uvalue = 'radius', $
               value = state.r, $
               xsize = 5)
    
    state.innersky_id = $
      cw_field(mapphot_base, $
               /long, $
               /return_events, $
               title = 'Inner sky radius:', $
               uvalue = 'innersky', $
               value = state.innersky, $
               xsize = 5)
    
    state.outersky_id = $
      cw_field(mapphot_base, $
               /long, $
               /return_events, $
               title = 'Outer sky radius:', $
               uvalue = 'outersky', $
               value = state.outersky, $
               xsize = 5)
    
    tmp_string3 = string(10000000.00, $
                         format = '("Sky level: ",g12.6)' )
    
    state.skyresult_id = $
      widget_label(mapphot_base, $
                   value = tmp_string3, $
                   uvalue = 'skyresult')
    
    tmp_string2 = string(1000000000.00, $
                         format = '("Object counts: ",g12.6)' )
    
    state.photresult_id = $
      widget_label(mapphot_base, $
                   value = tmp_string2, $
                   uvalue = 'photresult', $
                   /frame)
    
    mapphot_done = $
      widget_button(mapphot_base, $
                    value = 'Done', $
                    uvalue = 'mapphot_done')
    
    widget_control, mapphot_base, /realize
    
    xmanager, 'ftv_mapphot', mapphot_base, /no_block
    
endif

ftv_mapphot_refresh

end

;---------------------------------------------------------------------

pro ftv_textlabel

; widget front end for ftvxyouts, AJB 7/27/99


formdesc = ['0, text, , label_left=Text: , width=15', $
            '0, integer, 0, label_left=x: ', $
            '0, integer, 0, label_left=y: ', $
            '0, droplist, black|red|green|blue|cyan|magenta|yellow|white,label_left=Color:, set_value=0 ', $
            '0, float, 2.0, label_left=Charsize: ', $
            '0, integer, 1, label_left=Charthick: ', $
            '0, integer, 0, label_left=Orientation: ', $
            '1, base, , row', $
            '0, button, Cancel, quit', $
            '0, button, DrawText, quit']
            
textform = cw_form(formdesc, /column, $
                   title = 'ftv text label')

if (textform.tag9 EQ 1) then begin
    ftvxyouts, textform.tag1, textform.tag2, textform.tag0, $
      color = textform.tag3, charsize = textform.tag4, $
      charthick = textform.tag5, orientation = textform.tag6
endif

end

;---------------------------------------------------------------------

pro ftv_oplotcontour

; widget front end for ftvcontour,  AJB 7/27/99

common ftv_state
common ftv_images

minvalstring = strcompress('0, float, ' + string(state.min_value) + $
                           ', label_left=MinValue: , width=15 ')
maxvalstring = strcompress('0, float, ' + string(state.max_value) + $
                           ', label_left=MaxValue: , width=15')

formdesc = ['0, droplist, black|red|green|blue|cyan|magenta|yellow|white,label_left=Color:, set_value=0 ', $
;            '0, float, 1.0, label_left=Charsize: ', $
;            '0, integer, 1, label_left=Charthick: ', $
            '0, droplist, solid|dotted|dashed|dashdot|dashdotdotdot|longdash, label_left=Linestyle: , set_value=0', $
            '0, integer, 1, label_left=LineThickness: ', $
            minvalstring, $
            maxvalstring, $
            '0, integer, 6, label_left=NLevels: ', $
            '1, base, , row,', $
            '0, button, Cancel, quit', $
            '0, button, DrawContour, quit']
            
cform = cw_form(formdesc, /column, $
                   title = 'ftv text label')


if (cform.tag8 EQ 1) then begin
    ftvcontour, main_image, c_color = cform.tag0, $
;      c_charsize = cform.tag1, c_charthick = cform.tag2, $
      c_linestyle = cform.tag1, $
      c_thick = cform.tag2, $
      min_value = cform.tag3, max_value = cform.tag4, $, 
      nlevels = cform.tag5
endif

end

;----------------------------------------------------------------------

; Main program routine for FTV.  If there is no current FTV session,
; then run ftv_startup to create the widgets.  If FTV already exists,
; then display the new image to the current FTV window.

pro ftv, image, $
             min = minimum, $
             max = maximum, $
             autoscale = autoscale,  $
             linear = linear, $
             log = log, $
             histeq = histeq

common ftv_state
common ftv_images

if ( (n_params() EQ 0) AND (xregistered('ftv'))) then begin
    print, 'USAGE: ftv, array_name'
    print, '            [,min = min_value] [,max=max_value]'
    print, '            [,/autoscale] [,/linear] [,/log] [,/histeq]'
    retall
endif

if ( (n_params() NE 0) AND (size(image, /tname) NE 'STRING') ) then begin
    if ( (size(image))[0] NE 2) then begin
        print, 'Input data must be a 2-d array!'
        retall
    endif
endif

; If image is a filename, read in the file
if ( (n_params() NE 0) AND (size(image, /tname) EQ 'STRING') ) then begin
    fits_read, image, tmp_image
    if ( (size(tmp_image))[0] NE 2 ) then begin
        print, 'Error-- selected file is not a 2-D fits image!'
        junk = size( temporary(tmp_image))
        retall
    endif
    main_image = temporary(tmp_image)
    imagename = image
endif

if ( (n_params() EQ 0) AND (not (xregistered('ftv')))) then begin
;   Define default startup image 
    main_image = byte((findgen(500)*2-200) # (findgen(500)*2-200))
    imagename = ''
endif else begin
    scaled_image = 0
    display_image = 0
    if (size(image, /tname) NE 'STRING') then begin
; If user has passed ftv a data array, read it into main_image.
        main_image = image
        imagename = ''
    endif
endelse

if (not (xregistered('ftv'))) then ftv_startup

ftv_getstats
ftv_settitle, imagename

; check for command line keywords

if n_elements(minimum) GT 0 then begin
    state.min_value = minimum
endif

if n_elements(maximum) GT 0 then begin 
    state.max_value = maximum
endif

if state.min_value GE state.max_value then begin
    state.min_value = state.max_value - 1.
endif

ftv_set_minmax

if (keyword_set(autoscale)) then ftv_autoscale

if (keyword_set(linear)) then state.scaling = 0
if (keyword_set(log))    then state.scaling = 1
if (keyword_set(histeq)) then state.scaling = 2

state.zoom_level = 0
state.zoom_factor = 1.0

ftv_displayall

end






