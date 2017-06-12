pro multiplot_spectra,str1_arr,str2_arr,str3_arr,str4_arr,$
    flux=flux,norm=norm,column=column,apparent=apparent,$
    data=data,$
    wavelength=wavelength,vframe=vframe,sub=sub,$
    dimensions=dimensions,ygap=ygap,xgap=xgap,$
    device=device,ps=ps,filename=filename,$
    landscape=landscape,portrait=portrait,$
    xsize=xsize,ysize=ysize,$
    str1_factor=str1_factor,str2_factor=str2_factor,$
    str3_factor=str3_factor,str4_factor=str4_factor,$
    bad_range1=bad_range1,bad_range2=bad_range2,$
    bad_range3=bad_range3,bad_range4=bad_range4,$
    bad_color=bad_color,$
    all_res_factor=all_res_factor,all_bin_factor=all_bin_factor,$
    res_factor1=res_factor1,bin_factor1=bin_factor1,$
    res_factor2=res_factor2,bin_factor2=bin_factor2,$
    res_factor3=res_factor3,bin_factor3=bin_factor3,$
    res_factor4=res_factor4,bin_factor4=bin_factor4,$    
    labels=labels,label_factor=label_factor,position=position,$
    left=left,right=right,top=top,bottom=bottom,$
    text=text,tcolor=tcolor,lcolor=lcolor,$
    xrange=xrange,yrange=yrange,yminimum=yminimum,ymaximum=ymaximum,$
    yaxis_factor=yaxis_factor,$
    charsize=charsize,charthick=charthick,thick=thick,$
    xtitle=xtitle,ytitle=ytitle,title=title,$
    xminor=xminor,yminor=yminor,$
    xtickformat=xtickformat,ytickformat=ytickformat,$
    yticks=yticks,xticks=xticks,yfactor=yfactor,multiple=multiple,$
    color=color,$
    line=line,dotline=dotline,fill=fill,tagfill=tagfill,$
    polyfill=polyfill,tagpolyfill=tagpolyfill,$
    arrows=arrows,$
    xticklen=xticklen,yticklen=yticklen,xtickthick=xtickthick,$
    ymultiple=ymultiple

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; Purpose: To allow users to flexibly plot multiple spectra on one plot. 
;          Allows users to plot flux, normalized flux, apparent column 
;          density, and the difference in apparent column density between
;          two spectra with ease. 
;
;  str1_arr, str2_arr, str3_arr, str4_arr - String arrays containing the name of the spectra structure. 
;               Expected spectra tag names include vel, wave, flux, norm, and column. User must define str1_arr. If 
;               additional structure string arrays are past, then the panels will contain multiple over plotted spectra.
;               To plot an empty panel, pass 'blank', case insensitive. 
;  flux       - Flux keyword, equal to 0 or 1. if set, all spectral plots will be in flux units. 
;  norm       - Normalized flux keyword, equal to 0 or 1. If set, all spectral plots will be of normalized fluxes.  
;  apparent   - [DEFAULT:1] Apparent column density keyword, equal to 0 or 1. If set, all spectral plot panels will 
;               contain apparent column densities. If flux or norm are not set, then apparent=1 is assumed. Also, if 
;               sub is set, then apparent=1 will be set and flux=0 and norm=0 will be set. 
;  column     - Same as apparent.
;  wavelength - Set x variable to wavelength. Default is velocity. 
;  sub        - String arrays containing the name of the structure containing the spectra to be subtracted from defined 
;               str1_arr, str2_arr, str3_arr, and str4_arr arrays.
;  Dimensions - Number of plotted panels in [rows, columns]. If not set, then rows = number of passed spectra and 
;               columns = 1. Number of rows * number of columns MUST equal the number of passed spectra, 
;               else 1 column is assumed. 
;  xgap, ygap - Spacing between rows and columns in normalized units (between 0 and 1 for entire plotting region). 
;               Default is zero spacing. xgap=.01 & ygap=0.02 produce nice results. 
;  device     - Sets the plotting medium to either a postscript with 'PS' or to a terminal plot with 'X'. 
;               Default is 'X' unless filename is passed. If device is set to 'PS' and no file name is set, 
;               then filename='multiplot_spectra.eps' 
;  ps         - Sets device to 'PS'
;  filename   - Name of postscript file for plot. If set, device is set to 'PS'. Can pass names in the form of
;               filename and filename.eps. The program will check for and apply *.eps extension if needed. 
;  landscape  - Set postscript file to landscape, 10 in by 8 in.
;  portrait   - Set postscript to portrait, 8 in by 10 in
;  xsize, ysize - Sets either the x or y dimensions of the ps 
;  str#_factor - Multiple the column density of a given str_array by a factor.
;  bad_range# - Sets the range of a saturated region. [panel#, min value, max value]
;  bad_color  - Color to shade the bad saturated values defined in the bad_range# keyword. 
;               Specify a color string, e.g., 'white'. 'blk4' is default             
;  all_res_factor -
;  res_factor1, res_factor2, res_factor3, res_factor4 -
;  all_bin_factor -  
;  bin_factor1, bin_factor2, bin_factor3, bin_factor4 -
;  labels     - Add ion + wavelength label to all panels. Default position is in the top right corner.
;               If the ion is HI, then lambda and the wavelength is excluded.  
;  position   - Position integer array of size str1_arr that specifies the location of the labels, 
;               such that top-right => 0, bottom-right => 1, bottom-left => 2, top-left => 3 (ordered clockwise).
;               The default is all location at top right. If the number of elements does not equal the number
;               of elements in str1_arr, then the unspecified panels will be labeled in the top right corner.
;  left       - Position of the label. Default is top-left corner, but /bottom will set to bottom-left corner. 
;  right      - Position of the label. Default is top-right corner, but /bottom will set to bottom-right corner. 
;  top        - Position of the label. Default is top-left corner, but /left will set to top-left corner. 
;  bottom     - Position of the label. Default is bottom-left corner, but /right will set to bottom-right corner. 
;  text       - String array that adds a text label to specified panel. The labeling position will either
;               be above, if /bottom is set, or below, if /top is set, the label. Note that labels does not 
;               have to be set and that the location can be controlled by the position, left, right, top, and bottom
;               keywords even if the ion lambda wavelength label is excluded. 
;  tcolor     - Color of text label
;  xrange     - [xmin, xmax]. Default is [min(xarr), max(xarr)]  
;  yrange     - [ymin, ymax]. Default is [min(yarr)-small spacing, max(yarr)+small spacing] over the xrange.
;  yminumum   - Must be called with yaxis_factor 
;  ymaximum   - Must be called with yaxis_factor
;  yaxis_factor - [centeral location, lower factor, upper factor], e.g., [0,0.2,0.8]
;               This means that the yaxis will be centered about zero with 20% of the axis below 0 and 80% above.
;               If lower factor + upper factor is not equal to 1, then they will be normalized so they are.
;               This keyword must be called in conjunction with either yminimum or ymaximum
;  charsize   - Sets size of characters. Default is size of 1.25.
;  charthick  - Sets thickness of characters. Default is thick of 2. 
;               NOTE: This keyword has no affect when plotting to a postscript. It's an intrinsic IDL blunder. 
;  thick      - Specify thickness of line. If !p.thick is equal to or less than 2 and device is set to 'PS', then 
;               thick is set to 5 unless thick is set. If !p.thick is equal to or greater than 4 and 'X' is set, 
;               then thick=2 unless thick is set. 
;  xtitle     - String containing the title of the horizontal axis. If number of elements equal to the number of panels, then 
;               each panel will be labeled with that specific title; if these numbers are not equal, then only the first 
;               string passed will be used.
;               Defaults:
;               if wavelength not set or sub is set, then xtitle='LSR Velocity (km s!U-1!N)' 
;  ytitle     - String containing the title of the vertical axis. If number of elements equal to the number of panels, then 
;               each panel will be labeled with that specific title; if these numbers are not equal, then only the first 
;               string passed will be used. 
;               Defaults:
;               If flux is set, then ytitle='Flux in either [counts s!U-1!N] or [ergs s!U-1!N cm!U-2!N '+string(197b)+']'
;               If norm is set, then ytitle='Normalized Flux'. 
;               If apparent is set, then ytitle='Apparent Column Density [cm!U-2!N/(km s!U-1!N)]'
;               If sub is set, then ytitle='Residuals of Apparent Column Density [cm!U-2!N/(km s!U-1!N)]'
;               if wavelength is set and sub isn't set, then xtitle='Wavelength (Angstroms)'
;  title      - String containing title name of the plot.
;  xminor     - Number of minor x-ticks. Default: xminor=2
;  yminor     - Number of minor y-ticks. Default: xminor=1
;  xtickformat - Format of xtick labels, e.g., format='(f8.2)'. User does not have to specify the format for each tick as
;                the format will be applied to all ticks. If not set, then format is determined by idl.
;  ytickformat - Format of ytick labels, e.g., format='(f8.2)'. User does not have to specify the format for each tick as
;                the format will be applied to all ticks. If not set, then format is determined by idl.
;  xticks      -
;  yticks      -
;  yfactor     - Scaling factor for y-axis, e.g., if you want to transform the y-axis units to 1e12, set yfactor=1e12. 
;                The ytitle adjusts to add this change.
;  multiple    -
;  color       - 
;  line        - Specifies velcity or wavelength position to draw a vertical line. 
;  fill        - Specifies the range to highlight spectra. This is a two elelement array. To highlight the same range 
;                over all spectra, call only fill. To specify which panels to heighlight, call tagfill keyword.
;                Only highlights spectra passed by str1_arr.
;                   E.g., fill=[100,200] => all panels highlighted over the xrange [100,200]
;                   E.g., fill=[[100,200],[400,600]] => all pannels highlighted over the xrange [100,200] & [400,600]
;  tagfill     - Tags the panels and fill indexes to be highlighted with fill for each panel. Tag must have the 
;                   dimensions of [n_elements(fill),n_elements(spectra)]. 0=nofill, 1=first range specified in fill, etc.
;                   E.g.,  fill=[[100,200],[400,600]], tagfill=[[0,1],[1,1]] => For a panel plot, highlight only 
;                   [100,200] in the first panel, but highlight [100,200] and [400,600] in the second panel
;                   E.g., fill=[[100,200],[300,400]],tagfill=[[1,0],[1,1],[0,0],[0,1]] for four panels.
;  arrows      - Adds an arrow plus number identifier over a spectral feature at a specific xposition and in a specific panel
;                   [[panel index, xpos]]. Pannel number starts with 1. A positive panel number results in a down arrow and 
;                   negitive panel number results in an up arrow. 
;                   e.g., arrows=[[1,500],[2,400]] => label the feature at xposition 500 in panel 0 and the 400 feature in panel 1.
;                   e.g., arrows=[[1,400],[1,500]] => label both the 400 and 500 features in panel 0
;  xticklength -
;  yticklength - 
;  xtickthick  - 
;  ymultiple   - 
;
;
; e.g., str1_arr=['CAL_F_ALII1670IM']
;       multiplot_spectra,str1_arr 
; e.g., multiplot_spectra,[str1_arr,str1_arr,str1_arr,str1_arr,str1_arr,str1_arr,str1_arr,str1_arr],charsize=1.1,dim=[4,2],ygap=0.02,xgap=.01
;
; multiplot_spectra,HD33133_ordered,xrange=[-200,600],dim=[10,2],yfactor=1.e12
;
; Created by Dr. Kat Barger
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;m=number of rows
;n=number of columns
if (NOT keyword_set(dimensions)) then begin
   m_total=n_elements(str1_arr)
   n_total=1
endif else begin
   m_total=dimensions[0]
   if n_elements(dimensions) eq 2 then n_total=dimensions[1] $
      else n_total=1
endelse
if (m_total*n_total) ne n_elements(str1_arr) then begin
   m_total=n_elements(str1_arr) & n_total=1
   print,''
   print,'*** Must have an equal number of rows in each column ***'
   print,''
endif

;If fill is set and the user doesn't specify which panels to highlight, then highlight all of them for each supplied range.
if keyword_set(fill) and (not keyword_set(tagfill)) then tagfill=(intarr(m_total*n_total)+1)##(indgen(n_elements(fill[0,*]))+1)
if keyword_set(polyfill) and (not keyword_set(tagpolyfill)) then tagpolyfill=(intarr(m_total*n_total)+1)##(indgen(n_elements(polyfill[0,*]))+1)

;Only subtract apparent column densities.
if keyword_set(sub) AND ((n_elements(sub) ne n_total*m_total))  then begin
   sub=0
   print,'*** Elements of sub string structure names must equal number structure names passed ***'
   print,'*** Skipping subtraction. ***' 
endif
if keyword_set(sub) AND (n_elements(sub) eq n_total*m_total) then begin
   flux=0
   norm=0
   column=1
   wavelength=0
endif

if (keyword_set(all_res_factor)) AND (NOT keyword_set(res_factor1)) AND (NOT keyword_set(res_factor2)) $
   AND (NOT keyword_set(res_factor3)) AND (NOT keyword_set(res_factor4)) then begin
       res_factor1=all_res_factor
       res_factor2=all_res_factor       
       res_factor3=all_res_factor
       res_factor4=all_res_factor    
   endif

if (NOT keyword_set(res_factor1)) then res_factor1=fltarr(m_total*n_total)+1. 
if (NOT keyword_set(res_factor2)) then res_factor2=fltarr(m_total*n_total)+1. 
if (NOT keyword_set(res_factor3)) then res_factor3=fltarr(m_total*n_total)+1. 
if (NOT keyword_set(res_factor4)) then res_factor4=fltarr(m_total*n_total)+1. 
if (n_elements(res_factor1) eq 1.) OR (n_elements(res_factor1) ne n_total*m_total) then res_factor1=fltarr(m_total*n_total)+res_factor1[0] 
if (n_elements(res_factor2) eq 1.) OR (n_elements(res_factor2) ne n_total*m_total) then res_factor2=fltarr(m_total*n_total)+res_factor2[0] 
if (n_elements(res_factor3) eq 1.) OR (n_elements(res_factor3) ne n_total*m_total) then res_factor3=fltarr(m_total*n_total)+res_factor3[0] 
if (n_elements(res_factor4) eq 1.) OR (n_elements(res_factor4) ne n_total*m_total) then res_factor4=fltarr(m_total*n_total)+res_factor4[0] 

if (keyword_set(all_bin_factor)) AND (NOT keyword_set(bin_factor1)) AND (NOT keyword_set(bin_factor2)) $
   AND (NOT keyword_set(bin_factor3)) AND (NOT keyword_set(bin_factor4)) then begin
       bin_factor1=all_bin_factor
       bin_factor2=all_bin_factor       
       bin_factor3=all_bin_factor
       bin_factor4=all_bin_factor    
   endif

if (NOT keyword_set(bin_factor1)) then bin_factor1=fltarr(m_total*n_total)+1. 
if (NOT keyword_set(bin_factor2)) then bin_factor2=fltarr(m_total*n_total)+1. 
if (NOT keyword_set(bin_factor3)) then bin_factor3=fltarr(m_total*n_total)+1. 
if (NOT keyword_set(bin_factor4)) then bin_factor4=fltarr(m_total*n_total)+1. 
if (n_elements(bin_factor1) eq 1.) OR (n_elements(bin_factor1) ne n_total*m_total) then bin_factor1=fltarr(m_total*n_total)+bin_factor1[0]
if (n_elements(bin_factor2) eq 1.) OR (n_elements(bin_factor2) ne n_total*m_total) then bin_factor2=fltarr(m_total*n_total)+bin_factor2[0]
if (n_elements(bin_factor3) eq 1.) OR (n_elements(bin_factor3) ne n_total*m_total) then bin_factor3=fltarr(m_total*n_total)+bin_factor3[0]
if (n_elements(bin_factor4) eq 1.) OR (n_elements(bin_factor4) ne n_total*m_total) then bin_factor4=fltarr(m_total*n_total)+bin_factor4[0]

if (NOT keyword_set(yfactor)) OR (keyword_set(norm)) then yfactor=1.0
if (NOT keyword_set(title)) then title=' '
grand_ytitle=' '
if n_total*m_total eq n_elements(ytitle) then begin
  grand_ytitle=' ' 
  ;ytitle=ytitle[0]
endif else if ((keyword_set(ytitle)) AND (n_elements(ytitle) eq 1)) OR $
   ((keyword_set(ytitle)) AND n_elements(ytitle) ne n_elements(str1_arr)) then begin
   grand_ytitle=ytitle[0]
   ytitle=replicate(' ',n_total*m_total)
endif 
if keyword_set(norm) then begin 
   variable='NORM'
   if NOT keyword_set(ytitle) then begin
      grand_ytitle='Normalized Flux'
      ytitle=replicate(' ',n_total*m_total)
   endif
   if NOT keyword_set(yrange) then begin
      yrange=[-0.25,1.49]
      ytickv=[0,0.5,1.0]
      yticknames=['0','0.5','1.0']
   endif
endif else if keyword_set(flux) then begin
   variable='FLUX'
   if NOT keyword_set(ytitle) then begin
      grand_ytitle='Flux in either (counts s!U-1!N or ergs s!U-1!N cm!U-2!N '+string(197b)+')'
   endif
      ytitle=replicate(' ',n_total*m_total)
endif else begin
   variable='COLUMN' 
   if keyword_set(data) then variable='DATA'
   if NOT keyword_set(ytitle) then begin
      grand_ytitle='Apparent Column Density (cm!U-2!N/(km s!U-1!N))'
      if keyword_set(sub) then grand_ytitle='Residuals of '+ grand_ytitle
      ytitle=replicate(' ',n_total*m_total)
   endif
endelse 
;Finds the location of the structure tag with name equal to variable, 
;but first make sure that variable exists
if (n_elements(SCOPE_VARFETCH(str1_arr[0], /ENTER,level=1)) eq 0) AND (NOT strmatch(str1_arr[0],'*BLANK*',/fold_case)) then begin
   print,' '
   print,'*** Variable name ',str1_arr[0],' does not exist!!! ***'
   print,' '
   return
endif
;tag_loc=where(strcmp(tag_names(SCOPE_VARFETCH(str1_arr[0], /ENTER,level=1)),VARIABLE,/fold_case) eq 1)

if (keyword_set(xtitle)) and (n_elements(xtitle) ne n_total*m_total) then begin
  grand_xtitle=xtitle[0] 
   xtitle=replicate(' ',n_total*m_total)
endif else if n_total*m_total eq n_elements(xtitle) then begin
  grand_xtitle=' ' 
  xtitle=xtitle
endif else if ((NOT keyword_set(xtitle)) AND (NOT keyword_set(wavelength))) AND $
   (n_elements(xtitle) ne n_total*m_total) then begin
   grand_xtitle='LSR Velocity (km s!U-1!N)' 
   xtitle=replicate(' ',n_total*m_total)
endif else if ((NOT keyword_set(xtitle)) and (keyword_set(wavelength))) OR $
   (n_elements(xtitle) ne n_total*m_total) then begin
   grand_xtitle='Wavelength (Angstroms)'
   xtitle=replicate(' ',n_total*m_total)
endif 
if ytitle[0] ne ' ' then begin
;Add yfactor to the displayed ytitle
   yscalar=strcompress(string(fix(alog10(yfactor))),/re)  
   for i=0, n_elements(ytitle)-1 do begin
      temp_ytitle=strsplit(ytitle[i],'(',/extract,count=ygood)
      if (yfactor ne '0') AND (ygood eq 2) AND (NOT keyword_set(norm))$
         then ytitle[i]=temp_ytitle[0]+'(10!U'+yscalar+'!N '+temp_ytitle[1] $
      else if (yscalar ne '0') AND (ygood eq 1) AND (NOT keyword_set(norm)) $
         then ytitle[i]=temp_ytitle[0]+' x 10!U'+yscalar+'!N' 
   endfor
endif

;Sets x-axis variable
if (NOT keyword_set(wavelength)) then xtag='vel' else xtag='wave'
if (NOT validate_numeric(vframe)) OR (xtag eq 'wave') then vframe=[0.0]

if (NOT validate_numeric(str1_factor)) OR (variable NE 'COLUMN') then str1_factor=1.0
if (NOT validate_numeric(str2_factor)) OR (variable NE 'COLUMN') then str2_factor=1.0
if (NOT validate_numeric(str3_factor)) OR (variable NE 'COLUMN') then str3_factor=1.0
if (NOT validate_numeric(str4_factor)) OR (variable NE 'COLUMN') then str4_factor=1.0

if keyword_set(bad_range1) then bad_range1=transpose(bad_range1)
if keyword_set(bad_range2) then bad_range2=transpose(bad_range2)
if keyword_set(bad_range3) then bad_range3=transpose(bad_range3)
if keyword_set(bad_range4) then bad_range4=transpose(bad_range4)

if (NOT keyword_set(xrange)) then begin
  if (strmatch(str1_arr[0],'*BLANK*',/fold_case)) then begin
    print,''
    print,'*** Because the first element is BLANK, you must specify the xrange ***'
    print,''
    return
  endif
    xtag_loc=where(strcmp(tag_names((SCOPE_VARFETCH(str1_arr[0], /ENTER,level=1))),xtag,/fold_case) eq 1)
    x=(SCOPE_VARFETCH(str1_arr[0], /ENTER,level=1)).(xtag_loc)
    xrange=[min(x,/nan),max(x,/nan)]
endif

if keyword_set(filename) OR keyword_set(PS) then device='PS' else device='X'
if strmatch(device,'ps',/fold_case) then begin

   set_plot,device 
   if keyword_set(filename) then $
      name=validate_extension(filename,'eps') $
   else name='multiplot_spectra.eps'

   entry_device=!d.name
;   !p.font=-1 ;switch from vector to device fonts
;   device, /helvetica ; a classic sans-serif font
;   device,filename=name,bits_per_pixel=8,color=1
;
;   ;If only xsize or ysize is set, but not both, OR if both are unset
;   if ((keyword_set(xsize)) XOR (keyword_set(ysize))) OR $
;      ((NOT keyword_set(xsize)) AND (NOT keyword_set(ysize))) then begin 
;   if (NOT keyword_set(xsize)) then xsize=0.
;      if (NOT keyword_set(ysize)) then ysize=0.   
;      if keyword_set(landscape) then $
;         device,/landscape,xsize=10+xsize,xoffset=(11.0-10)*0.5,yoffset=10+xsize,/inches $
;      else device,/portrait,ysize=10+ysize,yoffset=(11.0-10)*0.5,xoffset=10+ysize,/inches
;      device,encapsulated=1,/helvetica
;   endif else begin
;   ;Both xsize and ysize set. Set the dimensions of the postscript to xsize inches by ysize inches
;      device,xsize=xsize,ysize=ysize
;   endelse
;
;         ;Device, _extra=PSConfig(name=name,FontType=-1,xsize=xsize,ysize=ysize,encapsulated=1,/nogui)

if keyword_set(portrait) then undefine,xsize,ysize,landscape


if n_elements(xsize) then if xsize eq 0 then undefine, xsize
if n_elements(ysize) then if ysize eq 0 then undefine, ysize
keywords = PSConfig(/nogui,xsize=xsize,ysize=ysize,Encapsulated=1,$
  Filename=name,FontType=-1,landscape=landscape,/helvetica)
  thisDevice = !D.Name
  Set_Plot, 'PS'
  Device, _Extra=keywords

   !x.thick=5
   !y.thick=5
   if (!p.thick le 2) AND (NOT keyword_set(thick)) then !p.thick=5 $
   else if (keyword_set(thick)) then !p.thick=thick else !p.thick=5
   lambda=cgsymbol('lambda') ;'!9l!3'

endif else begin

   set_plot,device
   if (!p.thick ge 4) AND (NOT keyword_set(thick)) then !p.thick=2
   if (!x.thick ge 4) AND (NOT keyword_set(thick)) then !x.thick=2
   if (!y.thick ge 4) AND (NOT keyword_set(thick)) then !y.thick=2
   lambda=cgsymbol('lambda') ;'!7k!3'

endelse

if (NOT keyword_set(xtickthick)) then xtickthick=!p.thick
if (NOT keyword_set(ymultiple)) then ymultiple=0

;if top right, then position=0; if bottom right, then position=1;
;if bottom left, then position=2, if top left, then position=3
if (NOT keyword_set(labels)) AND (keyword_set(position)) then labels=1
if (NOT keyword_set(position)) then position = intarr(m_total*n_total)
if keyword_set(labels) AND n_elements(position) ne n_total*m_total then begin
   print,'*** Number of elements in position ne to number of elements in str1_arr. ***'
   print,'             *** Setting all unset labels to top right. ***'
   position_new=intarr(n_total*m_total)
   position_new[0:n_elements(position)-1]=position
   position=position_new
endif
if ((NOT keyword_set(position)) AND (keyword_set(labels))) $
   OR n_elements(position) ne n_total*m_total then position=intarr(n_total*m_total)
if (keyword_set(right) or (NOT keyword_set(left))) AND $
   (NOT keyword_set(bottom)) AND (NOT keyword_set(position)) then position=intarr(n_total*m_total) 
if (keyword_set(right) or (NOT keyword_set(left))) AND keyword_set(bottom) then position=intarr(n_total*m_total)+1
if keyword_set(left) AND keyword_set(bottom) then position=intarr(n_total*m_total)+2
if keyword_set(left) AND (NOT keyword_set(bottom)) then position=intarr(n_total*m_total)+3  

if (NOT keyword_set(charsize)) then charsize=1.25
if (NOT keyword_set(charthick)) then charthick=2.0
if (NOT keyword_set(xminor)) then xminor=2.0
if (NOT keyword_set(yminor)) then yminor=1.0
if (NOT keyword_set(xticks)) then xticks=0
if (NOT keyword_set(yticks)) then yticks=0 
if (NOT keyword_set(xtickformat)) then xtickformat=replicate('',10) else xtickformat=replicate(xtickformat[0],10)
if (NOT keyword_set(ytickformat)) then ytickformat=replicate('',10) else ytickformat=replicate(ytickformat[0],10)
if n_elements(ytitle) eq n_elements(str1) then !p.position=[0.125,0.2,0.9,0.95] $
   else !p.position=[0.125,0.2,0.95,0.95]

if (NOT keyword_set(ygap)) then ygap=0.0
if (NOT keyword_set(xgap)) then xgap=0.0
if keyword_set(yrange) then yrange_user=yrange
if (NOT keyword_set(yrange_user)) AND (n_total gt 2) then xgap = 0.035*charsize

!p.background=fsc_color('white')
!p.color=fsc_color('black')
if (NOT keyword_set(bad_color)) then bad_color=fsc_color('blk4') else bad_color=fsc_color(bad_color)
if keyword_set(color) AND (size(color,/type) eq 2) then $
  color=[fsc_color('black'),fsc_color('forest green'),fsc_color('blue'),fsc_color('brown')] $
else if keyword_set(color) AND (size(color,/type) eq 7) then begin
   if n_elements(color) eq 1 then color=[fsc_color(color[0]),fsc_color('black'),fsc_color('black'),fsc_color('black')] $
   else if n_elements(color) eq 2 then color=[fsc_color(color[0]),fsc_color(color[1]),fsc_color('black'),fsc_color('black')] $
   else if n_elements(color) eq 3 then color=[fsc_color(color[0]),fsc_color(color[1]),fsc_color(color[2]),fsc_color('black')] $
   else if n_elements(color) eq 4 then color=[fsc_color(color[0]),fsc_color(color[1]),fsc_color(color[2]),fsc_color(color[3])]
endif else $
 color=[fsc_color('black'),fsc_color('black'),fsc_color('black'),fsc_color('black')]

if (NOT keyword_set(text)) then text=strarr(n_total*m_total)
if (NOT keyword_set(tcolor)) then tcolor=!p.color else tcolor=fsc_color(tcolor)
if (NOT keyword_set(tcolor)) then lcolor=!p.color else lcolor=fsc_color(lcolor)

;xtickname=replicate(' ',10)
;Supplying too many blank names will guarantee that the ticks aren't labeled.
blank=replicate(' ',10) 

ip=!p.position

erase

q=0
xticks_original=xticks
yticks_original=yticks
ymin_arr=fltarr(4)
ymax_arr=fltarr(4)
;m=number of rows
;n=number of columns
for n=0,n_total-1 do begin 
    for m=0, m_total-1 do begin 
                   ;xmin,ymin,xmax,ymax

     if (NOT strmatch(str1_arr[q],'*BLANK*',/fold_case)) then begin
       xtag_loc=where(strcmp(tag_names((SCOPE_VARFETCH(str1_arr[q], /ENTER,level=1))),xtag,/fold_case) eq 1)
       ytag_loc=where(strcmp(tag_names(SCOPE_VARFETCH(str1_arr[m+n], /ENTER,level=1)),VARIABLE,/fold_case) eq 1)    
     endif  

       !p.position=[ip[2]-(ip[2]-ip[0])/n_total*(n_total-n)+xgap/2.0,$
                    ip[3]-(ip[3]-ip[1])/m_total*(m+1)+ygap/2.0,$
                    ip[2]-(ip[2]-ip[0])/n_total*(n_total-n-1)-xgap/2.0,$
                    ip[3]-(ip[3]-ip[1])/m_total*m-ygap/2.0]

        if (n_elements(SCOPE_VARFETCH(str1_arr[q], /ENTER,level=1)) eq 0) AND (NOT strmatch(str1_arr[q],'*BLANK*',/fold_case)) then begin
           print,' '
           print,'*** Variable name ',str1_arr[q],' does not exist!!! ***'
           print,' '
           return
        endif

       if (NOT keyword_set(sub)) then begin
          if (NOT strmatch(str1_arr[q],'*BLANK*',/fold_case)) then begin
            x1=(SCOPE_VARFETCH(str1_arr[q], /ENTER,level=1)).(xtag_loc)
            if (NOT keyword_set(data)) then str1=rebin_reduce_res((SCOPE_VARFETCH(str1_arr[q], /ENTER,level=1)),bin_factor=bin_factor1[q],res_factor=res_factor1[q]) $
              else str1=(SCOPE_VARFETCH(str1_arr[q], /ENTER,level=1))
            if strmatch(variable,'column',/fold_case) then x1=str1.(xtag_loc)
            y1=str1.(ytag_loc)*str1_factor
          endif else begin 
            x1=[0,0]
            y1=[0,0]
          endelse  

          if n_elements(str2_arr) eq n_total*m_total then begin
             if n_elements(SCOPE_VARFETCH(str2_arr[q], /ENTER,level=1)) eq 0 AND (NOT strmatch(str2_arr[q],'*BLANK*',/fold_case)) then begin
                print,' '
                print,'*** Variable name ',str2_arr[q],' does not exist!!! ***'
                print,' '
                return
             endif
             if (NOT strmatch(str2_arr[q],'*BLANK*',/fold_case)) then begin
              x2=(SCOPE_VARFETCH(str2_arr[q], /ENTER,level=1)).(xtag_loc)
              if (NOT keyword_set(data)) then str2=rebin_reduce_res((SCOPE_VARFETCH(str2_arr[q], /ENTER,level=1)),bin_factor=bin_factor2[q],res_factor=res_factor2[q]) $
                else str2=(SCOPE_VARFETCH(str2_arr[q], /ENTER,level=1))
              if strmatch(variable,'column',/fold_case) then x2=str2.(xtag_loc)
              y2=str2.(ytag_loc)*str2_factor
            endif else begin 
              x2=[0,0]
              y2=[0,0]
            endelse  
        endif

          if n_elements(str3_arr) eq n_total*m_total then begin
             if n_elements(SCOPE_VARFETCH(str3_arr[q], /ENTER,level=1)) eq 0 AND (NOT strmatch(str3_arr[q],'*BLANK*',/fold_case)) then begin
                print,' '
                print,'*** Variable name ',str3_arr[q],' does not exist!!! ***'
                print,' '
                return
             endif
            if (NOT strmatch(str3_arr[q],'*BLANK*',/fold_case)) then begin
              x3=(SCOPE_VARFETCH(str3_arr[q], /ENTER,level=1)).(xtag_loc)
              if (NOT keyword_set(data)) then str3=rebin_reduce_res((SCOPE_VARFETCH(str3_arr[q], /ENTER,level=1)),bin_factor=bin_factor3[q],res_factor=res_factor3[q]) $
                else str3=(SCOPE_VARFETCH(str3_arr[q], /ENTER,level=1))
              if strmatch(variable,'column',/fold_case) then x3=str3.(xtag_loc)
              y3=str3.(ytag_loc)*str3_factor
            endif else begin 
              x3=[0,0]
              y3=[0,0]
            endelse 
          endif
   
          if n_elements(str4_arr) eq n_total*m_total then begin
             if (NOT strmatch(str3_arr[q],'*BLANK*',/fold_case)) then begin
              x4=(SCOPE_VARFETCH(str4_arr[q], /ENTER,level=1)).(xtag_loc)
              if (NOT keyword_set(data)) then str4=rebin_reduce_res((SCOPE_VARFETCH(str4_arr[q], /ENTER,level=1)),bin_factor=bin_factor4[q],res_factor=res_factor4[q]) $
                else str4=(SCOPE_VARFETCH(str4_arr[q], /ENTER,level=1))
              if strmatch(variable,'column',/fold_case) then x4=str4.(xtag_loc)
              y4=str4.(ytag_loc)*str4_factor
            endif else begin 
              x4=[0,0]
              y4=[0,0]
            endelse 
          endif

       endif else begin
          
          str1_sub1=sub_column((SCOPE_VARFETCH(str1_arr[q], /ENTER,level=1)),$
                               (SCOPE_VARFETCH(sub[q], /ENTER,level=1)))
          x1=str1_sub1.vel
          y1=str1_sub1.column
   
          if n_elements(str2_arr) eq n_total*m_total then begin
             str2_sub2=sub_column((SCOPE_VARFETCH(str2_arr[q], /ENTER,level=1)),$
                               (SCOPE_VARFETCH(sub[q], /ENTER,level=1)))
             x2=str2_sub2.vel
             y2=str2_sub2.column
          endif
   
          if n_elements(str3_arr) eq n_total*m_total then begin
             str3_sub3=sub_column((SCOPE_VARFETCH(str3_arr[q], /ENTER,level=1)),$
                               (SCOPE_VARFETCH(sub[q], /ENTER,level=1)))
             x3=str3_sub3.vel
             y3=str3_sub3.column
          endif
   
          if n_elements(str4_arr) eq n_total*m_total then begin
             str4_sub4=sub_column((SCOPE_VARFETCH(str4_arr[q], /ENTER,level=1)),$
                               (SCOPE_VARFETCH(sub[q], /ENTER,level=1)))
             x4=str4_sub4.vel
             y4=str4_sub4.column
          endif

       endelse

      if ((keyword_set(ymin3um)) AND (n_elements(yminimum) ne m_total*n_total)) OR $
         ((keyword_set(ymaximum)) AND (n_elements(ymaximum) ne m_total*n_total)) then begin
         print,' '
         print,'*** n_elements of ymin & ymax must be equal to n_elements of str1_arr ***'
         undefine,yminimum,ymaximum
      endif else if (keyword_set(yminimum)) AND (keyword_set(ymaximum)) AND $
         (n_elements(yminimum) eq m_total*n_total) AND (n_elements(ymaximum) eq m_total*n_total) then begin
         yrange=[min([yminimum[q],ymaximum[q]]),max([yminimum[q],ymaximum[q]])]
      endif else if (keyword_set(yminimum)) AND (keyword_set(yaxis_factor)) then begin
         yrange=axis_factor_range(yaxis_factor[0],yaxis_factor[1:2],min=yminimum[q])
      endif else if (keyword_set(ymaximum)) AND (keyword_set(yaxis_factor)) then begin
         yrange=axis_factor_range(yaxis_factor[0],yaxis_factor[1:2],max=ymaximum[q])
      endif 

      if ((NOT keyword_set(yrange_user)) AND (NOT keyword_set(yminimum)) AND (NOT keyword_set(ymaximum))) OR $
         ((NOT keyword_set(yrange_user)) AND (NOT keyword_set(yminimum)) AND (NOT keyword_set(yaxis_factor))) OR $
         ((NOT keyword_set(yrange_user)) AND (NOT keyword_set(ymaximum)) AND (NOT keyword_set(yaxis_factor))) then $          
           yrange=min_max_multi(x1,y1,x2,y2,x3,y3,x4,y4,range=xrange)/yfactor

      xticks=xticks_original
      yticks=yticks_original

      padding=abs(yrange[1]-yrange[0])*.1
      if keyword_set(yticks) then tickv,yrange,yticks,ytickname,ytickv,padding=padding,multiple=multiple,/zero $
      else begin
        ytickv=0
        ytickname=''
      endelse

      if keyword_set(xticks) then tickv,xrange,xticks,xtickname,xtickv,padding=0.1,/velocity $
      else begin
         xtickv=0
         xtickname=''
      endelse

       plot,x1-vframe[0],y1/yfactor,psym=10,$
            ytitle=ytitle[n+m],xtitle=xtitle[n+m],$
            charsize=charsize*0.75,charthick=charthick,thick=thick,$
            xrange=xrange,yrange=yrange,$
            ystyle=1,/noerase,yticks=yticks,$
            xtickname=axvalue(xtickname,m_total-1,100,blank),$
            ytickname=axvalue(ytickname,0,n,blank),$
            ytickv=ytickv,xtickv=xtickv,yminor=yminor,$
            ytickformat=axvalue(ytickformat,0,n,replicate('',10)),$
            color=fsc_color('blk8'),yticklen=yticklen,$
            xstyle=1,xminor=1,xticks=1,/nodata

            if keyword_set(polyfill) then begin
              for f=0,n_elements(polyfill[0,*])-1 do begin
                  if tagpolyfill[f,q] eq f+1 then begin
                      polyfill,[polyfill[0],polyfill[1],polyfill[1],polyfill[0]]-vframe[0],$
                        [!y.crange[0],!y.crange[0],!y.crange[1],!y.crange[1]],color=fsc_color('blk2') 
                      oplot,polyfill[0]*[1,1]-vframe[0],!y.crange,psym=10, color=fsc_color('black'),thick=thick*2./3.,linestyle=1
                      oplot,polyfill[1]*[1,1]-vframe[0],!y.crange,psym=10, color=fsc_color('black'),thick=thick*2./3.,linestyle=1
                  endif
              endfor
            endif 

       oplot,x1-vframe[0],y1/yfactor,psym=10,$
            color=color[0]

       if n_elements(bad_range1) ne 0 then begin
          tmp=where(bad_range1[*,0] eq q+1,count)
          if count ne 0 then begin
            x1_big=rebin(x1,n_elements(x1)*5)
            y1_big=frebin(y1,n_elements(y1)*5)
            bad_index=where((x1_big-vframe[0] ge min(bad_range1[tmp,1:2])) AND (x1_big-vframe[0] le max(bad_range1[tmp,1:2])),count)
              if count ne 0 then oplot,x1_big[bad_index]-vframe[0],y1_big[bad_index]/yfactor,psym=10,color=bad_color,thick=thick+2
          endif
       endif

        axis,xa=0,xminor=xminor,yminor=yminor,$
            xticks=xticks,$
            ytickv=ytickv,xtickv=xtickv,$
            xticklen=xticklen,$
            xtickformat=axvalue(xtickformat,m_total-1,m,replicate('',10)),$
            xtickname=axvalue(xtickname,m_total-1,m,blank),$
            xrange=xrange,xstyle=1,xthick=xtickthick,$
            charthick=charthick,charsize=charsize*0.75 
        axis,xa=1,xminor=xminor,yminor=yminor,$
            xticks=xticks,$
            ytickv=ytickv,xtickv=xtickv,$
            xticklen=xticklen,$
            xtickformat=axvalue(xtickformat,m_total-1,100,replicate('',10)),$
            xtickname=axvalue(xtickname,100,m,blank),$
            xrange=xrange,xstyle=1,xthick=xtickthick,$
            charthick=charthick,charsize=charsize*0.75  

            if keyword_set(fill) then begin
              for f=0,n_elements(fill[0,*])-1 do begin
                  if tagfill[f,q] eq f+1 then begin
                      good_fill=where((x1-vframe[0] ge fill[0,f]) AND (x1-vframe[0] le fill[1,f]),num_good)
                      if keyword_set(norm) then bottom=1. else bottom=0.
                      if num_good ne 0 then plotcolorfill,x1[good_fill]-vframe[0],y1[good_fill]/yfactor,/noerase,bottom=bottom,color=fsc_color('blk4') 
                             oplot,x1-vframe[0],y1/yfactor,psym=10, color=color[0],thick=thick
                  endif
              endfor
            endif 

            if keyword_set(arrows) then begin
               good_arrows=where(abs(arrows[0,*]) eq q+1,count_arrows)
               if count_arrows ne 0 then begin
                  for f=0, count_arrows-1 do begin
                    tmp=min(abs(x1-arrows[1,good_arrows[f]]),xloc)
                    if arrows[0,good_arrows[f]] gt 0 then begin
                       min_height=y1[xloc]/yfactor+(!y.crange[1]-!y.crange[0])*0.150
                       max_height=y1[xloc]/yfactor+(!y.crange[1]-!y.crange[0])*0.275
                       label_height=y1[xloc]/yfactor+(!y.crange[1]-!y.crange[0])*0.3
                       arrow,arrows[1,good_arrows[f]],max_height,arrows[1,good_arrows[f]],min_height,/data,thick=!p.thick,hsize=(!D.X_SIZE / 100.) 
                       xyouts,arrows[1,good_arrows[f]],label_height,strcompress(string(fix(good_arrows[f]+1)),/re),align=0.5,/data,$
                        charsize=charsize*0.75,charthick=charthick
                    endif else begin 
                       min_height=y1[xloc]/yfactor-(!y.crange[1]-!y.crange[0])*0.140
                       max_height=y1[xloc]/yfactor-(!y.crange[1]-!y.crange[0])*0.275
                       label_height=y1[xloc]/yfactor-(!y.crange[1]-!y.crange[0])*0.4
                       arrow,arrows[1,good_arrows[f]],max_height,arrows[1,good_arrows[f]],min_height,/data,thick=!p.thick,hsize=(!D.X_SIZE / 100.) 
                       xyouts,arrows[1,good_arrows[f]],label_height,strcompress(string(fix(good_arrows[f]+1)),/re),align=0.5,/data,$
                        charsize=charsize*0.75,charthick=charthick
                    endelse
                  endfor 
               endif
            endif

       if keyword_set(x2) then oplot,x2-vframe[0],y2/yfactor,psym=10,color=color[1]

       if n_elements(bad_range2) ne 0 then begin
          tmp=where(bad_range2[*,0] eq q+1,count)
          if count ne 0 then begin
                x2_big=interpol(x2,n_elements(x2)*5)
                y2_big=rebin(y2,n_elements(x2)*5) ;interpol(y2[bad_index],x2[bad_index],x2_big)
                pause
            bad_index=where((x2_big-vframe[0] ge min(bad_range2[tmp,1:2])) AND (x2_big-vframe[0] le max(bad_range2[tmp,1:2])),count)
              if count ne 0 then begin
                oplot,x2_big[bad_index]-vframe[0],y2_big[bad_index]/yfactor,psym=10,color=bad_color,thick=thick+2
              endif
          endif
       endif

       if keyword_set(x3) then oplot,x3-vframe[0],y3/yfactor,psym=10,color=color[2]

       if n_elements(bad_range3) ne 0 then begin
          tmp=where(bad_range3[*,0] eq q+1,count)
          if count ne 0 then begin
            x3_big=rebin(x3,n_elements(x3)*5)
            x3_big=x3_big-(x3_big[1]-x3_big[0])
            y3_big=frebin(y3,n_elements(y3)*5,/quadratic)
            bad_index=where((x3_big-vframe[0] ge min(bad_range3[tmp,1:2])) AND (x3_big-vframe[0] le max(bad_range3[tmp,1:2])),count)
              if count ne 0 then oplot,x3_big[bad_index]-vframe[0],y3_big[bad_index]/yfactor,psym=10,color=bad_color,thick=thick+2
          endif
       endif

       if keyword_set(x4) then oplot,x4-vframe[0],y4/yfactor,psym=10,color=color[3]

       if n_elements(bad_range4) ne 0 then begin
          tmp=where(bad_range4[*,0] eq q+1,count)
          if count ne 0 then begin
            x4_big=rebin(x4,n_elements(x4)*5)
            x4_big=x4_big-(x4_big[1]-x4_big[0])
            y4_big=frebin(y4,n_elements(y4)*5)
            bad_index=where((x4_big-vframe[0] ge min(bad_range4[tmp,1:2])) AND (x4_big-vframe[0] le max(bad_range4[tmp,1:2])),count)
              if count ne 0 then oplot,x4_big[bad_index]-vframe[0],y4_big[bad_index]/yfactor,psym=10,color=bad_color,thick=thick+2
          endif
       endif

       if (NOT keyword_set(yrange_user)) AND (n_total eq 2) then begin
            axis,!x.crange[1],!y.crange[0],/yax,$
                 ytitle=ytitle[n+m],xtitle=xtitle[n+m],$
                 xminor=xminor,yminor=yminor,$
                 yticks=yticks,xticks=xticks,$
                 ytickv=ytickv,xtickv=xtickv,$
                 charsize=charsize*0.75,$
                 charthick=charthick,$
                 xrange=xrange,yrange=yrange,$
                 xstyle=1,ystyle=1,/noerase,$
                 xtickname=axvalue(xtickname,m_total-1,m,blank),$
                 ytickname=axvalue(ytickname,1,n,blank),$
                 xtickformat=axvalue(xtickformat,m_total-1,m,replicate('',10)),$
                 ytickformat=axvalue(ytickformat,1,n,replicate('',10))
        endif else if (NOT keyword_set(yrange_user)) AND (n ne 0) then begin
            axis,!x.crange[0],!y.crange[0],yax=0,$
                 ytitle=ytitle[n+m],xtitle=xtitle[n+m],$
                 xminor=xminor,yminor=yminor,$
                 yticks=yticks,xticks=xticks,$
                 ytickv=ytickv,xtickv=xtickv,$
                 charsize=charsize*0.75,$
                 charthick=charthick,$
                 xrange=xrange,yrange=yrange,$
                 xstyle=1,ystyle=1,/noerase,$
                 xtickname=axvalue(xtickname,m_total-1,m,blank),$
                 ytickname=axvalue(ytickname,n,n,blank),$
                 xtickformat=axvalue(xtickformat,m_total-1,m,replicate('',10)),$
                 ytickformat=ytickformat ;axvalue(ytickformat,n_total-1,n,replicate('',10))
        endif

       if variable eq 'NORM' then oplot,!x.crange,[1,1],linestyle=1
       if !y.crange[0] ne 0 then oplot,!x.crange,[0,0],linestyle=1

       if n_elements(line) ne 0 then begin
          line=fltarr(n_elements(line))+line
          for r=0, n_elements(line)-1 do oplot,line[r]*[1.,1.],!y.crange,linestyle=2
       endif
       if n_elements(dotline) ne 0 then begin
          dotline=fltarr(n_elements(dotline))+dotline
          for r=0, n_elements(dotline)-1 do oplot,dotline[r]*[1.,1.],!y.crange,linestyle=1
       endif

       tmp=where(text ne '',num_text)
       if keyword_set(labels) or (num_text ne 0) then begin
            if (NOT keyword_set(label_factor)) then label_factor=0.8
            pos=[(!p.position[2]-!p.position[0])*6./8.0+!p.position[0],$
                 (!p.position[3]-!p.position[1])*6./8.0+!p.position[1]]   
           ;right
           if (position[q] eq 0) or (position[q] eq 1) then pos[0]=(!p.position[2]-!p.position[0])*7.0/8.0+!p.position[0]
           ;left
           if (position[q] eq 2) or (position[q] eq 3) then pos[0]=(!p.position[2]-!p.position[0])*1.25/8.0+!p.position[0]
           ;top
           if (position[q] eq 0) or (position[q] eq 3) then pos[1]=(!p.position[3]-!p.position[1])*6.5/8.0+!p.position[1] 
           ;bottom
           if (position[q] eq 1) or (position[q] eq 2) then pos[1]=(!p.position[3]-!p.position[1])*1.25/8.0+!p.position[1] 
  
           if (NOT strmatch(str1_arr[q],'*BLANK*',/fold_case)) then begin
             if tag_loc((SCOPE_VARFETCH(str1_arr[q], /ENTER,level=1)),'ion') eq 1 then begin
              if ((SCOPE_VARFETCH(str1_arr[q], /ENTER,level=1)).ion eq 'HI')  then $
                 label1=(SCOPE_VARFETCH(str1_arr[q], /ENTER,level=1)).ion 
            endif else $
                 label1=(SCOPE_VARFETCH(str1_arr[q], /ENTER,level=1)).ion+' '+lambda $
                      +strcompress(string((SCOPE_VARFETCH(str1_arr[q], /ENTER,level=1)).wavc,$
                      format='(I4)'),/re)
             
                 if keyword_set(labels) then $
                 xyouts,pos[0],pos[1],label1,$
                    align=0.5,/normal,charsize=charsize*label_factor,charthick=charthick,$
                    color=lcolor 
           
                if keyword_set(str2) then begin    
                  if (SCOPE_VARFETCH(str1_arr[q], /ENTER,level=1)).ion eq 'HI' then $
                    label2=(SCOPE_VARFETCH(str1_arr[q], /ENTER,level=1)).ion else if (NOT strmatch(str2_arr[q],'*BLANK*',/fold_case)) then $
                    label2=(SCOPE_VARFETCH(str2_arr[q], /ENTER,level=1)).ion+' '+lambda $
                      +strcompress(string((SCOPE_VARFETCH(str1_arr[q], /ENTER,level=1)).wavc,$
                      format='(I4)'),/re) else label2=''
      
      ;pause
                  if (label1 eq label2) AND keyword_set(labels) then xyouts,pos[0],pos[1],label1,align=0.5,/normal,$
                     charsize=charsize*label_factor,charthick=charthick,color=lcolor
                endif else if n_elements(label2) ne 0 then begin
                    xyouts,pos[0],pos[1]-0.01,label2,align=0.5,/normal,$
                     charsize=charsize*label_factor,charthick=charthick,color=lcolor
                     print,'got here'
                endif
             endif
         endif

               if keyword_set(text) then begin

                   if (NOT keyword_set(label_factor)) then label_factor=0.8
                   pos=[(!p.position[2]-!p.position[0])*6./8.0+!p.position[0],$
                        (!p.position[3]-!p.position[1])*6./8.0+!p.position[1]]   
                  ;right
                  if (position[q] eq 0) or (position[q] eq 1) then pos[0]=(!p.position[2]-!p.position[0])*7.0/8.0+!p.position[0]
                  ;left
                  if (position[q] eq 2) or (position[q] eq 3) then pos[0]=(!p.position[2]-!p.position[0])*1.25/8.0+!p.position[0]
                  ;top
                  if (position[q] eq 0) or (position[q] eq 3) then pos[1]=(!p.position[3]-!p.position[1])*6.5/8.0+!p.position[1] 
                  ;bottom
                  if (position[q] eq 1) or (position[q] eq 2) then pos[1]=(!p.position[3]-!p.position[1])*1.25/8.0+!p.position[1] 
                        ;top
                 if (position[q] eq 0) or (position[q] eq 3) then pos[1]=(!p.position[3]-!p.position[1])*5.5/8.0+!p.position[1] 
                 ;bottom
                 if (position[q] eq 1) or (position[q] eq 2) then pos[1]=(!p.position[3]-!p.position[1])*2.5/8.0+!p.position[1] 
                 xyouts,pos[0],pos[1],text[q],align=0.5,/normal,color=tcolor,charsize=charsize*label_factor,charthick=charthick
 
              endif

       q=q+1
    endfor
endfor

if n_elements(ytitle) eq n_elements(str1) then !p.position=[0.15,0.10,0.9,0.95] $
   else !p.position=[0.15,0.10,0.95,0.95]

if device eq 'X' then ypos = 0.025 else ypos = 0.10-m*0.0001
xyouts,0.55,ypos,grand_xtitle,/normal,align=0.5,charsize=charsize,charthick=charthick
if grand_ytitle ne ' ' then begin
;Add yfactor to the displayed ytitle
   yscalar=strcompress(string(fix(alog10(yfactor))),/re)  
   temp_ytitle=strsplit(grand_ytitle,'[',/extract,count=ygood)
   if (yscalar ne '0') AND (ygood eq 2) AND (NOT keyword_set(norm)) then $
      grand_ytitle=temp_ytitle[0]+'[10!U'+yscalar+'!N '+temp_ytitle[1] $
      else if (yfactor ne 1.) AND (ygood eq 1) AND (NOT keyword_set(norm)) $
         then grand_ytitle=temp_ytitle[0]+' x 10!U'+yscalar+'!N' $
      else grand_ytitle=temp_ytitle
endif
if device eq 'X' then xpos = 0.04 else xpos = 0.03+n*0.01
xyouts,xpos,0.55,grand_ytitle,/normal,orientation=90,align=0.5,charsize=charsize,charthick=charthick
xyouts,(!p.position[3]-!p.position[1])/2.0+!p.position[1],!p.position[3]+0.015,title,/normal,align=0.5,charsize=charsize,charthick=charthick


    !p.multi=0

if keyword_set(device) then begin
   if (device eq 'ps') or (device eq 'PS') then begin

    device,/close_file
    set_plot,entry_device
    Close, /All
    
    endif

    set_plot,'x'

endif    


cleanplot,/silent ;To reset and clean all idl plotting structures (!p, !x, !y)

end