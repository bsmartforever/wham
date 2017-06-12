function imdrizzle,input,output,xshift,yshift,trim=trim,stp=stp

;+
;
; IMDRIZZLE
;
; This can shift two images by fractional pixels.
; This preserves flux, but not rotations allowed.
;
; The outer edge of the image is always trimmed
; If /trim is set then the two images are trimmed
; to their common area.
;
; The xshift/yshift should be such that they will
; shift the image "im" to the reference image.
;
; INPUTS:
;  input     The list of input images.  This can be an array of
;             FITS filenames, a list file, or an array of images.
;             If an image is input it should be [Nimages,Nx,Ny].
;  output    The list of output images.  This can be a list of
;              filenames, or a string suffix.  If this is undefined
;              a 3D array of images is output.
;  xshift    The shift in the X direction.  This sign should be such
;             that shifting by this will align "im" with "refim".
;             If a list of images is input then this can be an array
;             or a single value.
;  yshift    The shift in the Y direction.  Sign convention and array
;             size is the same as for xshift.
;  /trim     Trim all images to the common overlapping area.
;  /stp      Stop at the end of the program.
;
; OUTPUTS:
;  The final shifted images are output.  The output type depends on
;  "output".
;
; USAGE:
;  IDL>outim = imdrizzle(image,xshift,yshift,trim=trim)
;
; By D.Nidever  Nov. 2007
;-


; Not enough inputs
if n_params() lt 4 then begin
  print,'Syntax - output = imdrizzle(input,xshift,yshift,trim=trim)'
  return,-1
endif

; Getting the input
intype = size(input,/type)

; String list input
if intype eq 7 then begin

  ; Load the list
  LOADINPUT,input,inlist
  ninlist = n_elements(inlist)

  ; Load the FITS images
  head = headfits(inlist[0])
  nx = sxpar(head,'NAXIS1')
  ny = sxpar(head,'NAXIS1')
  imarr = fltarr(ninlist,nx,ny)-999999.   ; bad for now
  for i=0,ninlist-1 do begin
    test = file_test(inlist[i])
    if test eq 1 then begin
      FITS_READ,inlist[i],im,head
      nx2 = n_elements(im[*,0])
      ny2 = n_elements(im[0,*])
      if nx2 ne nx or ny2 ne ny then begin
        print,'IMAGE SIZE NOT CORRECT'
        goto,FILEBOMB
      endif
      imarr[i,*,*] = im
    endif else begin
      print,'FILE ',inlist[i],' DOES NOT EXIST'
    endelse

    FILEBOMB:
  endfor

  nimages = ninlist

endif
; One image input
if size(input,/n_dim) eq 2 then begin
  sz = size(input)
  nx = sz[1]
  ny = sz[2]
  imarr = fltarr(1,nx,ny)
  imarr[0,*,*] = input
  nimages = 1
endif
; Image array input
if size(input,/n_dim) eq 3 then begin
  imarr = input
  nimages = n_elements(imarr[*,0,0])
end
; Array too large
if size(input,/n_dim) gt 3 then begin
  print,'ONLY 3D INPUT IMAGE ARRAYS ALLOWED'
  return,-1
endif


; Starting the output array
outimarr = imarr*0.


; Making xshift/yshift arrays
; If not the right size then just use the first shift
if n_elements(xshift) eq nimages then xshiftarr = xshift else $
   xshiftarr = fltarr(nimages)+xshift[0]
if n_elements(yshift) eq nimages then yshiftarr = yshift else $
   yshiftarr = fltarr(nimages)+yshift[0]



; Loop through the images
FOR i=0L,nimages-1 do begin

  undefine,im,xshft,yshft,intim,fracim

  ; Getting the image and shifts
  im = reform(imarr[i,*,*])
  xshft = xshiftarr[i]
  yshft = yshiftarr[i]

  ; Doing the integer shifts
  ; Always want the fractional shifts to be positive
  intxshift = floor(xshft)
  intyshift = floor(yshft)

  intim = shift(im,intxshift,intyshift)    ; for now allow wrap around

  ; Doing the fractional shifts
  ; Just take the fractional flux from neighboring pixels and combine them
  fracxshift = xshft-intxshift
  fracyshift = yshft-intyshift

  ; X shift
  ; Shift to the right 1
  fracim = im*0.
  fracim = shift(intim,1)*fracxshift + intim*(1.0-fracxshift)

  ; Y shift
  ; Shift up 1
  fracim = shift(fracim,0,1)*fracyshift + fracim*(1.0-fracyshift)

  ; Putting in output array
  outimarr[i,*,*] = fracim

END


; Trimming
if keyword_set(trim) then begin

  sz = size(imarr)
  nx = sz[2]
  ny = sz[3]

  ; All good for now
  xlo = 0
  ylo = 0
  xhi = nx-1
  yhi = ny-1

  ; Cutting off columns on the left
  ;  from positive xshifts. round up
  gleftxcut = where(xshiftarr gt 0.0,ngleftxcut)
  if ngleftxcut gt 0 then xlo = max(ceil(xshiftarr[gleftxcut]))

  ; Cutting off columns on the right
  ;  from negative xshifts. round up
  grightxcut = where(xshiftarr lt 0.0,ngrightxcut)
  if ngrightxcut gt 0 then xhi = nx-1-max(ceil(abs(xshiftarr[grightxcut])))

  ; Cutting off rows on the bottom
  ;  from positive yshifts. round up
  gbotycut = where(yshiftarr gt 0.0,ngbotycut)
  if ngbotycut gt 0 then ylo = max(ceil(yshiftarr[gbotycut]))

  ; Cutting off rows on the top
  ;  from negative yshifts. round up
  gtopycut = where(yshiftarr lt 0.0,ngtopycut)
  if ngtopycut gt 0 then yhi = ny-1-max(ceil(abs(yshiftarr[gtopycut])))

  ; Make sure there are pixels left
  if (xhi le xlo) or (yhi le ylo) then begin
    print,'NO COMMON AREA'
    return,-1
  endif

  ; Trimming
  outimarr_orig = outimarr
  outimarr = outimarr[*,xlo:xhi,ylo:yhi]

end


; Getting the output
noutput = n_elements(output)
outtype = size(output,/type)
; A preference for output given
if noutput gt 0 and outtype eq 7 then begin

  ; Suffix input
  if noutput eq 1 and strmid(output[0],0,1) ne '@' then begin
    ; We have an input list
    if n_elements(inlist) gt 0 then begin
      outfiles = file_basename(inlist,'.fits')+'.'+output+'.fits'

    ; No input list
    endif else begin
      print,'NO INPUT LIST. RETURNING IMAGE ARRAY'
      goto,NORMAL_OUT
    endelse
  endif

  ; A real list
  noutfiles = n_elements(outfiles)
  if noutfiles eq 0 and outtype eq 7 then $
    LOADINPUT,output,outfiles
  noutfiles = n_elements(outfiles)

  ; Enough output filenames
  if noutfiles ge nimages then begin
    for i=0,nimages-1 do begin
      outim = reform(outimarr[i,*,*])
      FITS_WRITE,outfiles[i],outim
    end

  ; NOT enough output filenames
  endif else begin
    print,'NOT ENOUGH OUTPUT FILESNAMES.  RETURNING IMAGE ARRAY'
    goto,NORMAL_OUT
  endelse


; Just output the image array
endif else begin
  NORMAL_OUT:
  output = outimarr
  output = reform(output)    ; if only one image input
endelse

return,output

if keyword_set(stp) then stop

end
