; +
; NAME: cimage__define 
;
; PURPOSE: 
;
; CALLING SEQUENCE: 
;                   
; INPUTS:  
;
; OPTIONAL INPUTS:                     
;
; OPTIONAL KEYWORD INPUTS:
;
; OUTPUTS: 
;
; OPTIONAL OUTPUTS;
;
; EXAMPLE:
;
; NOTES: 
; 
; PROCEDURES USED:
;
; REVISION HISTORY: 18DEC2002 - MWM: added comments.
; - 

function CImage::Init, filename=filename, data=data, header=header, xs=xs, $
	ys=ys, zs=zs, n_ext=n_ext, ext=ext

; set filename
if keyword_set(filename) then file=filename else file=''
path_filename=file
file=ql_getfilename(file)

; get data and image size
if keyword_set(data) then begin
	d=data
	s=size(*d)
	xsize=s[1]
	ysize=s[2]
        case s[0] of
            3: zsize=s[3]
            2: zsize=1
            else: zsize=1 
        endcase

        max=max(*d, min=min)

        ; set number of total extensions in the FITS data
        if keyword_set(n_ext) then n_ext=n_ext else n_ext=0

        ; set extension number of this instance
        if keyword_set(ext) then ext=ext else ext=0

; if not defined, create default image
endif else begin
	d=ptr_new(/allocate_heap)
	xsize=1
	ysize=1
	zsize=1
	max=0.0
	min=0.0
        n_ext=0
        ext=0
endelse

; get or create FITS header
if keyword_set(header) then h=header else h=ptr_new(/allocate_heap)

; override image size if set
if keyword_set(xs) then x=xs else x=xsize
if keyword_set(ys) then y=ys else y=ysize
if keyword_set(zs) then z=zs else z=zsize

; set values of members of object
self.filename=file
self.path_filename=path_filename
self.data=d
self.OrigData=d
self.header=h
self.xs=x
self.ys=y
self.zs=z
self.MinVal=min
self.MaxVal=max
self.n_ext=n_ext
self.ext=ext

return, 1

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  BEGIN CIMAGE ACCESSOR/MUTATOR FUNCTIONS
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro CImage::SetFilename, filename, file
; set the path + filename and the filename member variables
self.path_filename=filename

if keyword_set(file) then self.filename=filename else $
self.filename=ql_getfilename(filename)

end

function CImage::GetFilename
return, self.filename

end

function CImage::GetPathFilename
return, self.path_filename

end

pro CImage::SetData, data_ptr

self.data=data_ptr

end

function CImage::GetData

return, self.data

end

pro CImage::SetOrigData, data_ptr

self.OrigData=data_ptr

end

function CImage::GetOrigData

return, self.OrigData

end

pro CImage::SetHeader, header_ptr

self.header=header_ptr

end

function CImage::GetHeader

return, self.header

end

pro CImage::SetXS, xs

self.xs=xs

end

function CImage::GetXS

return, self.xs

end

pro CImage::SetYS, ys

self.ys=ys

end

function CImage::GetYS

return, self.ys

end

pro CImage::SetZS, zs

self.zs=zs

end

function CImage::GetZS

return, self.zs

end

pro CImage::SetMinVal, MinVal

self.MinVal=MinVal

end

function CImage::GetMinVal

return, self.MinVal

end

pro CImage::SetMaxVal, MaxVal

self.MaxVal=MaxVal

end

function CImage::GetMaxVal

return, self.MaxVal

end

function CImage::GetN_Ext
	return, self.n_ext
end

pro CImage::SetN_Ext, newN_Ext
	self.N_Ext=newN_Ext
end

function CImage::GetExt
	return, self.ext
end

pro CImage::SetExt, newExt
	self.ext=newExt
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  END CIMAGE ACCESSOR/MUTATOR FUNCTIONS
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro CImage__define

; data, header and OrigData point at undefined heap variable
data=ptr_new(/allocate_heap)
header=ptr_new(/allocate_heap)
OrigData=ptr_new(/allocate_heap)

; create a structure that holds an instance's information 
struct={CImage, $
        filename:'', $        ; image filename
        path_filename:'', $   ; image path and filename
        data:data, $          ; image data 
        header:header, $      ; header of the fits file
        xs:0L, $              ; x size of the image data
        ys:0L, $              ; y size of the image data
        zs:0L, $              ; z size of the image data
	OrigData:OrigData, $  ; original image data
        MinVal:0.0, $         ; minimum image value
        MaxVal:0.0, $         ; maximum image value
        n_ext:0, $            ; number of extensions in this FITS
        ext:0 $               ; extension of this image
       }

end

