pro dispimages, images, scale = scale

  if n_elements(scale) eq 0 then scale = 4 

  sz = size(images.orig, /dim)
  
  if !d.name eq 'X' then begin
    window, 10, xsize=sz[0]*scale * 2, ysize=sz[1]*scale * 2
    
    ;; use cleaned image to scale the others
    cl_min = min(images.clean, max = cl_max)
    
    loadct, 3
    tv, bytscl(rebin(images.biassub, sz[0]*scale, sz[1]*scale, /sample), min = cl_min, max = cl_max), 0
    tv, bytscl(rebin(images.clean, sz[0]*scale, sz[1]*scale, /sample), min = cl_min, max = cl_max), 1
    tv, bytscl(rebin(images.reflectsub, sz[0]*scale, sz[1]*scale, /sample), min = cl_min, max = cl_max), 2
    
    @mycmap
    tv, rebin(images.bad, sz[0]*scale, sz[1]*scale, 2, /sample)*127, 3
    
  endif
  
end