pro sigmaReject, images, box_width = box_width, n_sigma = n_sigma, verbose = verbose

  ;; Adapted from SIGMA_FILTER from IDL Astro library
  ;; 
  ;; Instead of just replacing values, set up a bad pixel mask. "Cleaned" image is also
  ;; available for later possible use (e.g., reflection subtraction).
  
  if N_ELEMENTS(box_width) ne 1 then box_width = 5
  if N_ELEMENTS(n_sigma) ne 1 then n_sigma = 3.0    ; PRESIGREJ in original makeSpect
  bw2 = box_width^2.0
  
  ;; Mean of surrounding pixels, excluding the central one
  mean = (filter_image(images.orig, smooth = box_width, /all)*bw2 - images.orig) / (bw2 - 1)
  imdev = (images.orig - mean)^2.0
  fact = n_sigma^2.0 / (bw2 - 2)
  imvar = fact * (filter_image(imdev, smooth = box_width, /all)*bw2 - imdev)
  
  where_bad = where(imdev ge imvar, nbad)
  images.bad = images.bad * 0
  images.good = images.bad + 1
  images.clean = images.orig
  
  if nbad gt 0 then begin 
    images.bad[where_bad] = 1
    images.good[where_bad] = 0
    images.clean[where_bad] = mean[where_bad]
  endif
   
  if keyword_set(verbose) then $
    print, nbad, nbad * 100.0/n_elements(images.orig), n_sigma, $
      format = '(I4, " (", F5.1, " %) pixels rejected at ", F3.1, " sigma in image space")'
   
 end 
  
  