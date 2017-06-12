pro reflectSubtract, images, params = params, verbose = verbose

  ;; Subtract the reflected ring from fabry-perot images by subtracting
  ;; a fraction of a spatially offset version of itself from itself.

  imDim = size(images.clean, /dim)
  nx = imDim[0]
  ny = imDim[1]

  ;; Use ROT to translate the image. Probably a little more straightforward to use
  ;; POLYWARP and POLY_2D since we're not really rotating--consider rewriting!
  
  ;; We're using the cleaned image from the cosmic ray rejection routine to avoid 
  ;; large negative pixels after the subtraction.
  images.reflect = rot(images.clean, 0, 1.0, (nx/2.0) - params.xoff, (ny/2.0) - params.yoff, $
                        missing = 0.0, cubic = -0.5)
  
  ;; TODO: We should probably exclude the region outside the aperature (rmax?) in the reflection, 
  ;;       although the error is likely small since values should be close to zero.
  
  images.reflectSub = images.clean - (images.reflect * params.e)
end