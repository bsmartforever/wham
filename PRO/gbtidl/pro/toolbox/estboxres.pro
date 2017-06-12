;+
; Estimate the new frequency_resolution after boxcar smoothing
; something with a known old frequency_resolution.
;
; <p>This estimates the new frequency_resolution by assuming that the
; frequeny_resolution is the FWHM of a gaussian response function and
; that the ratio of the widths is equal to the inverse ratio of
; the heights of the response functions before and after the
; smoothing.  Since a boxcar function isn't a Gaussian, this
; will be inaccurate to some degree.  The initial response function
; may also not be Gaussian.  Tests against a full convolution as well
; as tests involving noise from real GBT data indicate that this
; estimate is accurate to within a few percent.
;
; <p>The boxcar procedure uses this function to adjust the
; frequency_resolution of the data container.
;
; @param width {in}{required}{type=integer} The width of the boxcar.
; @param oldres {in}{required}{type=float} The frequency_resolution
; in channels before boxcar smoothing.
;
; @returns the frequency resolution in channels after boxcar
; smoothing to the given width.
;
; @examples
; <pre>
;   ; what would the new resolution be of the PDC after boxcar
;   ; smoothing with a width of 5 channels.
;   print,
;
;   esthanres(5,!g.s[0].frequency_resolution/abs(!g.s[0].frequency_interval))
; </pre>
;
; @uses <a href="gaussrespfn.html">gaussrespfn</a>
;
; @version $Id: estboxres.pro,v 1.1 2005/11/16 19:19:16 bgarwood Exp $
;-
function estboxres, width, oldres
    compile_opt idl2

    ibox = round(width)
    x = findgen(ibox) - (ibox-1)/2.0
    newht = total(gaussrespfn(x,oldres)) / ibox
    return, oldres/newht

end