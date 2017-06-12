;+
; :Description:
;   Given a date (old-style FITS or IAU), return the best-guess bias value to use
;-
function estimate_bias, date, humid = humid
  compile_opt idl2
  
  ;; parse date and compute WHAM day number
  day0 = parsedate('970101')
  day = parsedate(date)
  wday = day - day0
  
  bias = 0
  
  if day lt parsedate('090301') then begin 
    ;; KPNO static value
    ;; TODO: incorporate Kat's analysis!

    message, 'NOTE: using KPNO grand average--no model curves yet!', /info
    bias = 605.66

  endif else if (parsedate('090301') le day && day le parsedate('091231')) then begin

    ;; 2009 "settling" seasonal fit; humidity isn't strongest factor here, go off date only
    
    x = wday mod 365
    a = [609.52943, 1.8377230, 16.467953]
    bias = a[0] + a[1] * sin((x + a[2])/365.0 * 2*!dpi)

  endif else if (parsedate('100101') le day && day le parsedate('130401')) then begin

    if isa(humid) then begin

      ;; best estimate is made if we are given a humidity measurement
      ;; this model was fit using HUMID3 (wall sensor)

      a = [606.91620, 0.042393224]
      bias = a[0] + a[1] * humid
      
    endif else begin

      ;; otherwise use seasonal average curve: 1/1/2010 to 4/1/2013 (pre-controller failure)

      x = wday mod 365
      a = [607.82590, 0.82613025, 30.439691]
      bias = a[0] + a[1] * sin((x + a[2])/365.0 * 2*!dpi)

    endelse
  
  endif else begin
    message, 'NOTE: no bias info for date ' + date + ' (WHAM day ' + strtrim(fix(wday), 2) + '); using CTIO rough average.', /info
    bias = 608.00 
  endelse
  
  return, bias
end

  