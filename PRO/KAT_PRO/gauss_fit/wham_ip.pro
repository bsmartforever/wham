FUNCTION WHAM_ip, X 

; Purpose:
;   To extract the WHAM instrument profile for convolving 
;   with the Gaussian function in the fitting routine.
;
; Input:
;   X - An array containing the x-values over the region to be fit. 
;
; Output:
;   ip - Structure containing the following tags:
;        ip (ip kernal), h (height array), w (width array), and
;        c (center array) 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Have to convert width (FWHM) to sigma, height to area

  ; For some reason, many of the WHAM IPs will result in very 
  ; uncertain values for the width and area. The standard_ip.dat
  ; seems to work fine though.
  ; ip_name = '/d/wham/lib/whamspect2/ip/3gauss_ip.dat'
  ip_name = '/d/wham/lib/whamspect2/ip/standard_ip.dat'
  Get_Instr_Prof, ip_name, num_ip, center_ip, width_ip, height_ip
  ; ip_name = '/d/wham/lib/whamspect2/ip/smorn_ip.dat'
  ; Choose 100. unless 100. gt n_elements(x)
  ; The kernal must have n_elements(x) or smaller
  num=(100. gt n_elements(x)) ? n_elements(x) : 100.
  Kern = DBLARR(num)
  ; The spacing makes sure that the Gaussian has the same sampling spacing as X
  spacing=(max(x)-min(x))/n_elements(x)
  FOR i = 0, Num_Ip-1 DO BEGIN
     Kern = Kern + GAUSS1(DINDGEN(num+1)-num/2., [Center_Ip[i], (Width_Ip[i]/2.3548)/spacing, $
                              Height_Ip[i]*Width_Ip[i]*SQRT(2*!pi)/2.3548])
  ENDFOR
  ip={ip:Kern,h:Height_Ip,w:Width_Ip/2.3548,c:Center_ip}

  RETURN, ip

END