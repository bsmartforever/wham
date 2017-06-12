;function multi_gauss,x,y,err=err,ngauss=ngauss,quiet=quiet,$
;    height=height,center=center,width=width,fwhm=fwhm,$
;    fix_height=fix_height,fix_center=fix_center,fix_width=fix_width,$
;    tol_height=tol_height,tol_center=tol_center,tol_width=tol_width,$
;    fit_params=fit_params, ip=ip, wham=wham
;
; Purpose:
;   To simultaneously fit multiple Gaussians
;
; Input:
;   x - abscissa values for y 
;   y - spectrum data array
;   ngauss - numnber of Gaussians to fit
;          Default value is n_elements(center)
;
;   height - initial guess array for the Gaussian heights
;               Default is (max(y)-min(y))/4.
;   center - initial guess array for the Gaussian centers
;   width - initial guess array for the Gaussian width
;               Default is 30.0/2.3548 km/s 
;               (width = 1 standard deviation, not FWHM)
;               If ip is passed, then this will be set to 
;               the convolved Gaussian width.
;   
;   [Optional]:
;   [err] - error array for y. Required for Chi^2 calculations.
;
;   fix_height - FIX variable flag array for the Gaussian heights
;   fix_center - FIX variable flag array for the Gaussian centers
;   fix_width - FIX variable flag array for the Gaussian width
;
;   tol_height - TOLERANCE array about initial gauss values for the Gaussian heights
;   tol_center - TOLERANCE array about initial gauss values for the Gaussian centers
;   tol_width - TOLERANCE array about initial gauss values for the Gaussian width
;
;   ip - Instrument profile array, such as a broad Gaussian y-array for convolving 
;        with the Gaussian function used during the fit. If passed, the output 
;        yfit will be convolved with the ip array.
;   - To correctly calculate the width of the convolved Gaussian, pass 
;        the width of the instrument profile. This is determined automatically
;        for the wham_ip passed through the 'wham' keyword.
;   wham - wham instrument profile flag. If set, then the standard 3 Gaussian 
;        wham IP for H-alpha is used to broaden the Gaussina fit function to 
;        the WHAM velocity resolution (~12 km/s). Note that 3 Gaussians are 
;        used because the WHAM IP is asymmetric with one of the wings larger.            
;
; Output:
;
;   fit - Fit structure in form {xfit,yfit,chisq}
;           Note that program removes array elements where err=0
;           or where x,y,err are NAN values
;   [Optional]:
;   fit_params - Fit parameter structure containing:
;           area, center, width, height, and their corresponding errors.
;
; Example:
;
;   restore,'$HOME/WHAM/MS/reducing/pointed_summary/ha.sav'
;   x=ha[2].vel
;   y=ha[2].data
;   err=sqrt(ha[0].data_var)
;
;   ngauss=2
;     
;   ;initial guesses
;   height=[0.001,0.001]
;   center=[100,200]
;   width=[30,30]
;   
;   ;fix these variables. ALL VARIABLES CANNOT BE FIXED!
;   ;fix_height=[1,1]
;   fix_center=[1,1]
;   ;fix_width=[1,1]
;   
;   ;Tolerances to those variables 
;   ; *** Tolerance trumps the fixed variable keyword.
;   ;tol_height=[.0001,0.0001]
;   tol_center=[5,5]
;   ;tol_width=[10,10]
;   
;   fit=multi_gauss(x,y,err=err,$
;       height=height,center=center,width=width,$
;       fix_height=fix_height,fix_center=fix_center,fix_width=fix_width,$
;       tol_height=tol_height,tol_center=tol_center,tol_width=tol_width,$
;       fit_params=fit_params)
;   
;   ploterror,x,y,err
;   
;   oplot,fit.x,fit.y
;   oplot,!x.crange,[0,0],linestyle=1
;   
;   Dependencies:
;       arm_multgaussfit (http://bishop.astro.pomona.edu/penprase/OVIWEB/PRO/)
;       arm_multgauss (http://bishop.astro.pomona.edu/penprase/OVIWEB/PRO/)
;       cmtotal library (http://cow.physics.wisc.edu/~craigm/idl/fitting.html)
;
;   Restrictions:
;       -Assumes a flat and zero baseline
;
;   BUGS:
;       -The fix_h keyword currently doesn't work if an IP is passed with offset 
;         centers. This includes the whamip. To fix the height, you have to 
;         also fix the width as they are interdependent on each other. Not sure how 
;         to remove that dependency. Could be done by fixing the area instead of the 
;         height, but the fitting function only solves for c, w, h. I could modify 
;         this by using the Gaussian function instead of GAUS1, but that seems 
;         to obnoxious to worry about.
;
;   Notes: I got the idea to incorporate the arm_multgauss programs 
;          from Paul Sell's fit_gauss program.
;
; Created by Dr. Kat Barger 02/2014
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro constrain_params, ngauss,parinfo,params,$
    fix_params=fix_params,tol_params=tol_params,index,j

; Purpose:
; To go through each of the height, width, and center parameters and add 
; flags to parinfo that specify whether these variables are free, constrained, or fixed. 
;
; index=0 for height
; index=1 for center
; index=2 for width
;
;
    fix_params_loc=fltarr(ngauss)
    if (size(tol_params,/type) ne 2) AND (size(tol_params,/type) ne 4) then tol_params=fltarr(ngauss)-1
    if (size(fix_params,/type) ne 2) AND (size(fix_params,/type) ne 4) then fix_params=fltarr(ngauss)
    limited=[0,0]
    for j=0,ngauss-1 do begin
        fix_params_loc[j]=index+3*j
        if tol_params[j] eq -1 then tol=0 else tol=tol_params[j]
        parinfo[fix_params_loc[j]].limits=[params[j]-tol,params[j]+tol]
        if (tol eq 0) AND (fix_params[j] ne 1) then limited=[0,0] else limited=[1,1]
        parinfo[fix_params_loc[j]].limited = limited
        fix_params[j]=(tol_params[j] eq -1) OR (tol_params[j] eq 0) AND (fix_params[j] eq 1)
    endfor
    parinfo[fix_params_loc].fixed = fix_params

    return

end

FUNCTION WHAM_FUNC, X, P
  
  ;; NOTE that this function isn't actually used in multi_gauss.pro, but 
  ;; I'm keeping it here as more of a note to self on how to extract the 
  ;; WHAM IP then convolve it with a Gaussian so that it matches the 
  ;; velocity resolution of WHAM.

  ;; A Gaussian function convolved with WHAM IP
  ;; This well samples the ~12 km/s width
  ;; Have to convert width to sigma, height to area
  ip_name = '/d/wham/lib/whamspect2/ip/smorn_ip.dat'
  Get_Instr_Prof, ip_name, num_ip, mean_ip, width_ip, height_ip
  num=N_ELEMENTS(X)
  Kern = DBLARR(num)
  FOR i = 0, Num_Ip-1 DO BEGIN
    ;GAUSS1(XVALS, [MEAN, SIGMA, AREA], SKEW=skew)
     Kern = Kern + GAUSS1(DINDGEN(num)-num/2., [Mean_Ip[i], Width_Ip[i]/2.3548, $
                              Height_Ip[i]*Width_Ip[i]*SQRT(2*!pi)/2.3548])
  ENDFOR
  
  RETURN,  CONVOL(P[0]+GAUSS1(X, P[1:3]), Kern, /CENTER, /EDGE_TRUNCATE)
  
END

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro test_ip, ip, XVALS

; Purpose:
;   To ensure that the ip conforms with that needed for convol_params.pro.
;   This will make sure that ip is a structure containing the following tags:
;   ip, h, w, and c.
;     ip - an array of the same size as xvals containing the instrument profile.
;     h  - an array containing the height of the Gaussians used to create the IP.
;     w  - an array containing the width of the Gaussians used to create the IP.
;     c  - an array containing the center position of the Gaussians used to create the IP.
;
;  Input:
;    ip.w - The minumum required to create an instrument profile is passing the widths of 
;           the Gaussians. If only this tag is passed, all Gaussians are assumed to be 
;           aligned. 
;
;    ip.ip - If only the IP array is passed with no h, w, c tags passed, then the IP 
;           is assumed to be a single Gaussian and those parameters are measured. 
;           Note that this will be fairly accurate if the IP is near Gaussain in shape. 
;           This is because the major IP Gaussian will play the largest roll in defining 
;           the convolved width and the tallest Gaussian will control the height. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


if (where(strcmp(tag_names(ip),'w',/fold_case)) eq -1) AND $
   (n_elements(ip) ne 1) then begin

    print,''
    print,'** Widths, areas, centers were not passed in the ip.'
    print,'** Assuming the ip is composed of a single Gaussian.'
    print,''
    print,'** Pass w, h, c tags in ip to correct this.'
    print,''

endif else if (where(strcmp(tag_names(ip),'w',/fold_case)) ne -1) AND $
   (where(strcmp(tag_names(ip),'h',/fold_case)) eq -1) then begin
   if n_elements(ip) eq 1 then add_tag,ip,'h',1./(ip.w*sqrt(2.*!pi)),ip
endif 

        ;Check to see if ip is an array, not a struture
        if (size(ip,/type) eq 2) then $
            if (n_elements(ip.(tag_loc)) eq n_elements(XVALS)) then begin
              xip=ip.(tag_loc)
              yip=ip  
            endif 

        ;If ip is a structure, check to see if it contains the 'x' tag 
        tag_loc=where(strcmp(tag_names(ip),'x',/fold_case) eq 1)
        if tag_loc ne -1 then begin
          if (n_elements(ip.(tag_loc)) eq n_elements(XVALS)) AND $
             (where(strcmp(tag_names(ip),'ip',/fold_case) eq 1)) then begin
            xip=ip.(tag_loc)  
            yip=ip.ip
          endif
        ;If ip is a structure and doesn't contain a 'x' tag, then set xip=XVALS 
        ;if the number of elements in ip.ip is equal to XVALS  
        endif else if (where(strcmp(tag_names(ip),'ip',/fold_case)) ne -1) then begin
          if (n_elements(ip.ip) eq n_elements(XVALS)) then begin
              xip=xvals-avg(xvals)
              yip=ip.ip
          endif
        endif

        ;If the widths and heights are passed in the ip structure, but not the ip.ip tag, 
        ;then construct the ip.ip tag. Note that if the centers aren't passed in ip.c, 
        ;then they are assumed to be zero. 
        if (where(strcmp(tag_names(ip),'h',/fold_case)) ne -1) AND $
           (where(strcmp(tag_names(ip),'w',/fold_case)) ne -1) AND $
           (NOT keyword_set(yip)) then begin
           if (where(strcmp(tag_names(ip),'c',/fold_case)) ne -1) then $
              c=fltarr(n_elements(ip))+ip.(where(strcmp(tag_names(ip),'c',/fold_case))) $
           else begin 
              c=fltarr(n_elements(ip))
              print,''
              print,'** Centers were not passed in ip.c, assuming they are at 0 km/s'
              print,''
           endelse
           if (NOT keyword_set(xip)) then xip=float(xvals-avg(xvals))
           a=fltarr(n_elements(ip))
           for i=0, n_elements(ip)-1 do a[i]=gauss_area([ip[i].h,0,ip[i].w])
           a_total=total(a)
           ;Normalize all areas such that the total area equals 1
           ;and modify the heights to conform to this change
           a=a/a_total
           ;Make sure that the 'h' and 'w' tags are floats, not an integer data types.
           ip = alter_tags(ip, {h:0.0})
           ip = alter_tags(ip, {w:0.0})
           ip.h=a/(ip.w*sqrt(2.*!pi)) 

              ;Choose 100. unless 100. gt n_elements(x)
              ;The kernal must have n_elements(x) or smaller
              num=(100. gt n_elements(xip)) ? n_elements(xip) : 100.
              yip = DBLARR(num)
              ;The spacing makes sure that the Gaussian has the same sampling spacing as ipx
              spacing=(max(xip)-min(xip))/n_elements(xip)
              FOR i = 0, n_elements(ip)-1 DO BEGIN
                 yip = yip + GAUSS1(DINDGEN(num+1)-num/2., [0.0, (ip[i].w)/spacing, $
                                          ip[i].h*ip[i].w*SQRT(2*!pi)])
              ENDFOR

           tmp=replicate({ip:yip,h:ip.h,w:ip.w,c:c[0]},n_elements(ip))
           tmp.ip=yip
           tmp.h=ip.h
           tmp.w=ip.w
           tmp.c=c
           ip=tmp
        endif

        if (n_elements(xip)) eq (n_elements(yip)) AND $
           ((where(strcmp(tag_names(ip),'h',/fold_case)) eq -1) OR $
           (where(strcmp(tag_names(ip),'w',/fold_case)) eq -1) OR $
           (where(strcmp(tag_names(ip),'c',/fold_case)) eq -1)) then begin
            ip_fit=gaussfit(xip,yip,a)
            ip={ip:yip,h:a[0],w:a[2],c:0.0}
        endif 

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function multi_gauss,x,y,err=err,ngauss=ngauss,quiet=quiet,$
    height=height,center=center,width=width,$
    fix_height=fix_height,fix_center=fix_center,fix_width=fix_width,$
    tol_height=tol_height,tol_center=tol_center,tol_width=tol_width,$
    fit_params=fit_params,chisq=chisq,ip=ip,wham=wham,$
    unconvol_params=unconvol_params

;remove error values that are zero; this will mess up the fitting
if total(err) ne 0 then good=where(err ne 0) else good=indgen(n_elements(x))
XVALS=x[good]
YVALS=y[good]
if total(err) ne 0 then begin
    ERRVALS=err[good]

    ;remove all infinity values
    good=where(FINITE(ERRVALS, /NAN) eq 0)
    XVALS=XVALS[good]
    YVALS=YVALS[good]
    ERRVALS=err[good]
endif
good=where((FINITE((FINITE(XVALS, /NAN) eq 0)) OR (FINITE(YVALS, /NAN) eq 0)))
XVALS=XVALS[good]
YVALS=YVALS[good]

if (NOT keyword_set(center)) then center=(findgen(ngauss)+1.)*(max(x)-min(x))/(ngauss+1.)+min(x)
if (NOT keyword_set(ngauss)) then ngauss=n_elements(center)
if (NOT keyword_set(height)) then height=fltarr(ngauss)+(max(y)-min(y))/4.

if keyword_set(wham) then begin
  ip=wham_ip(XVALS) 
endif else if keyword_set(ip) AND (NOT keyword_set(wham)) then test_ip,ip,XVALS


;; Make sure that the fix parameter values are set properly if an IP is passed

;if (NOT keyword_set(width)) then width=fltarr(ngauss)+30./2.3548 else width=width/2.3548
if (NOT keyword_set(width)) then width=fltarr(ngauss)+30./2.3548 $
else if (keyword_set(width)) AND (keyword_set(ip)) then begin
  ;If width is initialized, set this value to the 'convolved' value of the width.
  cwidth=width
  width=sqrt(abs((cwidth)^2.-max(ip.w^2.))) 
  bad_loc=where(sqrt(total(ip.w^2.)) gt cwidth,count) 
  if count ne 0 then begin
      cwidth=sqrt(total(ip.w)^2.)+1.
      width[bad_loc]=sqrt(abs((cwidth)^2.-max(ip.w)^2.)) 
      print,''
      print,'** Initialized widths that are smaller than the IP width are unresolvable'
      print,'** Setting the width to ',sqrt(max(w.ip)^2.)+1.,format='(A-25,f-8.2)'
      print,''
  endif
endif
if (keyword_set(fix_center)) AND (keyword_set(ip)) then center=center-ip.c[where(ip.h eq max(ip.h))]
if (keyword_set(fix_height)) AND (keyword_set(fix_width)) AND (keyword_set(ip)) then begin
  height_arr=n_elements(ip)
  for i=0, n_elements(ip)-1 do height_arr[i]=height*sqrt(width^2.+max(ip.w)^2.)/((ip.h[i])*width*max(ip.w)*sqrt(2.*!pi))
  height=total(height)
endif else if (keyword_set(fix_height)) AND (NOT keyword_set(fix_width)) AND (keyword_set(ip)) then begin
  print,''
  print,'** Cannot fix the height if the width is not passed WHEN an IP is used.'
  print,'** This is because the height value depends on the width value.'
  print,''
endif

p=transpose([[height],[center],[width]])

;for j=0,ngauss-1 do begin
	;fix variables
    ;ngauss * 3 parameter per guass
    num_p = ngauss*3.0
    parinfo = replicate({value:0.D, fixed:0, limited:[0,0], $
                       limits:[0.D,0]}, num_p) 
 
 	p_tmp=p[0:2,*]
    parinfo[*].value = reform(p_tmp,ngauss*3) ;starting values

    ;This goes through each of the height, width, and center parameters and adds 
    ;flags to parinfo that specify whether these variables are free, constrained, or fixed. 
    constrain_params, ngauss,parinfo,height,fix_params=fix_height,tol_params=tol_height,0,j
    constrain_params, ngauss,parinfo,width,fix_params=fix_width,tol_params=tol_width,2,j
    constrain_params, ngauss,parinfo,center,fix_params=fix_center,tol_params=tol_center,1,j

p_out=arm_multgaussfit(XVALS,YVALS,p,err=ERRVALS,ngauss=ngauss,$
	  chisq=chisq,dof=dof,parinfo=parinfo,perror=p_err,yfit=yvals_fit,ip=ip,quiet=quiet)

;Confidence in parameter values:

;T_CVF((1-.99)/2, DoF) => This places the confidence level at 99% instead of 68%.
;This is like saying that the value lies within ~2.5 standard deviation instead of 1 standard deviation.
;This is basically like the acceptability of a measured answer. 
;See pg 149 of Error Analysis by Taylor

; From mpfit's header:
; *If* you can assume that the true reduced chi-squared
; value is unity -- meaning that the fit is implicitly
; assumed to be of good quality -- then the estimated
; parameter uncertainties can be computed by scaling PERROR
; by the measured chi-squared value.

;PCERROR = PERROR * SQRT(BESTNORM / DOF)   ; scaled uncertainties

if keyword_set(ERRVALS) then p_err=p_err*sqrt(chisq) ;*T_CVF((1-.99)/2, DoF)

fit_params=replicate({area:0.0,area_err:0.0,$
					  center:0.0,center_err:0.0,$
					  width:0.0,width_err:0.0,$
					  height:0.0,height_err:0.0},ngauss)

fit={x:XVALS,y:yvals_fit,chisq:chisq,dof:dof}

for i=0, ngauss-1 do begin
  fit_params[i].area=(gauss_area(p_out[(3*i):(3*(i+1))-1],err=p_err[(3*i):(3*(i+1))-1]))[0]
  fit_params[i].area_err=(gauss_area(p_out[(3*i):(3*(i+1))-1],err=p_err[(3*i):(3*(i+1))-1]))[1]
	fit_params[i].center=p_out[i*3.+1]
	fit_params[i].center_err=p_err[i*3.+1]
	fit_params[i].width=abs(p_out[i*3+2])
	fit_params[i].width_err=p_err[i*3+2]
	fit_params[i].height=p_out[i*3.]
	fit_params[i].height_err=p_err[i*3.]
endfor

unconvol_params=fit_params
if keyword_set(ip) then convol_params,fit_params,ip=ip,quiet=quiet

if (NOT keyword_set(quiet)) then begin
    print,''
    print,'** Reduced Chi-Squared: ',chisq,format='(A-25,f-8.2)'
    for i=0,ngauss-1 do begin
        print,''
        print,'** GAUSS 1 **','FIXED:',format='(A-50,A-25)'
        if (max(abs(fit_params[i].Area)) gt 1e5) or (min(abs(fit_params[i].Area)) lt 1e-3) then $
        print,'** Area +/- error:',fit_params[i].Area,fit_params[i].Area_err,format='(A-25,2e-25.2)' else $
        print,'** Area +/- error:',fit_params[i].Area,fit_params[i].Area_err,format='(A-25,2f-10.2)' 
        print,'** Center +/- error:',fit_params[i].Center,fit_params[i].Center_err,fix_center[i],format='(A-25,2f-10.2,I8)'
        print,'** Width +/- error:',fit_params[i].Width,fit_params[i].Width_err,fix_width[i],format='(A-25,2f-10.2,I8)'
        if (max(abs(fit_params[i].Height)) gt 1e5) or (min(abs(fit_params[i].Height)) lt 1e-2) then $
        print,'** Height +/- error:',fit_params[i].Height,fit_params[i].Height_err,fix_height[i],format='(A-25,2e-10.2,I8)' else $
        print,'** Height +/- error:',fit_params[i].Height,fit_params[i].Height_err,fix_height[i],format='(A-25,2f-10.2,I8)'
    endfor
    print,''
endif

undefine,fix_width,fix_height,fix_center,tol_width,tol_height,tol_center

return,fit

end 