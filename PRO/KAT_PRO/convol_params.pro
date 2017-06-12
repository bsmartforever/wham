pro convol_params, params, ip=ip, quiet=quiet

; Purpose:
; 	To calculate the modified params values from the convolution operation. 
;	Note that the fit gives close values, but slightly over estimates the 
;	height and slightly under estimates the width.
;
; Input: 
;	params - Structure containing the area, center, width, and height of the 
;		fit parameters along with their corresponding uncertainties.
;		Can contain the results of multiple Gaussians.
;
;		Example:
;		** Structure <3411be8>, 8 tags, length=32, data length=32, refs=1:
;		   AREA            FLOAT          0.162456
;		   AREA_ERR        FLOAT         0.0219968
;		   CENTER          FLOAT           156.969
;		   CENTER_ERR      FLOAT           2.25353
;		   WIDTH           FLOAT           22.0328
;		   WIDTH_ERR       FLOAT           2.46893
;		   HEIGHT          FLOAT        0.00294155
;		   HEIGHT_ERR      FLOAT       0.000223572
;		
;	ip - Structure containing the height, width, and center positions
;		 of the instrument profile.
;
;		 Example:
;		 ** Structure <3411088>, 4 tags, length=360, data length=360, refs=1:
;		    IP              DOUBLE    Array[33]
;		    H               DOUBLE    Array[4]
;		    W               DOUBLE    Array[4]
;		    C               DOUBLE    Array[4]
;		 **Note that the 'ip' tag is not used in this calculation and is not required.
;		 
; Bugs: 
;	 	- The error of the area seems to be a bit too small. For now, 
;       use which ever is larger, the error calculated by the fit or 
;		the one calculated for the modified convolved values below.
;
; 		- The heights calculated will be an upper limit value if multiple 
;		Gaussians are used to construct the IP AND if they have offset 
;		centers. 
;
;		Note that the individual convolved IP Gaussians with the data 
;		are additive, so the heights will be as well. BUT when the centers 
;		of the Gaussian IP are offset, then the maximum peaks of the  
;		resultant transformation wont perfectly align. 
;
;
; Created by Dr. Kat Barger 02/2014
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	w_ip=ip.w
	h_ip=ip.h
	c_ip=ip.c

ngauss=n_elements(w_ip)
for j=0, n_elements(params)-1 do begin

	w=params[j].width
	h=params[j].height
	c=params[j].center

	w_err=params[j].width_err
	h_err=params[j].height_err
	c_err=params[j].center_err

	w_arr=fltarr(ngauss)
	h_arr=fltarr(ngauss)
	c_arr=fltarr(ngauss)
	a_arr=fltarr(ngauss)

	w_err_arr=fltarr(ngauss)
	h_err_arr=fltarr(ngauss)
	c_err_arr=fltarr(ngauss)
	a_err_arr=fltarr(ngauss)

	for i=0, ngauss-1 do begin
		h_arr[i]=h*w*h_ip[i]*w_ip[i]*sqrt(2.*!pi)/sqrt(w^2.+w_ip[i]^2.)
			dh1=w*h_ip[i]*w_ip[i]*sqrt(2.*!pi)/sqrt(w^2.+w_ip[i]^2.)*h_err
			dh2=h*h_ip[i]*w_ip[i]^3.*sqrt(2.*!pi)/(w^2.+w_ip[i]^2.)^(3./2.)*w_err
		h_err_arr[i]=sqrt(dh1^2.+dh2^2.)

		w_arr[i]=abs(sqrt(w^2.+w_ip[i]^2.))
		w_err_arr[i]=w/sqrt(w^2.+w_ip[i]^2.)*w_err    ;w_ip[i]*w_err/w_arr

		c_arr[i]=c+c_ip[i]
		c_err_arr[i]=c_err

		a_arr[i]=(gauss_area([h_arr[i],0.,w_arr[i]],error=[h_err_arr[i],0.,w_err_arr[i]]))[0]
		a_err_arr[i]=(gauss_area([h_arr[i],0.,w_arr[i]],error=[h_err_arr[i],0.,w_err_arr[i]]))[1]
	endfor

	junk=max(h_arr,max_loc)
	params[j].height=total(h_arr)
	params[j].height_err=sqrt(total(h_err_arr^2.))
	params[j].center=c_arr[max_loc]
	;Position the Gaussian convolved with the largest amplitude ip peak
	params[j].center_err=c_err_arr[max_loc]

	junk=max(w_arr,max_loc)
	;The width Gaussian convolved with the largest amplitude ip peak 
	params[j].width=w_arr[max_loc]
	params[j].width_err=w_err_arr[max_loc]
	params[j].area=total(a_arr)

	;I'm not sure why tol_a_err gives me a lower value than the fit gives. 
	;Choose the larger of the two values to be on the safe side. 
	tol_a_err=sqrt(total(a_err_arr^2))
	params[j].area_err=(tol_a_err gt params[j].area_err) ? tol_a_err : params[j].area_err 


	if (NOT keyword_set(quiet)) then begin
		print,''
		print,'** Convolved Values',format='(A-25)'
		print,'area','width','height',format='(3(A-10))'
		print,params[j].area,params[j].width,params[j].height,format='(3(f-10.4))'
		print,''
	endif

endfor

	return

end