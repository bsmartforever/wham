pro getchelle,infile,w,f,norder=norder,npix=npix,hd=hd,plot=plot
;+
;	Reads an iraf-formatted (multispec) echelle spectrum in FITS
;
;	IN: 	infile	- string	full name of the input .ec.fits file
;	
;	OUT:	w	- double	wavelength matrix (npix x norder)
;		f	- double	flux matrix	(npix x norder)
;
;	KEYWORDS: norder - integer	number of orders (xdisp dimension)
;		  npix	 - integer	number of pixels (dispersion dimen.)
;		  hd	 - strarr	header
;		  plot	 - 		produces a plot of the spectr. on demand
;
;	NOTES: - the program assumes, norder<npix
;
;	C. Allende Prieto, UT@Austin, November 2004 -only linear dispersion
;	I. Ramirez, UT, November 2006, modified to read from the header and 
;		apply a Doppler correction (redshift)	
;	C. Allende Prieto, April 2007, rewritten to handle polynomial and 
;		log-linear wavelength solutions
;					", IAC, May 2011, patched to handle 1-order files
;
;-

if N_params() LT 1 then begin
      print,'% getchelle: - getchelle,infile,w,f[,norder=norder,npix=npix,hd=hd,plot=plot]'
      return
endif

;reading the data
f=readfits(infile,hd)

;checking dimensions and finding out the dispersion direction
cc=size(hd)
if (cc(0) eq 1) then begin
	hdlines=cc(1) 
endif else begin
	print,'% getchelle: - error, the header has not 1 dimension'
	return
endelse
cc2=size(f)
if (cc2(0) eq 3) then begin ; this must be a 3-layer spectrum
	f=f[*,*,0]
	cc2=size(f)
endif

;if the first dimension is smaller than the 2nd, we interpret that
;as the first being the x-dispersion direction and we transpose
trans=0
if cc2(1) lt cc2(2) then begin
	f=transpose(f)
	trans=1
endif

;now we know
npix=cc2(1) & norder=cc2(2)
if (cc2(0) ne 2) then begin
	print,'% getchelle: - Warning, the data file is not 2-D'
	norder=1
endif
w=dblarr(npix,norder)

;physical and normalized coords.
;assumed LTV1=0.0 and LTM1_1=1.0
p=dindgen(npix)+1.d0
n=(p-(max(p)+min(p))/2.d0) / ((max(p)-min(p))/2.d0)

;extracting the wavelength calib. solution from the image header.

;first we combine the relevant lines from the header into a single string
i=0  & found=-1
while(found eq -1 and i lt hdlines) do begin
	found=strpos(hd(i),'spec1')
	i=i+1
endwhile
linear_header=' '
j=0
while (j le hdlines-1) do begin
	if (strpos(hd(j),'WAT2',0) ne -1) then begin
		linear_header=linear_header+strmid(hd(j),11,79-11)
	endif
endwhile


;loop on apertures

for order=1,norder do begin 

	key1='spec'+strtrim(string(order),2)
	key2='spec'+strtrim(string(order+1),2)
	;extract relevant line
	limit=strpos(linear_header,key2)
	if max(limit) eq -1 then begin
		if order eq norder then limit=strlen(linear_header) else begin
			print,'% getchelle: error, unexpected end of header'
			print,order,norder
			pause
			return
			endelse
	endif
	chunk=strmid(linear_header,strpos(linear_header,key1),$
	limit-strpos(linear_header,key1))
	;extract content (enclosed in quotes)
	quote_start=strpos(chunk,'"')
	quote_end  =strpos(chunk,'"',/reverse_search)
	chunk=strmid(chunk,quote_start+1,quote_end-quote_start-1)
	fields=strsplit(chunk,/extract)

	;load the basic fields
	ap=fix(fields(0))
	beam=fix(fields(1))
	dtype=fix(fields(2))
	w1=double(fields(3))
	dw=double(fields(4))
	nw=fix(fields(5))
	;check z consistency
	if order gt 1 then begin
	if abs(z-double(fields(6))) gt 1d-12 then begin
		print,'% getchelle: error, redshift inconsistent between'
		print,'% getchelle: consecutive apertures'
		return
	endif
	endif
	z=double(fields(6))
	aplow=double(fields(7))
	aphigh=double(fields(8))
	nfields=n_elements(fields)

	if (nfields gt 9) then begin
		; non linear dispersion
		;safety check
		if (dtype ne 2) then begin
		 	print,'% getchelle: -- error, unexpected value of dtype'
			return		
		endif
		nstat=9
		wt_total=0.d0
		while nstat lt nfields-1 do begin
			wt_i=double(fields[nstat])	   & nstat=nstat+1
			wt_total=wt_total+wt_i
			w0_i=double(fields[nstat])	   & nstat=nstat+1
			ftype_i=fix(fields[nstat])	   & nstat=nstat+1
			if ftype_i ne 1 and ftype_i ne 2 then begin
				print,'% getchelle: -error: ftype must be'
				print,'% getchelle: 1 (Chebyshev) or 2 (Legendre)'
				return
			endif

			ord=fix(fields[nstat]) 		   & nstat=nstat+1
			pr=[fields[nstat],fields[nstat+1]] & nstat=nstat+2
			pr=double(pr)

			;the mess below forced by the lack of blanks between 
			;the last coefficient and the following weight (wt_i)
			coef=fields[nstat:nstat+ord-2]	   & nstat=nstat+ord-1
			dotpos1=strpos(fields[nstat],'.')			
			dotpos2=strpos(fields[nstat],'.',/reverse_search)
			if (dotpos2 gt dotpos1) then begin
				coef=[coef,strmid(fields[nstat],0,dotpos2-1)]
				fields[nstat]=strmid(fields[nstat],dotpos2-1)
			endif else begin
				coef=[coef,fields[nstat]]
			endelse
			coef=double(coef)
			
			wn=dblarr(npix)
			peval,ftype_i,ord,coef,n,wn
			w[*,order-1]=w[*,order-1]+wt_i*(w0_i+wn)/(1.d0+z)
		endwhile
		if abs(total(wt_total)-1.d0) gt 1e-10 then begin
			print,'% getchelle: -error: weights do not add to 1.0'
			print,'% getchelle: wt_total=',wt_total
			return
		endif 
	endif else begin
		; linear
		case dtype of
		-1 : w[*,order-1]= p
		 0 : w[*,order-1]= (w1+dw*(p-1.d0))/(1.d0+z)
	 	 1 : w[*,order-1]= 10.d0^((w1+dw*(p-1.d0))/(1.d0+z))
	 	else: begin
	 		  print,'% getchelle: -- error, unexpected value of dtype=',dtype
			  return
 			end
		endcase
	endelse
endfor 	

;reversing wavelength scale, when orders have wavelength decreasing with pixel
;aperture order is left unchanged
if (w[1,0]-w[0,0] lt 0.d0) then begin
	w=reverse(w) 
	f=reverse(f)
endif

;picture it
if keyword_set(plot) then begin
	order=1
	step_1=median(w[*,0]-shift(w[*,0],1))
	wobject=where(strpos(hd,'OBJECT') gt -1)
	if min(wobject) gt -1 then wobject=min(wobject) else wobject=6
	
	plot,w(*,order-1),f(*,order-1)/mean(f(*,order-1))+order-1,$
	yrange=[0,norder+1],$
	ytitle='Aperture',title=hd(wobject),$
	charsize=1.2,xcharsize=.01,xrange=[w(0,order-1)-step_1*0.27*npix,$
	w(npix-1,order-1)+step_1*(npix*.3)],xstyle=1,ystyle=1
	xyouts,.4,.05,'Wavelength (Angstroms)',/normal,charsize=1.2
	xyouts,w(0,order-1)-step_1*0.32*npix,$
	1,w(0,order-1),charsize=1.
	xyouts,w(npix-1,order-1),1,$
	w(npix-1,order-1),charsize=1.
	
	for order=2,norder do begin
		oplot,w(*,0),f(*,order-1)/mean(f(*,order-1))+order-1
		xyouts,w(0,0)-step_1*0.32*npix,$
		float(order),w(0,order-1),charsize=1.
		xyouts,w(npix-1,0),float(order),$
	        w(npix-1,order-1),charsize=1.
	endfor
endif

end

