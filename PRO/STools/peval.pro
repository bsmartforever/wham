pro	peval,ftype,ord,coef,n,result
;+
;	Iterative evaluation of a Chebyshev or Legendre polynomial
;
;	IN: ftype	integer		=1 (Chebyshev) =2 (Legendre)
;	    ord		integer		order
;	    coef	dblarr		polynomial coefficients
;	    n		dblarr		normalized coords of the array (-1.:1.)
;	
;	OUT: result	dblarr		polynomial values for pixels in array n
;
;	C. Allende Prieto, April 2007, to support for getchelle
;   Andrei Tokovinin, September 2012, fixed a bugin the Legendre evaluation
;-

if N_params() lt 4 then begin
	print,'% peval: use -- peval,ftype,ord,coef,n,result'
	return
endif

if N_elements(coef) ne ord then begin
	print,'% peval: ord should match the size of the array coef'
        return	
endif

np=n_elements(n) ;number of pixels 
xi=dblarr(3,np)  ;storage for current and two previous xi-s

case ftype of

	1: begin
		xi[0,*]=replicate(1.d0,np)
		xi[1,*]=n
		result=coef[0]*xi[0,*]+coef[1]*xi[1,*]
		for i=3,ord do begin
			xi[2,*]=2.d0*n*xi[1,*]-xi[0,*]
			result=result+coef[i-1]*xi[2,*]
			xi[0,*]=xi[1,*]
			xi[1,*]=xi[2,*]
		endfor
	end

	2: begin

; Use IDL legendre function 
;            result = dblarr(np)
;            for i=0,ord-1 do result += coef[i]*legendre(n,i, /double)

		xi[0,*]=replicate(1.d0,np)
		xi[1,*]=n
		result=coef[0]*xi[0,*]+coef[1]*xi[1,*]
		for i=3,ord do begin
		  xi[2,*]=((2.d0*i-3.d0)*n*xi[1,*]-(i-2.0d0)*xi[0,*])/(i-1.d0)
			result=result+coef[i-1]*xi[2,*]
			xi[0,*]=xi[1,*]
			xi[1,*]=xi[2,*]
		endfor
	end
	else: begin
		print,'% peval: error, only ftypes=1 (Chebyshev) and 2 (Legendre) are supported'
		result=0.d0
		return
		end

endcase

end
