PRO solidsquare, a,b
;This progeam calculated the solid angle of a projected square in two different ways to compare
;While most inputs will have a and b equal, I allow for variance for future use.
;

	omega1= 4*ASIN(SIN(a/2)*SIN(b/2))
	omega2 = a*b
	print, omega1, omega2
	end