function wcstnxcor, xi, eta, tnx

    ;compute normalized values for xi and eta used in the 
    ;chebyshev and legendre polynomial functions:
    
    xin = (2.d * xi - (tnx.ximax + tnx.ximin)) / (tnx.ximax - tnx.ximin)
    etan = (2.d * eta - (tnx.etamax + tnx.etamin)) / (tnx.etamax - tnx.etamin)

    ncoor = n_elements(xi)
    pm = dblarr(4,ncoor)
    pm[0,*] = 1.
    pm[1,*] = xin
    for m=1,2 do pm[m+1,*] = 2.0d * xin * Pm[m,*] - Pm[m-1,*] 
    pn = dblarr(4,ncoor)
    pn[0,*] = 1.
    pn[1,*] = etan
    for n=1,2 do pn[n+1,*] = 2.0d * etan * Pn[n,*] - Pn[n-1,*] 
    
    sum = dblarr(ncoor)

    for i = 0, 9 do begin
        sum = sum + tnx.c[i] * Pm[tnx.m[i],*] * Pn[tnx.n[i],*]
    end

    return, sum

end
