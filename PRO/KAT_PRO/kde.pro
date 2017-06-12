;=== snip here for kde.pro === 
;+ 
; NAME: 
;    KDE 
; 
; PURPOSE: 
;    Estimate the probability density underlying a set of discrete 
;    samples (measurements) using the kernel density estimator method. 
; 
; CATEGORY: 
;    Statistics 
; 
; CALLING SEQUENCE: 
;    d = kde(x, t) 
; 
; INPUTS: 
;    x: discrete samples of the desired distribution. 
; 
;    t: values for which the probability density is required. 
; 
; KEYWORD PARAMETERS: 
;    scale: smoothing factor, also called the bandwidth 
;        used to compute the kernel density estimate 
; 
; KEYWORD FLAGS: 
;    By default, KDE uses the Epanechnikov kernel to compute 
;    the kernel density estimate, this can be overridden by 
;    setting one of the following flags: 
;    GAUSSIAN: use Gaussian kernel 
;    TRIANGULAR: use triangular kernel 
;    BIWEIGHT: use biweight kernel 
; 
; OUTPUTS: 
;    d: probability density estimated at each value specified by t 
; 
; RESTRICTIONS: 
;    Gaussian kernel used for multivariate systems. 
; 
; PROCEDURE: 
;    d_i = (1/n) \sum_{j = 1}^n K((x_j - t_i)/h) / h 
; 
;    where h is the estimated optimal smoothing parameter and 
;    where K(z) is the selected kernel. 
; 
; REFERENCE: 
; B. W. Silverman, 
; Density Estimation for Statistics and Data Analysis 
; (CRC Press, Boca Raton, 1998) 
; 
; EXAMPLE: 
;    IDL> x = randomn(seed, 1000) 
;    IDL> t = 2. * findgen(100)/99. 
;    IDL> d = kde(x,t) 
;    IDL> plot, t, d 
;    IDL> plot, t, histogram(x, min=0, max=2, nbins=100), /noerase 
; 
; MODIFICATION HISTORY: 
; 09/18/2010 Written by David G. Grier, New York University 
; 
; Copyright (c) 2010 David G. Grier 
; 
;- 
function kde_nd, x, y, $ 
                  scale = scale 
COMPILE_OPT IDL2, HIDDEN 
sx = size(x, /dimensions) 
sy = size(y, /dimensions) 
nd = sx[0]                      ; number of dimensions 
nx = sx[1]                      ; number of data points 
ny = sy[1]                      ; number of sampling points 
; optimal smoothing parameter in each dimension 
; Silverman Eqs. (3.30) and (3.31) 
sx = stddev(x, dimension=2) 
rx = fltarr(nd) 
for d = 0, nd-1 do $ 
    rx[d] = iqr(x[d,*]) 
h = 0.9 * (sx < rx/1.34) / nx^0.2 
scale = h 
; density estimate 
; Silverman Eq. (2.15) and Table 3.1 
fac = replicate(1., nx) 
res = fltarr(ny, /nozero) 
hfac = h # fac 
for j = 0, ny-1 do begin 
    z = total(0.5 * ((x - y[*,j] # fac) / hfac)^2, 1) 
    res[j] = mean(exp(-z)) 
endfor 
res /= 2. * !pi * total(h^2)^(nd/2) ; normalization 
return, res 
end 
function kde_1d, x, y, $ 
                  scale = scale, $ 
                  biweight = biweight, $ 
                  triangular = triangular, $ 
                  gaussian = gaussian 
COMPILE_OPT IDL2, HIDDEN 
nx = n_elements(x)              ; number of data points 
ny = n_elements(y)              ; number of samples 
; optimal smoothing parameter 
; Silverman Eqs. (3.30) and (3.31) 
sx = stddev(x)                  ; standard deviation 
rx = iqr(x)                     ; interquartile range 
h = 0.9 * (sx < rx/1.34) / nx^0.2 
scale = h 
; density estimate 
; Silverman Eq. (2.15) and Table 3.1 
t = x/h 
s = y/h 
res = fltarr(ny)                ; result 
if keyword_set(biweight) then begin 
    for j = 0, ny-1 do begin 
       z = (t - s[j])^2 
       w = where(z lt 1.) 
       res[j] = 15. * total((1. - z[w])^2) / (h * 16. * nx) 
    endfor 
endif $ 
else if keyword_set(triangular) then begin 
    for j = 0, ny-1 do begin 
       z = abs(t - s[j]) 
       w = where(z lt 1.) 
       res[j] = total(1. - z[w]) / (h * nx) 
    endfor 
endif $ 
else if keyword_set(gaussian) then begin 
    for j = 0, ny-1 do begin 
       z = 0.5 * (t - s[j])^2 
       res[j] = mean(exp(-z)) / (h * sqrt(2.*!pi)) 
    endfor 
endif $ 
else begin                      ; Epanechnikov 
    for j = 0, ny-1 do begin 
       z = (t - s[j])^2 
       w = where(z lt 5) 
       res[j] = 0.75 * total((1. - z[w]/5)) / (h * sqrt(5.) * nx) 
    endfor 
endelse 
return, res 
end 
function kde, x, y, scale = scale, $ 
               gaussian = gaussian, $ 
               biweight = biweight, $ 
               triangular = triangular 
COMPILE_OPT IDL2 
sx = size(x) 
sy = size(y) 
if sx[0] gt 2 then $ 
    message, "data must be organized as [ndims,npoints]" 
if sy[0] ne sx[0] then $ 
    message, "inputs must have the same number of dimensions" 
if (sx[0] eq 2) and (sx[1] ne sy[1]) then $ 
    message, "inputs must have the same number of dimensions" 
ndims = (sx[0] eq 2) ? sx[1] : 1 
if ndims gt 1 then begin 
    if keyword_set(biweight) or keyword_set(triangular) then $ 
       message, "Multidimensional: using Gaussian kernel", /inf 
    res = kde_nd(x, y, scale = scale) 
endif else $ 
    res = kde_1d(x, y, scale = scale, $ 
                gaussian = gaussian, $ 
                biweight = biweight, $ 
                triangular = triangular) 
return, res 
end 
