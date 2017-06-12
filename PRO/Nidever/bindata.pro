pro bindata,x,y,xbin,ybin,binsize=binsize,min=min,max=max,nan=nan,stp=stp,std=std

;+
; Use this to bin X/Y data.  Uses Histogram.pro.
;
; INPUTS:
;  x          Array of X values to be binned
;  y          Array of Y values to be binned
;  =binsize   Binsize for X dimension
;  =min       Minimum X value   
;  =max       Maximum X value
;  /nan       Ignore NaNs
;  /std       Return the standard deviation of Y for each bin instead
;             of the mean
;  /stp       Stop at the end of the program
;
; OUTPUTS:
;  xbin       The X-value for each bin (center).
;  ybin       The mean of the binned Y values per bin.  If /std is
;             set the standard deviation is used.
;
; USAGE:
;  bindata,x,y,xbin,ybin,binsize=binsize,min=min,max=max,nan=nan,stp=stp,std=std
;
;
; By D.Nidever  April2 007
;-

; Not enough inputs
if n_elements(x) eq 0 or n_elements(y) eq 0 then begin
  print,'Syntax - bindata,x,y,xbin,ybin,binsize=binsize,min=min,max=max,nan=nan,stp=stp,std=std'
  return
endif

; Bin the X data and get the indices
H = HISTOGRAM(X, binsize=binsize, min=min, max=max, nan=nan, REVERSE_INDICES = R, locations=locations)

; How the REVERSE_INDICES WORK
;Set all elements of A that are in the ith bin of H to 0.  
;IF R[i] NE R[i+1] THEN A[R[R[I] : R[i+1]-1]] = 0  
;The above is usually more efficient than the following: 
;bini = WHERE(A EQ i, count)  
;IF count NE 0 THEN A[bini] = 0  


nh = n_elements(h)
ybin = h*0.0

; Loop through the bins
; and bin the Y values
for i=0,nh-1 do begin
  IF R[i] NE R[i+1] THEN begin
    y2 =  Y[R[R[I] : R[i+1]-1]]
    ny2 = n_elements(y2)

    ; Mean
    ybin[i] = total(y2)/ny2

    ; Doing St.Dev.
    if keyword_set(std) then begin
      ybin[i] = stddev(y2)
    endif

  endif
end

; X-values for each bin
xbin = locations+0.5*binsize

if keyword_set(stp) then stop

end
