;+
; NAME: mode
; PURPOSE:
; 	Calculate the mode (most common element in an array)
; 	optionally with binning
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2004-04-23 17:48:23 by Marshall Perrin 
;-

FUNCTION mode,input,bin=bin,autobin=autobin,quiet=quiet,help=help

        IF keyword_set(help) then begin
           print,' ' 
           print,'FUNCTION mode,input,bin=bin,autobin=autobin,quiet=quiet,help=help'
           return,0;
        endif

	h = histogram(float(input),binsize=binsize,omin=omin)
	if not(keyword_set(binsize)) then binsize=1
	mh = max(h)
	i = float(where(h eq max(h)))
	iout = i*float(binsize)+float(omin)
        ;print,iout
	return,iout[0]

        ;temp=input
        ;input=float(input)        
        ;IF keyword_set(autobin) THEN begin
        ;   autobin=1
        ;   plothist,input,xhist,yhist,/autobin,/noplot
        ;   bin=(max(input)-min(input))/sqrt(N_elements(input))
        ;ENDIF else plothist,input,xhist,yhist,bin=bin,/noplot
        ;plot,xhist-bin/2.0,yhist,psym=symcat(10)

        ;max_yhist=max(yhist)
        ;loc=where(yhist eq max(yhist))
        ;most_probable=avg(xhist(loc))-float(bin)/2.0

        ;input=temp
        ;return,most_probable

end
