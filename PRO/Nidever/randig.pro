function randig

; returns a random digit, 0-9

;time = systime(1)
;seed = time*1000.-floor(time*1000.)
;seed = seed*100
rnd = randomu(seed,/uniform)

integer = floor(rnd*10.)
if integer eq 10 then integer = 9


;stop

return,integer

end
