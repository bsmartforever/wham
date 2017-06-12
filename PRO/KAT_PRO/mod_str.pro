function mod_str, structure, tagname, newvalue

NAMES = TAG_NAMES(Structure)

cmdline = 'S1={'

for i_tag = 0,N_TAGS(Structure)-1,1 do begin

if strcmp( STRUPCASE(TagName), NAMES(i_tag) ) ne 1 then begin
cmdline = cmdline + NAMES(i_tag) + ': structure.('+strtrim(string(i_tag),2)+')'
endif else begin
cmdline = cmdline + NAMES(i_tag) + ': newvalue'
endelse

if i_tag lt N_TAGS(Structure)-1 then begin
cmdline = cmdline + ', '
endif else begin
cmdline = cmdline + ' }'
endelse

endfor

result = EXECUTE(cmdline);

structure = S1

return,structure

end