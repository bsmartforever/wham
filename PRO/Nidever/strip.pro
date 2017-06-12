function strip,input
; output=strcompress(input,/remove_all) does the same thing
n = n_elements(input)
output = input
for i=0,n-1 do begin
 output(i) = strjoin(strsplit(input(i),/extract),'')
end
return,output
;return,strjoin(strsplit(input,/extract),'')
end
