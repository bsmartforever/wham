pro make_gif_html,htmlfile,giflist,dir=dir,width=width,$
                  index=index,link=link

;+
; This program makes a webpage for a bunch of gif files
;
; INPUTS:
;  htmlfile   The html output file (default htmlfile='gifpage.html')
;  giflist    List of gif files
;  =dir       Do all the gifs in this directory
;  =width     Width of the images.  The default is full size.
;  /index     Make an index of the images at the top
;  /link      Make the image a link to itself.
;
; Written by D.Nidever  Sept.2006
;-

; Use giflist
if keyword_set(giflist) then begin
  loadinput,giflist,names
  ;readcol,giflist,names,format='A',/silent
end

; Use all the gifs in this directory
if keyword_set(dir) then begin
  names = file_search(dir+'*.gif')
endif

n = n_elements(names)
if n eq 0 then begin
  print,'Syntax - make_gif_html,htmfile,giflist,dir=dir'
endif

if not keyword_set(htmlfile) then htmlfile = 'gifpage.html'

if keyword_set(width) then add = ' width='+strtrim(width,2) else add=''

; Write the HTML file
openw,unit,/get_lun,htmlfile

printf,unit,'<HTML>'
printf,unit,'<BODY>'

; Make an index
if keyword_set(index) then begin
  printf,unit,'<h1>Index</h1><br>'
  for i=0,n-1 do begin
    printf,unit,'<a href="#'+names[i]+'">'+names[i]+'</a>'
  end
end

for i=0,n-1 do begin

  ; Make a link to itself
  link1 = ''
  link2 = ''
  if keyword_set(link) then begin
    link1 = '<a href="'+names[i]+'">'
    link2 = '</a>'
  endif

  ; Make an anchor
  if keyword_set(index) then printf,unit,'<a name="'+names[i]+'"></a>'

  printf,unit,'<h2>'+names[i]+'</h2><br>'
  printf,unit,link1+'<img src="'+names[i]+'"'+add+'>'+link2+'<br>'
  printf,unit,'<p>'

end

printf,unit,'</BODY>'
printf,unit,'</HTML>'

close,unit
free_lun,unit

print,'HTML file ',htmlfile,' created'

;stop

end
