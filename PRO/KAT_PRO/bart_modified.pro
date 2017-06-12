pro bart_modified

   tvlct, r, g, b, /get

;white
r[0:4]=255
g[0:4]=255
b[0:4]=255

;gray to white
min=4 & max=65
r[min+1:max]=(fix(interpol([255,150],max-min),type=2))
g[min+1:max]=(fix(interpol([255,155],max-min),type=2))
b[min+1:max]=(fix(interpol([255,155],max-min),type=2))

;yellow to gray
min=65 & max=160
r[min+1:max]=(fix(interpol([150,255],max-min),type=2))
g[min+1:max]=(fix(interpol([155,255],max-min),type=2))
b[min+1:max]=(fix(interpol([155,178],max-min),type=2))

;yellow to orange/read
min=160 & max=255
r[min+1:max]=(fix(interpol([255,239],max-min),type=2))
g[min+1:max]=(fix(interpol([255,59],max-min),type=2))
b[min+1:max]=(fix(interpol([178,44],max-min),type=2))

   r[0] = 255 & g[0] = 255 & b[0] = 255
   tvlct, r, g, b
   ;tvscl,dist(400)

end