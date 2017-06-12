pro bart_modified

   tvlct, r, g, b, /get

;white
r[0:9]=255
g[0:9]=255
b[0:9]=255

;gray to white
min=0 & max=50
r[min+1:max]=(fix(interpol([0,150],max-min),type=2))
g[min+1:max]=(fix(interpol([0,155],max-min),type=2))
b[min+1:max]=(fix(interpol([0,155],max-min),type=2))

;yellow to gray
min=50 & max=135
r[min+1:max]=(fix(interpol([150,255],max-min),type=2))
g[min+1:max]=(fix(interpol([155,255],max-min),type=2))
b[min+1:max]=(fix(interpol([155,178],max-min),type=2))


;yellow to gray
min=135 & max=155
r[min+1:max]=(fix(interpol([255,255],max-min),type=2))
g[min+1:max]=(fix(interpol([255,255],max-min),type=2))
b[min+1:max]=(fix(interpol([191,191],max-min),type=2))

;orange to yellow/orange
min=155 & max=175
r[min+1:max]=(fix(interpol([255,253],max-min),type=2))
g[min+1:max]=(fix(interpol([255,175],max-min),type=2))
b[min+1:max]=(fix(interpol([178,75],max-min),type=2))

;yellow to orange/read
min=175 & max=205
r[min+1:max]=(fix(interpol([253,255],max-min),type=2))
g[min+1:max]=(fix(interpol([175,141],max-min),type=2))
b[min+1:max]=(fix(interpol([75,60],max-min),type=2))

;orange/red to red
min=205 & max=255
r[min+1:max]=(fix(interpol([255,227],max-min),type=2))
g[min+1:max]=(fix(interpol([141,26],max-min),type=2))
b[min+1:max]=(fix(interpol([60,28],max-min),type=2))

   r[0] = 255 & g[0] = 255 & b[0] = 255
   tvlct, r, g, b
   ;tvscl,dist(400)

end