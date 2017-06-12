pro bart_modified

   tvlct, r, g, b, /get

;white
r[0:9]=255
g[0:9]=255
b[0:9]=255

;gray to white
min=9 & max=13
r[min+1:max]=(fix(interpol([255,225],max-min),type=2))
g[min+1:max]=(fix(interpol([255,225],max-min),type=2))
b[min+1:max]=(fix(interpol([255,225],max-min),type=2))

;gray to white
min=13 & max=25
r[min+1:max]=(fix(interpol([225,150],max-min),type=2))
g[min+1:max]=(fix(interpol([225,155],max-min),type=2))
b[min+1:max]=(fix(interpol([225,155],max-min),type=2))

;yellow to gray
min=25 & max=75
r[min+1:max]=(fix(interpol([150,255],max-min),type=2))
g[min+1:max]=(fix(interpol([155,255],max-min),type=2))
b[min+1:max]=(fix(interpol([155,178],max-min),type=2))


;orange to yellow/orange
min=75 & max=120
r[min+1:max]=(fix(interpol([255,253],max-min),type=2))
g[min+1:max]=(fix(interpol([255,204],max-min),type=2))
b[min+1:max]=(fix(interpol([178,138],max-min),type=2))

;yellow to orange/read
min=120 & max=190
r[min+1:max]=(fix(interpol([253,252],max-min),type=2))
g[min+1:max]=(fix(interpol([204,141],max-min),type=2))
b[min+1:max]=(fix(interpol([138,89],max-min),type=2))

;yellow to orange/red
min=190 & max=255
r[min+1:max]=(fix(interpol([252,215],max-min),type=2))
g[min+1:max]=(fix(interpol([141,48],max-min),type=2))
b[min+1:max]=(fix(interpol([89,31],max-min),type=2))

   r[0] = 255 & g[0] = 255 & b[0] = 255
   tvlct, r, g, b
   ;tvscl,dist(400)

end