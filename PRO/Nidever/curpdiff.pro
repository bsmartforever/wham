pro curpdiff

; This program allows you to find the distance between two points.

print,'FIRST CLICK'
cursor,x1,y1
wait,0.2

print,'SECOND CLICK'
cursor,x2,y2

print,'Distance = ',stringize(sqrt((x1-x2)^2. + (y1-y2)^2.),ndec=4)
print,'Delta X = ',stringize(x2-x1,ndec=4)
print,'Delta Y = ',stringize(y2-y1,ndec=4)
print,'Angle = ',stringize(atan(y2-y1,x2-x1)*!radeg,ndec=4),' (CCW from Right)'
lineq,x1,y1,x2,y2

;stop

end