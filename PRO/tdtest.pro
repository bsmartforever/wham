pro tdtest, smc, iha, los, sphere=sphere, ell1=ell1, ell2=ell2, ell3=ell3, radius=radius
; This is the three-D test program. At the bottom is a 3-d scatterplot that 
;is currently commented out that shows what the model looks like
;Using the coordinates of the SMC, I create a 3-D model to estimate the line of sight depth
;Keywords are used to make the different shapes. Sphere makes a sphere of the given radius
;ell1 makes an ellipsoid with the axis ratios of 1:1.17:1.28
;ell2 makes an ellipsoid with the acis ratios of 1:1.24:1.39
;ell3 makes an ellispoid with the axis ratios of 1:1.33:1.61
;The axis ratios all come from Subramanian and Subramaniam 2012.
;Sphere is the default because I'm lazy

;

IF NOT keyword_set(radius) THEN radius=10000.0

;makes the ellipsoid equation into a sphere by setting all sem-major axis equal
IF (keyword_set(sphere)) OR (NOT keyword_set(ell1) AND NOT keyword_set(ell2) AND NOT keyword_set(ell3)) THEN BEGIN

a= radius
b= radius
c= radius

ENDIF


IF keyword_set(ell1) THEN BEGIN 

a= 1.5*radius
b= 1.0*radius
c= 1.0*radius

ENDIF

;Second ellipsoid axis ratio

IF keyword_set(ell2) THEN BEGIN 

a= 1.5*radius
b= 1.0*radius
c= 1.0*radius

ENDIF


IF keyword_set(ell3) THEN BEGIN 

a= 1.0*radius
b= 0.5*radius
c= 0.5*radius

ENDIF

print, a, b, c

;I don't even use this. Why did I bother?
;read in coordinates
file = './v_list.list'
OPENR, lun, file, /GET_LUN
; Read one line at a time, saving the result into array
array = ''
line = ''
WHILE NOT EOF(lun) DO BEGIN & $
  READF, lun, line & $
  array = [array, line] & $
ENDWHILE
; Close the file and free the file unit
FREE_LUN, lun


; I need to make the coordinates into something like xy coordinates centered around 0
  v_list = make_array(n_elements(smc.glon), 3, VALUE=0.0)
  glon=smc.glon
  glat=smc.glat
  ;mean_lon= glon(where(iha eq MAX(iha)))
  ;mean_lat= glat(where(iha eq MAX(iha)))
  ;mean_lon=301.698
  ;mean_lat=-44.497
  ;Taken from Stanmarovic 2004 paper. This is the estimated kinematic center in radio
  mean_lon=301.6894
  mean_lat=-44.8504

;I need to translate this from degrees to parsecs. This should be a centered array
 cent_lon=glon
 cent_lat=glat

 glon_par=glon
 glat_par=glat

FOR i= 0, n_elements(smc.glon)-1 DO BEGIN
cent_lon[i]=glon[i]-mean_lon
cent_lat[i]=glat[i]-mean_lat
;cent_lat[i]=61000*tan(cent_lat[i]*!dtor)

ENDFOR
;Hopefully correctly converts coordinates to parsecs from degrees
glon_par=61000.0*tan(cent_lon*!dtor)
glat_par=61000.0*tan(cent_lat*!dtor)



;Here is the equation for a sphere, evetually program should read in what model I want and go with that, plus read in radius

;Equation for a sphere
z = make_array(n_elements(smc.glon), 1, VALUE=0.0)
;z=SQRT(-((glon_par)^2)/a^2-((glat_par)^2)/b^2+1)*c
;This doubles up the array so I can visually see the 3-D image
glon=glon_par
glat=glat_par
z_full=z

full_array=make_array(n_elements(z_full),3)
full_array[*,0]=glon
full_array[*,1]=glat
full_array[*,2]=z_full



  los_fix=where(FINITE(z_full) eq 0, /null)
  z_full[los_fix]=0


;Non-rotated ellipsoid
;dist_size = SCATTERPLOT3D(smc.glon, smc.glat, $
;   full_array[*,2], SYM_OBJECT=ORB(), /SYM_FILLED, $
;   RGB_TABLE=7, AXIS_STYLE=2, SYM_SIZE=1)


;going to attempt to rotate now

rot_x = make_array(3,3, value = 0.0)
rot_y = make_array(3,3, value= 0.0)
rot_z = make_array(3,3, value= 0.0)

; x rotation array
x_angle=5.0
y_angle=85.0
z_angle=-52.39148997001

rot_x[0,0]=1.0
rot_x[1,1]=COS(x_angle*!dtor)
rot_x[2,2]=COS(x_angle*!dtor)
rot_x[1,2]=SIN(x_angle*!dtor)
rot_x[2,1]=-SIN(x_angle*!dtor)

;print, rot_x

;y rotation matrix
rot_y[0,0]=COS(y_angle*!dtor)
rot_y[1,1]=1.0
rot_y[2,2]=COS(y_angle*!dtor)
rot_y[0,2]=-SIN(y_angle*!dtor)
rot_y[2,0]=SIN(y_angle*!dtor)

;z rotation matrix

rot_z[0,0]=COS(z_angle*!dtor)
rot_z[1,1]=COS(z_angle*!dtor)
rot_z[2,2]=1.0
rot_z[0,1]=-SIN(z_angle*!dtor)
rot_z[1,0]=SIN(z_angle*!dtor)


;Think it needs to be going the other direction

;full_array=rot_x#full_array

;full_array=full_array#rot_x
;full_array=full_array#rot_y
full_array=full_array#rot_z

full_array[*,2]=SQRT(-((full_array[*,0])^2)/a^2-((full_array[*,1])^2)/b^2+1)*c

; fix nans to avoid shrinking array
  array_fix=where(FINITE(full_array[*,2]) eq 0, /null)
  full_array[array_fix,2]=0.0
  ;full_array[*,2]=0.0

;Thisis currently messed up, not getting the correct locations. Imap may be missing blocks because of fixes, check

    faint_loc=where((full_array[*,2] lt 1500.0 ) AND (full_array[*,2] gt 0))
    full_array[faint_loc,2]=1500.0
    faint_loc=where((full_array[*,2] lt 1500.0 ) AND (iha ge .23))

    full_array[faint_loc,2]=1500.0

    faint_loc=where(iha lt .23)

    full_array[faint_loc,2]=0


final_array=make_array(n_elements(z_full)*2,3)




final_array[*,0]=[[full_array[*,0]],[full_array[*,0]]]
final_array[*,1]=[[full_array[*,1]],[full_array[*,1]]]
final_array[*,2]=[[full_array[*,2]],[-full_array[*,2]]]


z_angle=-z_angle
rot_z[0,0]=COS(z_angle*!dtor)
rot_z[1,1]=COS(z_angle*!dtor)
rot_z[2,2]=1.0
rot_z[0,1]=-SIN(z_angle*!dtor)
rot_z[1,0]=SIN(z_angle*!dtor)

;Trying to get the colors to map to intensity Here
zmin = 0.03
zmax = 1.0
cbottom = 0
        color = bytscl(iha, $
                        max = zmax, min = zmin, $
                        top = !d.table_size-2-cbottom) $
                  + cbottom

color=[[color],[color]]
; Need 
los= final_array[0:n_elements(z_full)-1,2]
print, size (final_array)

print, size(color)

;transpose back to the correct orientation. Doesnt really matter but whatever
;full_array=TRANSPOSE(full_array)

;The total line of sight is the total z distantances from the center. For a sphere, it is just z+z. 
;For other shapes, though, it will have to be the two different z's, especially when rotated

; Rotated ellipsoid
;dist_size = SCATTERPLOT3D(final_array[*,0], final_array[*,1], $
;   final_array[*,2], SYM_OBJECT=ORB(), /SYM_FILLED, $
;   RGB_TABLE=7, AXIS_STYLE=2, SYM_SIZE=1)

no_faints=where(final_array[*,2] gt 0 OR final_array[*,2] lt 0)

longlon=[[smc.glon],[smc.glon]]
longlat=[[smc.glat],[smc.glat]]



;@mycmap

dist_size = SCATTERPLOT3D(longlon[no_faints], longlat[no_faints], $
   final_array[no_faints,2], SYM_OBJECT=ORB(), /SYM_FILLED, MAGNITUDE=color[no_faints,0],$
   RGB_TABLE=33, AXIS_STYLE=2, SYM_SIZE=1, XRANGE=[MAX(smc.glon), MIN(smc.glon)])





END