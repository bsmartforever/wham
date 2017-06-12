PRO BESTAIRMASS, year, month, day, hour
@colplstart
filen=STRING(STRTRIM(year-2000,1),STRING(month,FORMAT='(I02)'),STRING(day,FORMAT='(I02)'),STRING(hour,FORMAT='(I02)'),'.ps')

device, file = filen

; Example bestairmass, 2015, 11, 04, 21.0
;Input the date and it outbuts  which MC blocks areabove 60

;This reads in all of the pointings. Keep in galactic coordinates. Euler transforms ro equitorial
OPENR,1,'./MC_pointings_all.dat'
length=file_lines('MC_pointings_all.dat')

;This reads in all the pointings in magellanic coordinates so I can map it flat
OPENR,2, './MC_pointings_all_MC.dat'
length2=file_lines('MC_pointings_all_MC.dat')

loadct,10
H=FLTARR(3,length) ;A big array to hold the data 
S=FLTARR(3)      ;A small array to read a line 
FOR n=0,length-1 DO BEGIN 
    READF,1,S    ;Read a line of data 
    H[*,n]=S     ;Store it in H 
ENDFOR 

;Need to fix names. These are defualts from where I found the example and hold no meaning.

H2=FLTARR(3,length2) ;A big array to hold the data 
S2=FLTARR(3)      ;A small array to read a line 
FOR n=0,length2-1 DO BEGIN 
    READF,2,S2    ;Read a line of data 
    H2[*,n]=S2     ;Store it in H 
ENDFOR 

CLOSE,1
CLOSE,2

; Getting both the real time input as well as the advanced julian date to test weather an object is rising or setting
JDCNV, year, month, day, hour, julian
JDCNV, year, month, day, hour+.05, julianadv
euler, H[0,*], H[1,*], ra, dec, 2


; Here I am going to make an array of all of the # of points in each block

f=0;
BLOCKSCOUNT=FLTARR(2,2000)

for n=0,length-1 DO BEGIN

B = WHERE(H[2,*] EQ H[2,n], count, COMPLEMENT=B_C, NCOMPLEMENT=count_c)

	if BLOCKSCOUNT[1,f] LT H[2,n] THEN BEGIN
	f=f+1
	BLOCKSCOUNT[0,f]= count
	BLOCKSCOUNT[1,f]=H[2,n]

	ENDIF 

ENDFOR

;Just hardcoding in exptime and pointing_verhead.

exptime=30
pointing_overhead=6.7
block_overhead = 18


;Here I make the 2 zd calculates to check if the object is rising or not.
eq2hor, ra, dec, julian, alt, az, obsname = 'ctio'
eq2hor, ra, dec, julianadv, altadv, azadv, obsname = 'ctio'
zd = 90-alt
zdadv = 90 -altadv
hold=0
topten=MAKE_ARRAY(2,10)

; Currently, this code finds all the blocks as close to zenith as 65 degrees, but then filters it down to 10. Can adjust. I want to map out and color

BLOCKS=FLTARR(2,2000)
m=0
holdarray=[9999,9999,9999,99999,99999]
blon=0
blat=0
bcount=0
bnum=H[2,0]

bdata = MAKE_ARRAY(432, 4, /FLOAT)


for n=0, length-1 DO BEGIN

;create a loop that counts the number of points in each block. Assign block # plus # of points to an array. When it comes time to map, check this for block number, use points to average coordinates for each block

;If the block is a member of the main block, the coordinates get added up here
if bnum EQ H[2,n] THEN BEGIN

blon=blon+H2[0,n]
blat=blat+H2[1,n]

bcount=bcount+1

ENDIF
;If it is the next block, the numbers for the block get recorded and things reset in here and bnum moves up to the next block
if bnum NE H[2,n] THEN BEGIN

bdata[bnum-7000,0]=bnum
bdata[bnum-7000,1]=bcount
bdata[bnum-7000,2]=blon/bcount-360
bdata[bnum-7000,3]=blat/bcount

blon=H2[0,n]
blat=H2[1,n]

bcount=1
bnum=H[2,n]

ENDIF


if zd[n] LT 60 THEN BEGIN

	if BLOCKS[1,m] LT H[2,n] THEN BEGIN
	m=m+1
	BLOCKS[0,m]= zd[n]
	BLOCKS[1,m]=H[2,n]

	if zd[n] LT zdadv[n] THEN BEGIN
	motion = -1
	ENDIF
	if zd[n] GE zdadv[n] THEN BEGIN
	motion = 1
	ENDIF

	B = WHERE(H[2,*] EQ H[2,n], count, COMPLEMENT=B_C, NCOMPLEMENT=count_c)
	
	;Currently has everything in MC coordinates. Switching back to ecl temprorarily
	holdarray=[[holdarray],[[max(zd[B]),H[2,n],H[0,n]-360,H[1,n],motion]]]
	hold=0

	ENDIF 

ENDIF

ENDFOR
fakearray=holdarray

for l=0, 9 DO BEGIN

	mini=MIN(fakearray[0,*],index)
	topten[0,l]=index
	topten[1,l]=fakearray[1,index]
	fakearray[0,index]=99999
ENDFOR

print, "ZD	"+"Block	" + "l	" + "b	" + "Rising/Setting	"
print, holdarray[*,topten[0,*]]

nul1=[0]
nul2=[0]

;Make the plot
plot, nul1, nul2, xrange=[5,-125],ytitle='b',charsize=1.5,title='Magellanic Stream',$
xtitle='l',yrange=[-25,25],background=255,color=0, psym=1


;Where I color code everything. The observable blocks are

holdsize=size(holdarray[1,*], /n_elements)

; Colors all the observable blocks above 65 degrees
for l=0, length2-1 DO BEGIN
	for m=0, holdsize-1 DO BEGIN
		if H2[2,l] EQ holdarray[1,m] THEN BEGIN
			if (holdarray[0,m] GT 60)THEN BEGIN
			c=50
			ENDIF
			if (holdarray[0,m] GT 50) && (holdarray[0,m] LT 60) THEN BEGIN
			c=100
			ENDIF
			if (holdarray[0,m] GT 40) && (holdarray[0,m] LT 50)  THEN BEGIN
			c=135
			ENDIF
			if (holdarray[0,m] GT 30) && (holdarray[0,m] LT 40)  THEN BEGIN
			c=175
			ENDIF
			if(holdarray[0,m] LT 30)  THEN BEGIN
			c=215
			ENDIF
		ENDIF

	ENDFOR
	for m=0, 9 DO BEGIN
		if H2[2,l] EQ topten[1,m] THEN BEGIN
		sym=5
		ENDIF
	ENDFOR

	OPLOT, [H2[0,l]-360], [H2[1,l]],  color=c, psym=sym
	sym=1
	c=0


ENDFOR

for l=0, 430 DO BEGIN

  xyouts, bdata[l,2]+2.5, bdata[l,3], fix(bdata[l,0]-7000), CHARSIZE=.3,color = color

ENDFOR


device,/close

END
