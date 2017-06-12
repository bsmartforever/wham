pro locatespectrum, channelin, slitin, xledge, xredge, ybot, ytop, $
	xpixsize, ypixsize, xslitsize, yslitsize, xsia, ysia, $
	xmask, ymask, mjd=mjd, farf=farf, _extra=e
;+
; NAME:
;	LOCATESPECTRUM
;
; PURPOSE:
;	This procedure returns the nominal x and y extents of the chosen 
;	spectrum.
;
; CATEGORY:
;	Data.
;
; CALLING SEQUENCE:
;	LOCATESPECTRUM, Channel, Slit, Xledge, Xredge, Ybot, Ytop,
;		Xpixsize, Ypixsize, Xslitsize, Yslitsize, Xsia, Ysia,
;		Xmask, Ymask
;
; INPUTS:
;	Channel:Channel name, e.g. 'LiF1A'
;	Slit:	Slit name: 'HIRS', 'MDRS', 'LWRS', or 'PINH'
;
; OUTPUTS:
;	Xledge:	Position of left edge of spectrum, in pixels.
;	Xredge:	Position of right edge of spectrum, in pixels.
;	Ybot:	Position of bottom edge of spectrum, in pixels.
;	Ytop:	Position of top edge of spectrum, in pixels.
;	Xpixsize, Ypixsize:
;		X and Y pixel size in um for the input segment.
;	Xslitsize, Yslitsize:
;		X and Y size of slit in um on the detector (i.e.
;		including spectrograph magnification).
;	Xsia:	A two element vector containing the [low,high] x
;		location of the SIA table for each slit. At the moment,
;		this assumes that the SIA table is a simple rectangle.
;	Ysia:	A two element vector containing the [low,high] y
;		location of the SIA table for each slit. At the moment,
;		this assumes that the SIA table is a simple rectangle.
;	Xmask:	A two element vector containing the [low,high] x
;		location of the standard onboard mask for each slit,
;		scaled to match the full size of the detector. This will
;		return the current masks, unless the MJD keyword is used.
;	Ymask:	A two element vector containing the [low,high] y
;		location of the standard onboard mask for each slit,
;		scaled to match the full size of the detector. This will
;		return the current masks, unless the MJD keyword is used.
;
; KEYWORDS:
;	MJD:	If included, returns Xmask, Ymask, Xsia, and Ysia values
;		appropriate to this MJD rather than the current date.
;	SILENT:IF included, don't print any informational messages. This is
;		passed directly to date2mjd.pro.
;	FARF:	If present, use return ybot and ytop in FARF coordinates
;		rather than raw coordinates.
;
; PROCEDURE:
;	Straightforward.
;
; EXAMPLE:
;	To find the location of the LiF1A MDRS data:
;		IDL> locatespectrum,'LiF1A','MDRS',Xledge,Xredge,Ybot,Ytop
;
; MODIFICATION HISTORY:
; 	Originally part of STRABSORB.PRO. Created LOCATESPECTRUM on
;	 9 June 2000.
;	3 July 2000 Removed 'Segment' input. Now gets segment from 'Channel.'
;	 Changed so that all inputs are case insensitive.
;	13 September 2000 Added Xpixsize, Ypixsize.
;	8 March 2001 Added Xslitsize, Yslitsize.
;	16 March 2001 Added Xmask, Ymask, MJD.
;	20 March 2001 Added Xsia, Ysia.
;	27 April 2001 Fixed error which had ytop and ybot reversed
;	28 January 2002 Added SILENT keyword, which is simply passed to
;	 date2mjd via _extra
;	29 January 2002 Updated to latest versions of SIA tables and masks
;	8 September 2003 Added FARF keyword. Added FARF coordinates for ytop
;	 and ybot, based on values in chid??11.fit
;	19 September 2003 Fixed error in LiF2B LWRS FARF yc and yw.
;	22 December 2003 Adjusted the SIA table dates based on SIAtables.xls
;	 updates.
;TODO: Should read the values from the chid??11.fit file directly.
;-
	channel = strupcase(channelin)
	slit = strupcase(slitin)
	seg = strmid(channel,3,2)

	xmag = 1.0		;x magnification of spectrograph
	ysicmag = 1.08		;y magnification of spectrograph
	ylifmag = 1.09		;y magnification of spectrograph
	ymag = ysicmag		;Since these are so close, use only SiC value

	xslitum = [13.6,43.6,327,6]
	yslitum = [218,218,327,6]
				;slit sizes in um for HIRS, MDRS, LWRS, PINH
				; (from CU drawing)

	if not keyword_set(mjd) then mjd = date2mjd(systime(),_extra=e)

;Set the SIA table:
	xsialwrs = [[0,16383],[0,16383],$	;SiC1A, LiF1A (sialwrs1x8e)
		[0,16383],[0,16383],$		;SiC1B, LiF1B
		[0,16383],[0,16383],$		;SiC2A, LiF2A
		[0,16383],[0,16383]]		;SiC2B, LiF2B
	ysialwrs = [[16,175],[496,655],$	;SiC1A, LiF1A (sialwrs1x8e)
		[0,175],[464,639],$		;SiC1B, LiF1B
		[352,463],[624,751],$		;SiC2A, LiF2A
		[448,559],[656,799]]		;SiC2B, LiF2B
	xsiahirs = [[0,16383],[0,16383],$	;SiC1A, LiF1A (siahirs1x8d)
		[0,16383],[0,16383],$		;SiC1B, LiF1B
		[0,16383],[0,16383],$		;SiC2A, LiF2A
		[0,16383],[0,16383]]		;SiC2B, LiF2B
	ysiahirs = [[160,319],[640,799],$	;SiC1A, LiF1A (siahirs1x8d)
		[112,271],[592,767],$		;SiC1B, LiF1B
		[288,399],[560,671],$		;SiC2A, LiF2A
		[400,495],[592,703]]		;SiC2B, LiF2B
	xsiamdrs = [[0,16383],[0,16383],$	;SiC1A, LiF1A (siamdrs1x8d)
		[0,16383],[0,16383],$		;SiC1B, LiF1B
		[0,16383],[0,16383],$		;SiC2A, LiF2A
		[0,16383],[0,16383]]		;SiC2B, LiF2B
	ysiamdrs = [[288,447],[752,927],$	;SiC1A, LiF1A (siamdrs1x8d)
		[240,399],[720,895],$		;SiC1B, LiF1B
		[208,319],[464,575],$		;SiC2A, LiF2A
		[352,463],[544,655]]		;SiC2B, LiF2B

;Previous SIA table locations:
;if (mjd le 52029. and mjd gt 51697.) then begin	;6/2/00 - 4/30/01
if (mjd lt 52119.44132D and mjd ge 51681.38568D) then begin ;5/17/00 - 7/29/01
	ysiahirs = [[160,319],[640,799],$	;SiC1A, LiF1A (siahirs1x8c)
		[112,271],[608,767],$		;SiC1B, LiF1B
		[288,399],[560,671],$		;SiC2A, LiF2A
		[400,495],[592,703]]		;SiC2B, LiF2B
endif
if (mjd lt 51681.38568D) then begin	;before 5/17/00
	ysiahirs = [[160,319],[624,783],$	;SiC1A, LiF1A (siahirs1x8b)
		[128,287],[608,767],$		;SiC1B, LiF1B
		[288,399],[544,655],$		;SiC2A, LiF2A
		[400,495],[592,703]]		;SiC2B, LiF2B
endif
;if (mjd le 52029. and mjd gt 51580.) then begin	;2/6/00 - 4/30/01
if (mjd lt 52080.32341D and mjd ge 51556.36777D) then begin ;1/13/00 - 6/20/01
	ysiamdrs = [[288,447],[768,927],$	;SiC1A, LiF1A (siamdrs1x8c)
		[240,399],[736,895],$		;SiC1B, LiF1B
		[208,319],[464,575],$		;SiC2A, LiF2A
		[352,463],[544,655]]		;SiC2B, LiF2B
endif
;if (mjd le 51580. and mjd gt 51500.) then begin	;11/18/99 - 2/6/00
if (mjd lt 51556.36777D) then begin	;before 1/13/00
	ysiamdrs = [[288,447],[768,927],$	;SiC1A, LiF1A (siamdrs1x8b)
		[240,399],[736,895],$		;SiC1B, LiF1B
		[208,319],[464,575],$		;SiC2A, LiF2A
		[224,335],[544,655]]		;SiC2B, LiF2B
endif
;if (mjd le 51500.) then begin			;before 11/18/99
;	ysiamdrs = [[304,463],[784,943],$	;SiC1A, LiF1A (siamdrs1x8a)
;		[272,431],[752,911],$		;SiC1B, LiF1B
;		[256,367],[512,623],$		;SiC2A, LiF2A
;		[368,479],[576,687]]		;SiC2B, LiF2B
;endif
;if (mjd le 52029. and mjd gt 51682.) then begin	;5/18/00 - 4/30/01
if (mjd lt 52030.62749D and mjd ge 51682.01259D) then begin ;5/18/00 - 5/1/01
	ysialwrs = [[16,175],[496,655],$	;SiC1A, LiF1A (sialwrs1x8d)
		[0,175],[480,639],$		;SiC1B, LiF1B
		[352,463],[624,735],$		;SiC2A, LiF2A
		[448,559],[672,783]]		;SiC2B, LiF2B
endif
;if (mjd le 51682. and mjd gt 51610.) then begin	;3/7/00 - 5/18/00
if (mjd lt 51682.01259D and mjd ge 51607.30156D) then begin ;3/4/00 - 5/18/00
	ysialwrs = [[16,175],[480,639],$	;SiC1A, LiF1A (sialwrs1x8c)
		[0,175],[464,623],$		;SiC1B, LiF1B
		[352,463],[624,735],$		;SiC2A, LiF2A
		[448,559],[672,783]]		;SiC2B, LiF2B
endif
;if (mjd le 51610. and mjd gt 51470.) then begin	;10/19/99 - 3/7/00 
if (mjd lt 51607.30156D) then begin	;before 3/4/00 
	ysialwrs = [[16,175],[480,639],$	;SiC1A, LiF1A (sialwrs1x8b)
		[0,175],[464,623],$		;SiC1B, LiF1B
		[384,495],[624,735],$		;SiC2A, LiF2A
		[480,591],[656,767]]		;SiC2B, LiF2B
endif
;if (mjd le 51470.) then begin	;before 10/19/99
;	;note that there is some overlap between a and b
;	ysialwrs = [[80,239],[528,687],$	;SiC1A, LiF1A (sialwrs1x8a)
;		[32,191],[512,671],$		;SiC1B, LiF1B
;		[400,511],[672,783],$		;SiC2A, LiF2A
;		[496,607],[704,815]]		;SiC2B, LiF2B
;endif

;Set the masks (as of MOCR 883, 11/19/01). Multiply 16 for pixel numbers:

	xmsk1a = [[0,1023],[0,1023],$			;AIC ASC
		[0,1023],[62,955],[62,955],[62,955],$	;SiC SiCH SiCM SiCL
		[0,1023],[62,955],[62,955],[62,955]]	;LiF LiFH LiFM LiFL
	ymsk1a = [[0,63],[27,30],$			;AIC ASC
		[0,26],[10,19],[18,27],[1,10],$		;SiC SiCH SiCM SiCL
		[31,63],[40,49],[47,57],[31,40]]	;LiF LiFH LiFM LiFL

	xmsk1b = [[0,1023],[0,1023],$			;AIC ASC
		[0,1023],[62,955],[62,955],[62,955],$	;SiC SiCH SiCM SiCL
		[0,1023],[62,955],[62,955],[62,955]]	;LiF LiFH LiFM LiFL
	ymsk1b = [[0,63],[25,28],$			;AIC ASC
		[0,24],[7,16],[15,24],[0,10],$		;SiC SiCH SiCM SiCL
		[29,63],[37,47],[45,55],[29,39]]	;LiF LiFH LiFM LiFL

	xmsk2a = [[0,1023],[0,1023],$			;AIC ASC
		[0,1023],[56,961],[56,961],[56,961],$	;SiC SiCH SiCM SiCL
		[0,1023],[56,961],[56,961],[56,961]]	;LiF LiFH LiFM LiFL
	ymsk2a = [[0,63],[11,12],$			;AIC ASC
		[0,27],[18,24],[13,19],[22,28],$	;SiC SiCH SiCM SiCL
		[31,63],[35,41],[29,35],[39,46]]	;LiF LiFH LiFM LiFL

	xmsk2b = [[0,1023],[0,1023],$			;AIC ASC
		[0,1023],[62,968],[62,968],[62,968],$	;SiC SiCH SiCM SiCL
		[0,1023],[62,968],[62,968],[62,968]]	;LiF LiFH LiFM LiFL
	ymsk2b = [[0,63],[18,20],$			;AIC ASC
		[21,34],[25,30],[22,28],[28,34],$	;SiC SiCH SiCM SiCL
		[35,63],[37,43],[34,40],[41,49]]	;LiF LiFH LiFM LiFL

		;these are of the form [lo,hi] for AIC, ASC, SiC, SiC-HIRS,
		; SiC-MDRS, SiC-LWRS, LiF, LiF-HIRS, LiF-MDRS, LiF-LWRS

	;Previous mask locations:
	if (mjd lt date2mjd("NOV 19 2001", _extra=e)) then begin
				;MOCR 883, implemented 11/19/01
		ymsk1a = [[0,63],[27,30],$		;AIC ASC
			[0,26],[9,18],[16,25],[1,10],$	;SiC SiCH SiCM SiCL
			[31,63],[39,48],[47,56],[31,40]];LiF LiFH LiFM LiFL
		ymsk2a = [[0,63],[11,12],$		;AIC ASC
			[0,27],[19,23],[13,18],[24,29],$;SiC SiCH SiCM SiCL
			[31,63],[36,40],[30,35],[41,46]];LiF LiFH LiFM LiFL
		ymsk1b = [[0,63],[25,28],$		;AIC ASC
			[0,24],[8,17],[15,24],[1,9],$	;SiC SiCH SiCM SiCL
			[29,63],[37,46],[46,55],[29,38]];LiF LiFH LiFM LiFL
		ymsk2b = [[0,63],[18,20],$		;AIC ASC
			[21,34],[26,30],[21,25],[30,34],$;SiC SiCH SiCM SiCL
			[35,63],[38,42],[35,39],[42,48]];LiF LiFH LiFM LiFL
	endif

	if (mjd lt date2mjd("OCT 17 2001", _extra=e)) then ymsk2a[*,1] = [11,13]
				;MOCR 850, implemented 10/17/01

	if (mjd lt date2mjd("APR 19 2000", _extra=e)) then ymsk2a[*,1] = [28,30]
			 	; MOCR 569 implemented 4/19/2000

;Convert mask values to pixel values:
	xmsk1a[0,*] = xmsk1a[0,*] * 16
	xmsk1a[1,*] = xmsk1a[1,*] * 16 + 15
	ymsk1a[0,*] = ymsk1a[0,*] * 16
	ymsk1a[1,*] = ymsk1a[1,*] * 16 + 15
	xmsk1b[0,*] = xmsk1b[0,*] * 16
	xmsk1b[1,*] = xmsk1b[1,*] * 16 + 15
	ymsk1b[0,*] = ymsk1b[0,*] * 16
	ymsk1b[1,*] = ymsk1b[1,*] * 16 + 15
	xmsk2a[0,*] = xmsk2a[0,*] * 16
	xmsk2a[1,*] = xmsk2a[1,*] * 16 + 15
	ymsk2a[0,*] = ymsk2a[0,*] * 16
	ymsk2a[1,*] = ymsk2a[1,*] * 16 + 15
	xmsk2b[0,*] = xmsk2b[0,*] * 16
	xmsk2b[1,*] = xmsk2b[1,*] * 16 + 15
	ymsk2b[0,*] = ymsk2b[0,*] * 16
	ymsk2b[1,*] = ymsk2b[1,*] * 16 + 15

	case seg of
		'1A': begin
			xledge = 1038 
			xredge = 15230
			xpixsize = 6.0
			ypixsize = 9.11 
		end
		'1B': begin
			xledge = 1100
			xredge = 15210
			xpixsize = 6.0
			ypixsize = 9.08 
		end
		'2A': begin
			xledge = 1000 
			xredge = 15300 
			xpixsize = 6.0
			ypixsize = 14.75 
		end
		'2B': begin
			xledge = 1100
			xredge = 15400
			xpixsize = 6.0
			ypixsize = 16.31
		end
		else: begin
			print,'Invalid segment name!'
			stop
		end
	endcase

;Center and half width of y extent of spectra and mask locations:
	case slit of
		'LWRS': begin		;(yc and yw measured from I8180102)
			xslitsize = xslitum[2] * xmag
			yslitsize = yslitum[2] * ymag
			case channel of
				'SIC1A': begin
					if keyword_set(farf) then begin
						yc = 76
						yw = 64
					endif else begin
						yc = 96
						yw = 80
					endelse
					xmask = xmsk1a[*,5]
					ymask = ymsk1a[*,5]
					xsia = xsialwrs[*,0]
					ysia = ysialwrs[*,0]
				end
				'LIF1A': begin
					if keyword_set(farf) then begin
						yc = 552
						yw = 72
					endif else begin
						yc = 576
						yw = 80
					endelse
					xmask = xmsk1a[*,9]
					ymask = ymsk1a[*,9]
					xsia = xsialwrs[*,1]
					ysia = ysialwrs[*,1]
				end
				'SIC1B': begin
					if keyword_set(farf) then begin
						yc = 72
						yw = 64
					endif else begin
						yc = 60
						yw = 56
					endelse
					xmask = xmsk1b[*,5]
					ymask = ymsk1b[*,5]
					xsia = xsialwrs[*,2]
					ysia = ysialwrs[*,2]
				end
				'LIF1B': begin
					if keyword_set(farf) then begin
						yc = 568
						yw = 88
					endif else begin
						yc = 556
						yw = 92
					endelse
					xmask = xmsk1b[*,9]
					ymask = ymsk1b[*,9]
					xsia = xsialwrs[*,3]
					ysia = ysialwrs[*,3]
				end
				'SIC2A': begin
					if keyword_set(farf) then begin
						yc = 416
						yw = 64
					endif else begin
						yc = 416
						yw = 64
					endelse
					xmask = xmsk2a[*,5]
					ymask = ymsk2a[*,5]
					xsia = xsialwrs[*,4]
					ysia = ysialwrs[*,4]
				end
				'LIF2A': begin
					if keyword_set(farf) then begin
						yc = 808
						yw = 72
					endif else begin
						yc = 696
						yw = 56
					endelse
					xmask = xmsk2a[*,9]
					ymask = ymsk2a[*,9]
					xsia = xsialwrs[*,5]
					ysia = ysialwrs[*,5]
				end
				'SIC2B': begin
					if keyword_set(farf) then begin
						yc = 432
						yw = 64
					endif else begin
						yc = 516
						yw = 44
					endelse
					xmask = xmsk2b[*,5]
					ymask = ymsk2b[*,5]
					xsia = xsialwrs[*,6]
					ysia = ysialwrs[*,6]
				end
				'LIF2B': begin
					if keyword_set(farf) then begin
						yc = 808
						yw = 80
					endif else begin
						yc = 724
						yw = 80
					endelse
					xmask = xmsk2b[*,9]
					ymask = ymsk2b[*,9]
					xsia = xsialwrs[*,7]
					ysia = ysialwrs[*,7]
				end
			endcase
		end
		'MDRS': begin		;measured from M1031104
			xslitsize = xslitum[1] * xmag
			yslitsize = yslitum[1] * ymag
			case channel of
				'SIC1A': begin
					if keyword_set(farf) then begin
						yc = 328
						yw = 64
					endif else begin
						yc = 364
						yw = 80
					endelse
					xmask = xmsk1a[*,4]
					ymask = ymsk1a[*,4]
					xsia = xsiamdrs[*,0]
					ysia = ysiamdrs[*,0]
				end
				'LIF1A': begin
					if keyword_set(farf) then begin
						yc = 816
						yw = 72
					endif else begin
						yc = 844
						yw = 80
					endelse
					xmask = xmsk1a[*,8]
					ymask = ymsk1a[*,8]
					xsia = xsiamdrs[*,1]
					ysia = ysiamdrs[*,1]
				end
				'SIC1B': begin
					if keyword_set(farf) then begin
						yc = 328
						yw = 64
					endif else begin
						yc = 316
						yw = 80
					endelse
					xmask = xmsk1b[*,4]
					ymask = ymsk1b[*,4]
					xsia = xsiamdrs[*,2]
					ysia = ysiamdrs[*,2]
				end
				'LIF1B': begin
					if keyword_set(farf) then begin
						yc = 824
						yw = 72
					endif else begin
						yc = 812
						yw = 80
					endelse
					xmask = xmsk1b[*,8]
					ymask = ymsk1b[*,8]
					xsia = xsiamdrs[*,3]
					ysia = ysiamdrs[*,3]
				end
				'SIC2A': begin
					if keyword_set(farf) then begin
						yc = 168
						yw = 56
					endif else begin
						yc = 260
						yw = 60
					endelse
					xmask = xmsk2a[*,4]
					ymask = ymsk2a[*,4]
					xsia = xsiamdrs[*,4]
					ysia = ysiamdrs[*,4]
				end
				'LIF2A': begin
					if keyword_set(farf) then begin
						yc = 568
						yw = 64
					endif else begin
						yc = 516
						yw = 60
					endelse
					xmask = xmsk2a[*,8]
					ymask = ymsk2a[*,8]
					xsia = xsiamdrs[*,5]
					ysia = ysiamdrs[*,5]
				end
				'SIC2B': begin
					if keyword_set(farf) then begin
						yc = 200
						yw = 56
					endif else begin
						yc = 404
						yw = 60
					endelse
					xmask = xmsk2b[*,4]
					ymask = ymsk2b[*,4]
					xsia = xsiamdrs[*,6]
					ysia = ysiamdrs[*,6]
				end
				'LIF2B': begin
					if keyword_set(farf) then begin
						yc = 576
						yw = 72
					endif else begin
						yc = 596
						yw = 72
					endelse
					xmask = xmsk2b[*,8]
					ymask = ymsk2b[*,8]
					xsia = xsiamdrs[*,7]
					ysia = ysiamdrs[*,7]
				end
			endcase
		end
		'HIRS': begin		;side 2 from S5140203
			xslitsize = xslitum[0] * xmag
			yslitsize = yslitum[0] * ymag
			case channel of
				'SIC1A': begin
					if keyword_set(farf) then begin
						yc = 200
						yw = 64
					endif else begin
						yc = 240
						yw = 80
					endelse
					xmask = xmsk1a[*,3]
					ymask = ymsk1a[*,3]
					xsia = xsiahirs[*,0]
					ysia = ysiahirs[*,0]
				end
				'LIF1A': begin
					if keyword_set(farf) then begin
						yc = 688
						yw = 72
					endif else begin
						yc = 704
						yw = 80
					endelse
					xmask = xmsk1a[*,7]
					ymask = ymsk1a[*,7]
					xsia = xsiahirs[*,1]
					ysia = ysiahirs[*,1]
				end
				'SIC1B': begin
					if keyword_set(farf) then begin
						yc = 192
						yw = 72
					endif else begin
						yc = 192
						yw = 64
					endelse
					xmask = xmsk1b[*,3]
					ymask = ymsk1b[*,3]
					xsia = xsiahirs[*,2]
					ysia = ysiahirs[*,2]
				end
				'LIF1B': begin
					if keyword_set(farf) then begin
						yc = 696
						yw = 80
					endif else begin
						yc = 688
						yw = 92
					endelse
					xmask = xmsk1b[*,7]
					ymask = ymsk1b[*,7]
					xsia = xsiahirs[*,3]
					ysia = ysiahirs[*,3]
				end
				'SIC2A': begin
					if keyword_set(farf) then begin
						yc = 296
						yw = 56
					endif else begin
						yc = 340
						yw = 64
					endelse
					xmask = xmsk2a[*,3]
					ymask = ymsk2a[*,3]
					xsia = xsiahirs[*,4]
					ysia = ysiahirs[*,4]
				end
				'LIF2A': begin
					if keyword_set(farf) then begin
						yc = 696
						yw = 64
					endif else begin
						yc = 612
						yw = 64
					endelse
					xmask = xmsk2a[*,7]
					ymask = ymsk2a[*,7]
					xsia = xsiahirs[*,5]
					ysia = ysiahirs[*,5]
				end
				'SIC2B': begin
					if keyword_set(farf) then begin
						yc = 306
						yw = 56
					endif else begin
						yc = 444
						yw = 56
					endelse
					xmask = xmsk2b[*,3]
					ymask = ymsk2b[*,3]
					xsia = xsiahirs[*,6]
					ysia = ysiahirs[*,6]
				end
				'LIF2B': begin
					if keyword_set(farf) then begin
						yc = 696
						yw = 72
					endif else begin
						yc = 644
						yw = 64
					endelse
					xmask = xmsk2b[*,7]
					ymask = ymsk2b[*,7]
					xsia = xsiahirs[*,7]
					ysia = ysiahirs[*,7]
				end
			endcase
		end
		'PINH': begin
			xslitsize = xslitum[3] * xmag
			yslitsize = yslitum[3] * ymag
			print,'No position data for this slit'
			stop
		end
	endcase

	ybot = yc - yw
	ytop = yc + yw
	

end
