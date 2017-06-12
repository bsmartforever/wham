pro rm_stars, str, int, index, radius=radius, faint=faint, quiet=quiet, help=help

; Purpose: 
;
; 	To remove sightlines in a map structure and intensity array that are 
;	contaminated by bright stellar emission and absorption.  
;
; Input:
;
; 	str    - Map structure containing glon and glat at minumum. 
;   int    - Intensity array of same length as str
;   index  - Variable name that will hold the index array of the 
;			 locations in str that don't have stars.
;   radius - Radial distance around glon and glat 
;            to search for a bright star. 
;            [DEFAULT]: radius = 0.55 - the WHAM beam size.
;
; Optional:
; 
;   faint  - The mv limit to search for bright stars. Flags when star with 
;            visual magnitudes brighter than the limit are present.  
;           [DEFAULT]: faint = 6.0 - value used in WHAM north survey.
;   quiet  - [OPTIONAL] If quiet = 1, then skip terminal output.
;
; Output:
;
;	str    - Cleaned map structure, with sightlines near stars removed.
;	int    - Cleaned intensity structure if provided.
;	index  - Index array of good locations in str that don't have stars.
;           
; Example:
;	rm_stars,str
;
;	rm_stars,str,int,index
;		Will provide an index array even if the int isn't defined.
;
; Dependencies:
;	stars.pro - flags sightlines contaminated by stars.
;	whamsao   - a WHAM command line function.
;
; By Dr. Kat Barger in November 2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;The WHAM survey used 6 mag and 0.55 degrees as the cutoff
if (NOT keyword_set(faint)) then faint=6. 
if (NOT keyword_set(radius)) then star_radius=sqrt(1./!dpi) else star_radius=radius

if (NOT keyword_set(quiet)) then print,'Searching for bright stars...'
if keyword_set(help) then print,'Flag: 0=no star; 1=bright star'

num=n_elements(str)

tmp=stars(str.glon, str.glat, radius=radius, faint=faint, /quiet)

index=where(tmp.flag eq 0)

if n_elements(int) eq num then int=int[index] 
str=str[index]

end