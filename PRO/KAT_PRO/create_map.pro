pro create_map,dates,dir=dir,extension=extension,line=line,$
    v_min=v_min,v_max=v_max,vbin=vbin,$
    block_min=block_min,block_max=block_max,$
    quiet=quiet

; Purpose:
;   Creates a map data structure with gridded in glon and glat based on pre established blocks.
;   User specifies which preexisting blocks to use to define this grid and the desired output velocity array.
;
; Input:
;   date - date array. Assumes integers, e.g., [101201,110102,110928]
;   dir  - directory of data at the date directory locations, e.g., '$HOME/WHAM/Bridge/data/nii/'
;           Assumes current directory by default.
;   line - basically the filter name stored in the header files, e.g., 'ha', 'nii', 'sii', etc.
;           Assumes 'ha' by default.
;   extension - Fits extention to pull. Assumes procspec by default.
;   v_min/v_max - minimum/maximum velocity of the vel variable in the map structure Default -500/500 km/s. 
;   vbin - velocity spacing of the vel variable in the map structure. Default 2 km/s.
;   block_min/max - sets the min and max block numbers to consider. Default is 0 to 10000.
;         e.g., for the smc blocks, set to 2000 and 2999
;
; Output:
;   map_data.sav - contains a structure named map_data with the vel,data,var,glon,glat tags
;
; Dependences:
;   program expects pointings.dat, linear_interpol_err.pro, and whamsao 
;   program cleans dataset by removing sightlines 0.55 degrees and closer to stars Mv=6 and b brighter
;   pointings file, e.g., pointings.dat. Must set the location of that below.
;
; E.g., create_map,121217,ext='vlsr_atm'
;
; Created by Dr. Kat Barger 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This just gets rid of an annoying IDL error message, can be removed from the code without causing problems
!except=0

pointings_file='$HOME/PRO/KAT_PRO/pointings.dat'

;define output file names:
   ;Includes vel,data,var,glon,glat
   filename_data='map_data.sav'  
   filename_num_observations='observations_per_sightline.sav'

;Enter directories of interest, only the dates
dates=strcompress(string(dates),/remove_all)
if NOT keyword_set(dir) then dir='.'
dir_path=validate_directory(dir)

;line name for path
if (NOT keyword_set(line)) then line='ha'

if (NOT keyword_set(extension)) then extension='procspec'
struc_extend=extension

if (NOT keyword_set(v_min)) then v_min=-500.
if (NOT keyword_set(v_max)) then v_max=500.
if (NOT keyword_set(vbin)) then vbin=2

;Define your velocity array
;For this example, I'm setting up my velocity arrays to be in 2 km/s intervals from 0 to 450 km/s.
;Data set is linearly interpolated to new velocity binning and the corresponding errors are 
;recalculated for this binning using linear_interpol_err.pro. 
;vbin=spacing between velocity elements, e.g., 2 km/s
vel_arr=makearr((v_max-v_min)/vbin,v_min,v_max)

;Specify which blocks you want to use to define the girding of your map. 
;This defines the Glat and Glon for each grid point of your map,
;This information comes from pointings.dat which lists
;glon glat block_number for all survey and Magellanic blocks.
readcol,pointings_file,glon,glat,block
if (NOT keyword_set(block_min)) then block_min = 0
if (NOT keyword_set(block_max)) then block_max = 10000
good_blocks=where((block gt block_min) and (block lt block_max),num_new)
glon=glon(good_blocks)
glat=glat(good_blocks)
block=block(good_blocks)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;read in first directory to create a variable structure to replicate for including other directories.
readblock,dir_path+dates[0]+'/'+line+'/combo/b*',map_data,ftsext=struc_extend,/ext,/quiet
        print,'directory ', dir[0],' read'
for i=1, n_elements(dir)-1 do begin
	readblock,dir_path+dir(i)+'/'+line+'/combo/b*',temp,ftsext=struc_extend,/ext,/quiet
        temp_map_data=replicate(temp(0),n_elements(temp)+n_elements(map_data))
        temp_map_data(0:n_elements(temp)-1)=temp
        temp_map_data(n_elements(temp):n_elements(temp)+n_elements(map_data)-1)=map_data
        map_data=temp_map_data
        print,'directory ', dir[i],' read'
endfor

if (NOT keyword_set(quiet)) then begin
  print,''
  print,'*** Removing files with bad CCD temperatures ***'
  print,''
endif

map_data=map_data(where(map_data.ccdtemp lt -100.)) 

;User cover to trim edges of spectra before averaging them together.
;e.g., cover=[5,90]
cover=[0,n_elements(map_data[0].vel)-1]

temp=replicate({name:map_data[0].name,$
                vel:map_data[0].vel(cover(0):cover(1)),$
                data:map_data[0].data(cover(0):cover(1)),$  
                var:map_data[0].var(cover(0):cover(1)), $
                glon:map_data[0].glon,$
                glat:map_data[0].glat $
                },n_elements(map_data))

temp.name=map_data.name
temp.vel=map_data.vel(cover(0):cover(1))
temp.data=map_data.data(cover(0):cover(1))
temp.var=map_data.var(cover(0):cover(1))
temp.glon=map_data.glon
temp.glat=map_data.glat


      old_map=replicate({vel:vel_arr,data:fltarr(n_elements(vel_arr)),var:fltarr(n_elements(vel_arr)),glon:1.0,glat:1.0},n_elements(map_data))
      old_map.glon=map_data.glon
      old_map.glat=map_data.glat
      new_map=replicate({name:' ',vel:vel_arr,data:fltarr(n_elements(vel_arr)),var:fltarr(n_elements(vel_arr)),glon:1.0,glat:1.0},num_new)  
      new_map.glon=glon
      new_map.glat=glat

      for i=0,n_elements(map_data)-1 do begin
          old_map_data=interpol(map_data(i).data,map_data(i).vel,vel_arr)
          zero=where((vel_arr lt min(map_data(i).vel)) or (vel_arr gt max(map_data(i).vel)))
          old_map_data(zero)=0
          ;Calculate error for the specified velocity binning. 
          error=(linear_interpol_err(map_data(i).vel,vel_arr,map_data(i).data,sqrt(map_data(i).var)))^2.0        
          old_map(i).data=old_map_data
          old_map(i).var=error
          ;plot,vel_arr,old_map_data
          ;oplot,map_data(i).vel,map_data(i).data,color=fsc_color('orchid')
          ;oplot,old_map(i).vel,old_map(i).data,color=fsc_color('blue')
      endfor

      ;int_map_data=intmap(map_data,vmin=v_min,vmax=v_max)*1.0/22.8
      ;int_old_map=intmap(old_map,vmin=v_min,vmax=v_max)*1.0/22.8

      ;plot,intmap(map_data,vmin=v_min,vmax=v_max)*1.0/22.8,xrange=[490,510],xstyle=1 
      ;oplot,intmap(old_map,vmin=100,vmax=v_max)*1.0/22.8,color=fsc_color('orchid')


      ;read,'Kill here: ',die

      num_ave=fltarr(n_elements(new_map))
      for i=0L, num_new-1 do begin
          ;count = observations per sightline near 0.6 degrees, saved to num_ave array
          subindex=spectnear(map_data,new_map(i).glon,new_map(i).glat,0.2,count)
          ;print,i,new_map(i).glon,new_map(i).glat,count
          ;if count ne 0 then print,map_data(subindex).glon,map_data(subindex).glat
          num_ave(i)=count
          count_arr=fltarr(n_elements(vel_arr))
          if count ne 0 then begin 
             for j=0,count-1 do begin
                 new_map(i).data=new_map(i).data+old_map(subindex(j)).data
                 new_map(i).var=new_map(i).var+old_map(subindex(j)).var
                 count_arr=count_arr+(old_map(subindex(j)).data ne 0)
             endfor
             divide_loc=where(count_arr ne 0)
             new_map(i).data(divide_loc)=new_map(i).data(divide_loc)/count_arr(divide_loc)
             new_map(i).var(divide_loc)=new_map(i).var(divide_loc)/count_arr(divide_loc)^2.
             ;plot,map_data(subindex).vel,map_data(subindex).data
             ;oplot,new_map(i).vel,new_map(i).data,color=fsc_color('orchid')
             ;read,"kill here: ",die
          endif
      endfor

      total_data=total(new_map.data,1)
      good=where(total_data ne 0)
      new_map=new_map[good]

;removing pointings with bright stars of Mv=6 and brighter within 0.55 degrees of pointing
;The WHAM survey used 6 mag and 0.55 degrees as the cutoff
;removing stars

if (NOT keyword_set(quiet)) then begin
  print,''
  print,'*** Removing sight lines with bright foreground stars ***'
  print,''
endif

stars_flag=stars(new_map.glon,new_map.glat,/quiet)
new_map=new_map(where(stars_flag.flag eq 0))

     ; num_observations=num_ave(where(stars_flag eq 0))

      ;save,num_observations,filename=filename_num_observations
      ;save,new_map,filename='new_map.sav'


map_data=new_map
save,map_data,filename='map_data.sav'
if (NOT keyword_set(quiet)) then begin
  print,''
  print,'*** Saving map to map_data.sav ***'
  print,''
endif 

end