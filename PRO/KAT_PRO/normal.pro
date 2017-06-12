function normal, array

array=float(array)
norm_arr=(array-min(array))/(max(array)-min(array))


   return, norm_arr;

end