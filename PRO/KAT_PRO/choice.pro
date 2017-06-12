function choice, selection,valid_selections,statement,quit_flags=quit_flags,exit_statement=exit_statement

;   This program tests the validity of a user response. The program
;   will test if the response is a string or a valid number
;
;   Input:
;   selection = variable containing user response
;   valid_selections = an array containing valid responses (e.g. [0,1])
;     statement = statement containing user choices (e.g. '0=yes; 1=no :')
;     String selections are case insensitive.
;   quit_flags = characters or numbers that will allow the user to 
;                prematurely escape from the choice program.
;                For example, if quit_flags='q', then the user has the  
;                option of entering 'q' instead of the valid_selections to
;                quit the program.
;                DEFAULT is ['q','qq','quit','exit'], case insensitive 
;   exit_statement = Specified message to print out if user prematurely escapes from program.
;
;   Output:
;   ***NOTE: All output will be of the same type as valid_selections[0]
;   selection = a valid selection response
;   exit_flag = if output is equal to -1337, user desires exiting program
;
;   example: test=choice(selection,['e','p'],'Please choose e or p: ')
;   test=choice(selection,[0,1,2],'Please choose 0, 1, or 2: ',exit_statement='Exiting...')
;
;   Created by Dr. Kat Barger, last modified 11/2013
;
;   Modifications:
;   -Now case insensitive -Kat
;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Define the default format for the output to have. 
output_type=size(valid_selections[0],/type)
   
valid_selections=strcompress(fix(valid_selections,type=7),/re)

if NOT keyword_set(quit_flags) THEN quit_flags=['q','qq','quit','exit'] $
   else quit_flags=strarr(n_elements(quit_flags))+string(quit_flags)  
       ;Convert quit_flags to string array so that numbers can also be passed. 

         exit_flag=0

          selection=''
          read, selection, prompt=statement
           selection=fix(selection,type=7) ;Convert to string

            if total(strmatch(quit_flags,selection,/fold_case)) ge 1 then exit_flag = '-1337'
            if total(strmatch(valid_selections,selection,/fold_case)) eq 0 then selection = '-999'
         while (selection eq '-999') and (exit_flag ne '-1337') do begin
               print,"******************************************"
               print,"*** Please try again selection invalid ***" 
               print,"******************************************"
            selection=''
            read,selection,prompt=statement
            selection=fix(selection,type=7) ;Convert to string

               if total(strmatch(quit_flags,selection,/fold_case)) then exit_flag = '-1337'
               if total(strmatch(valid_selections,selection,/fold_case)) eq 0 then selection= '-999'
         endwhile  

   if (keyword_set(exit_statement) eq 0) AND (exit_flag eq '-1337') then begin
      return,fix(exit_flag,type=output_type); ;Return exit_flag, -1337, as same type as valid_selections[0]
   endif else if (keyword_set(exit_statement)) AND (exit_flag eq '-1337') then begin 
         print,"************************"
         print,exit_statement
         print,"************************"
      return, fix(exit_flag,type=output_type) ;Return exit_flag, -1337, as same type as valid_selections[0]
   endif else return,fix(selection,type=output_type); ;Return user's selection as same type as valid_selections[0]

end