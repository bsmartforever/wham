function choice, selection,valid_selections,statement

;   This program tests the validity of a user response. The program
;   will test if the response is a string or a valid number
;
;   selection = variable containing user response
;   valid_selections = an array containing valid responses (e.g. [0,1])
;   statement = statement containing user choices (e.g. '0=yes; 1=no :')
   
         exit_flag=0

         test_num_selection=validate_numeric(selection)
         if test_num_selection eq 0 then begin
            if (selection eq 'q') or (selection eq 'Q') or (selection eq 'qq') or (selection eq 'QQ') or $
               (selection eq 'quit') or (selection eq 'Quit') or (selection eq 'exit') or $ 
               (selection eq 'Exit') then exit_flag = 1 else selection=-999
         endif 
         selection=fix(selection,type=4)
               if (selection ne valid_selections(0)) and (selection ne valid_selections(1)) then selection=-999 
         while (selection eq -999) do begin
               print,"******************************************"
               print,"*** Please try again selection invalid ***" 
               print,"******************************************"
            selection=''
            read, statement, selection
               test_num_selection=validate_numeric(selection)
               if test_num_selection eq 0 then selection=-999
               selection=fix(selection,type=4)
pause
               if (selection ne valid_selections(0)) and (selection ne valid_selections(1)) then selection=-999 
         endwhile  


return,selection;

end