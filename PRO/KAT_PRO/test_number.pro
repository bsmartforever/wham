function test_number, number,statement,exit_statement,not_equal=not_equal,range=range

;   This program tests the validity of a user response. The program
;   will test if the response is a number
;
;   Input:
;   number = variable containing user response (can be an array of responses)
;   statement = statement informing user what to enter
;               (e.g. 'What velocity range would you like the background to be over?')
;
;   Output:
;   number = a valid number response
;   exit_flag = if output is equal to -1337, user desires exiting program

         exit_flag=0

         ;if n_elements(number) eq 1 then number=['number']

         test_num_number=validate_numeric(number)
         if test_num_number eq 0 then begin
            if (number[0] eq 'q') or (number[0] eq 'Q') or (number[0] eq 'qq') or (number[0] eq 'QQ') or $
               (number[0] eq 'quit') or (number[0] eq 'Quit') or (number[0] eq 'exit') or $ 
               (number[0] eq 'Exit') then exit_flag = -1337
            number=fltarr(n_elements(number))-999
         endif 
         number=fix(number,type=4)
         IF keyword_set(not_equal) and (n_elements(number) ne 1) Then begin
            if number[0] eq number[1] then begin
               number=fltarr(n_elements(number))-999
               print,"*****************************************"
               print,"***     Equal numbers are invalid     ***" 
            endif
         Endif
         IF keyword_set(range) and (n_elements(number) ne 1) then begin
            IF ((number[0] lt range[0]) or (number[0] gt range[1]) or (number[1] lt range[0]) or (number[1] gt range[1])) Then begin
               print,"*****************************************"
               print,"***           Invalid Range           ***" 
               print,"*****************************************"
               print,strcompress('Valid numbers are between '+string(range[0])+' and '+string(range[1]))
               number=fltarr(n_elements(number))-999
             ENDIF
         Endif
         while (number[0] eq -999) and (exit_flag ne -1337) do begin
               print,"*****************************************"
               print,"***  Please try again number invalid  ***" 
               print,"*****************************************"
            number=string(number)
            print,statement
            if keyword_set(range) and (n_elements(number) ne 1) then begin
               temp1=''
               temp2=''
               read,'Range [lower bound]: ', temp1
               read,'Range [upper bound]: ', temp2
               number[0]=temp1
               number[1]=temp2
            endif else read, number
               test_num_number=validate_numeric(number)
         if test_num_number eq 0 then begin
            if (number[0] eq 'q') or (number[0] eq 'Q') or (number[0] eq 'qq') or (number[0] eq 'QQ') or $
               (number[0] eq 'quit') or (number[0] eq 'Quit') or (number[0] eq 'exit') or $ 
               (number[0] eq 'Exit') then exit_flag = -1337
            number=fltarr(n_elements(number))-999
         endif 
               number=fix(number,type=4)
         IF keyword_set(not_equal) and (n_elements(number) ne 1) Then begin
            if number[0] eq number[1] then begin
               number=fltarr(n_elements(number))-999
               print,"*****************************************"
               print,"***     Equal numbers are invalid     ***" 
            endif
         Endif

        IF keyword_set(range) and (n_elements(number) ne 1) then begin
          IF ((number[0] lt range[0]) or (number[0] gt range[1]) or (number[1] lt range[0]) or (number[1] gt range[1])) Then begin
               print,"*****************************************"
               print,"***           Invalid Range           ***" 
               print,"*****************************************"
               print,'Valid numbers are between '+string(range[0])+' and '+string(range[1])
               number=fltarr(n_elements(number))-999
           endif
        Endif

        endwhile  

        IF keyword_set(range) and (n_elements(number) ne 1) then begin
          if (number[0] gt number[1]) Then begin
            temp=number[0]
            number[0]=number[1]
            number[1]=temp
          endif
        Endif

   if exit_flag eq 0 then begin
      return,number; 
   endif else begin 
         print,"************************"
         print,exit_statement
         print,"************************"
      return, exit_flag
   endelse

end