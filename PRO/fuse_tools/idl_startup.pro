;
;   IDL_STARTUP.PRO
;   FUSE Data Analysis Facility
;
;   Useage: The IDL_STARTUP procedure can be used to initialize the IDL
;		environment (i.e. procedures, variables, system variables
;		,etc) prior to starting each IDL session.
;
;   Logical: When you start IDL, it automatically looks for a logical
;		definition for IDL_STARTUP. The logical points to the
;		procedure containing the items discussed under the section
;		Useage (see above). To determine the name of your local
;		IDL_STARTUP procedure, enter:
;
;		    % setenv | grep IDL_STARTUP
;
;   How to use: The IDL_STARTUP logical can be defined by:
;
;		    % setenv  IDL_STARTUP    /disk1/fuse/idl_startup.pro 
;
;  Special note: Since each site which running IDL will usually have
;	an existing customized IDL_STARTUP.PRO file, we suggest that
;	remote installations should follow these steps:
;	1) Compare your local IDL_STARTUP procedure against the 
;		FUSE DAF version (this file). Remove any
;		conflicts between the two procedures that would
;		cause problems.
;	2) Append the FUSE DAF version (this file) to the end of your
;		local IDL_STARTUP procedure. Be sure that the logic
;		in your local procedure still works properly (i.e. check
;		for branch statements, etc that would not allow the
;		FUSE DAF commands to be executed properly).
;	3) Check the OPTIONAL section of the FUSE DAF version. This 
;		section contains suggested settings for existing IDL
;		system variables. You may choose not to use these
;		settings.
;
;	3/27/96	jkf/acc		- increased .size(code/data areas to 65000)
;	9/5/96  jkf/acc		- added !edit_input=500 (command line recall)
;	7/15/99 TLB/ACC		- Version for FUSE project
;	2/23/07 wvd		- Make compatible with IDL v6.3
;
;------------------------------------------------------------------------------

;
;  All FUSE DAF procedures are contained into the "pro" dataset. The path
;  to these procedures "must" be included in the IDL_PATH definition. We
;  suggest you place path to the FUSE procedures ahead of the path to 
;  other IDL procedures. This will ensure the proper IDL procedure will
;  be compiled when running the FUSE software.
;
;  Warning: When you are not running the FUSE software, you may want to
;	alter the IDL_PATH definition to reflect the software environment
;	that you are currently using.
;
;  Here is an example of our suggested changes to IDL_PATH. The example
;	assumes you have selected /disk1/fuse/pro as the location of 
;	the FUSE "pro" procedures dataset.
;
;	example:       !path = "/disk1/fuse/pro:" + !path
; 
;***************************************************************************
; Change the following path definition to reflect your installation of the 
; FUSE "pro" procedures dataset.
;***************************************************************************
;

; Increase code and data area size to maximum
;
; .size 65000 65000

; Increase the size of the IDL command recall buffer.  Default is 20.
; Use this for versions of IDL earlier than 6.2
; !edit_input=500

; Use this for versions of IDL beginning with 6.2
PREF_SET, "IDL_RBUF_SIZE", 500, /COMMIT
; You need only run this once, as IDL saves preferences between sessions.

!quiet=1				; Turn off verbose mode during startup. 

;
; FUSE system variables ( Required for running FUSE Software)
;
defsysv,'!plotunit',0L		; Redirect plot output
defsysv,'!priv',0L		; User selectable privilege mode 
defsysv,'!textout',1L		; Redirect text output (default=terminal)
defsysv,'!textunit',0L		; ...works with !textout
defsysv,'!dump',1L		; Level of output display mode
defsysv,'!debug',0L		; Debug mode
defsysv,'!prelaunch',0L		; Prelaunch data
defsysv,'!noprint',0		; Suppress text output
defsysv,'!noplot',0		; Suppress graphics output

;
; ----------------------------------------------------
;  (Optional) Suggested system variable settings 
; ----------------------------------------------------
;
PROMPT,'FUSE>'				; IDL prompt
!type     = 16				; Place grid around the plots
!grid     = 0
!x.margin(0) = !x.margin(0) * 1.5    	; Allow YTITLE on left edge
!ignore   = 0                        	; Only plot inside the box!
!quiet    = 0                           ; Turn on verbose mode. 

;
; -----------------------------------------------------
;   End of FUSE DAF IDL_STARTUP procedure
; ------------------------------------------------------------------------
