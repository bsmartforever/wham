package edu.jhu.Cas.CasJobsCL;

public class StatusOptionArgument extends Argument{
    public StatusOptionArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "status";
	SHORT_NAME = "t";
	HELP = "-"+LONG_NAME+" [(<int>|<string>)]\n"+
	    "\t\tSpecifies the status of the jobs to retrieve.  Can specify\n"+
	    "\t\teither an integer or a string.  Accepted integers are 0-5.\n"+
	    "\t\tAccepted strings are ready, started, running, cancelling,\n"+
	    "\t\tcancelled, failed, or finished.\n"+
	    "\t\tNot specifing this options returns jobs with any status.";
    }

    public Object DoWork(Object[] o) throws Exception{
	cjActions.STATUS = status;
	return null;
    }


    private int status;
    public int LoadArgs(int currentIndex,String[] arguments) throws Exception{
	if(!HasOption(currentIndex,arguments))
	    Fail("Incorrect 'status' option useage\n\nStatus Help:");
	++currentIndex;
	status = -1;
	try{
	    status = Integer.parseInt(arguments[currentIndex]);
	}catch(Exception e){}
	if(status == -1){
	    status = Util.String2Status(arguments[currentIndex]);
	    if(status == -1) Fail("'"+arguments[currentIndex]+"' is not a valid status.\n\nStatus Help:");
	}
	return ++currentIndex;
    }
}
