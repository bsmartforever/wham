package edu.jhu.Cas.CasJobsCL;

public class JobIDArgument extends Argument{
    public JobIDArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "jobid";
	SHORT_NAME = "j";
	HELP = "-"+LONG_NAME+" <long>\n"+
	    "\t\tSets the jobid.";
    }

    public Object DoWork(Object[] o) throws Exception{
	return null;
    }

    private String table;
    public int LoadArgs(int currentIndex,String[] arguments) {
	if(!HasOption(currentIndex, arguments))
	    Fail("Missing parameter for '"+LONG_NAME+"'");
	try{
	    cjActions.JOBID = Long.parseLong(arguments[++currentIndex]);
	}catch(Exception e){
	    Fail("'"+LONG_NAME+"' param not in correct format.");
	}
	return ++currentIndex;
    }
}
