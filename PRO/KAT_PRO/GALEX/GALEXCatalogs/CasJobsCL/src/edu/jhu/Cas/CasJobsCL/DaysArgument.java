package edu.jhu.Cas.CasJobsCL;

public class DaysArgument extends Argument{
    public DaysArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "days";
	SHORT_NAME = "y";
	HELP = "-"+LONG_NAME+" <int>\n"+
	    "\t\tSpecifies how many days back to fetch jobs.  For example,\n"+
	    "\t\tspecifying 1 would mean jobs within 24 hours, while 7 would\n"+
	    "\t\tmean within the week.  The default value for this option is 1";
    }

    public Object DoWork(Object[] o) throws Exception{
	cjActions.DAYS = days;
	return null;
    }

    private int days;
    public int LoadArgs(int currentIndex,String[] arguments) throws Exception{
	if(!HasOption(currentIndex,arguments))
	    Fail("Incorrect 'days' option useage\n\nDays Help:");
	++currentIndex;
	days = -1;
	try{
	    days = Integer.parseInt(arguments[currentIndex]);
	}catch(Exception e){
	    Fail("'days' option must be an integer\n\nDays Help:");
	}
	return ++currentIndex;
    }
}
