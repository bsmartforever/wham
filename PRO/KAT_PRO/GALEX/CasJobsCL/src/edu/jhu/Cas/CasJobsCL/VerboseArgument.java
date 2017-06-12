package edu.jhu.Cas.CasJobsCL;

public class VerboseArgument extends Argument{
    public VerboseArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "verbose";
	SHORT_NAME = "v";
	HELP =	"-verbose\n"+
	    "\t\tSpecify this option to toggle verbose mode to the opposite\n"+
	    "\t\tf what is specified in config.";
    }

    public Object DoWork(Object[] o) throws Exception{
	CasJobsCL.VERBOSE = !CasJobsCL.VERBOSE;
	return null;
    }

    public int LoadArgs(int currentIndex,String[] arguments) {
	return ++currentIndex;
    }
}
