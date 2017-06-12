package edu.jhu.Cas.CasJobsCL;

public class WSIDArgument extends Argument{
    public WSIDArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "wsid";
	SHORT_NAME = "w";
	HELP =	"-"+LONG_NAME+" <long>\n"+
	    "\t\tSpecify this option to use a specific wsid.\n";
    }

    public Object DoWork(Object[] o) throws Exception{
	return null;
    }

    public int LoadArgs(int currentIndex,String[] arguments) {
	CJobsProps.set(CJobsProps.WSID, arguments[++currentIndex]);
	return ++currentIndex;
    }
}
