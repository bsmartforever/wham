package edu.jhu.Cas.CasJobsCL;

public class TableArgument extends Argument{
    public TableArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "table";
	SHORT_NAME = "b";
	HELP = "-"+LONG_NAME+" <string>\n"+
	    "\t\tIndicate a specific table for output command.\n"+
	    "\t\tSpecify this param to download/extract a specific table.\n"+
	    "\t\tDefault operation returns a jobid.";
    }

    public Object DoWork(Object[] o) throws Exception{
	cjActions.TABLE = table;
	return null;
    }

    private String table;
    public int LoadArgs(int currentIndex,String[] arguments) {
	try{
	    table = arguments[++currentIndex];
	}catch(Exception e){
	    Fail("Error parsing table option parameter.  This param cannot be null.");
	}
	return ++currentIndex;
    }
}
