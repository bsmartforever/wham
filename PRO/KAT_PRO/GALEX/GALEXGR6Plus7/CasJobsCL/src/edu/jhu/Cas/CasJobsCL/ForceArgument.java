package edu.jhu.Cas.CasJobsCL;

public class ForceArgument extends Argument{
    public ForceArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "force";
	SHORT_NAME = "F";
	HELP = "-"+LONG_NAME+"\n"+
	    "\t\tRuthlessly overwrite existing files.";
    }

    public Object DoWork(Object[] o) throws Exception{
	cjActions.FORCE = true;
	return null;
    }

    private String table;
    public int LoadArgs(int currentIndex,String[] arguments) {
	return ++currentIndex;
    }
}
