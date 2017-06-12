package edu.jhu.Cas.CasJobsCL;

public class OutputTypeArgument extends Argument{
    public OutputTypeArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "type";
	SHORT_NAME = "a";
	HELP = "-"+LONG_NAME+" <string>\n"+
	    "\t\tIndicate the output type for an output command.\n"+
	    "\t\tValid types include 'CSV', 'FITS', 'DATASET' and 'VOTABLE'.";
    }

    public Object DoWork(Object[] o) throws Exception{
	cjActions.OUTPUT_TYPE = type;
	return null;
    }

    private String type;
    public int LoadArgs(int currentIndex,String[] arguments) {
	try{
	    type = arguments[++currentIndex];
	}catch(Exception e){
	    Fail("Error parsing type option parameter.  This param cannot be null.");
	}
	return ++currentIndex;
    }
}
