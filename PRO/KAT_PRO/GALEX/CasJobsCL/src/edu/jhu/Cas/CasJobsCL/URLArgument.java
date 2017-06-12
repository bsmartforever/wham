package edu.jhu.Cas.CasJobsCL;

public class URLArgument extends Argument{
    public URLArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "url";
	SHORT_NAME = "u";
	HELP = "-url\n"+
	    "\t\tInclusion of this option indicates that the url of an\n"+
	    "\t\toutput job should be displayed (but not downloaded).\n"+
	    "\t\tThis option pauses the program while an output job is running\n"+
	    "\t\tand displays the output's URL when the job has completed.";
    }

    public Object DoWork(Object[] o) throws Exception{
	cjActions.URL = true;
	cjActions.PATH = path;
	return null;
    }

    public String path = "";
    public int LoadArgs(int currentIndex,String[] arguments) throws Exception{
	if(HasOption(currentIndex,arguments))
	    if(!arguments[currentIndex+1].startsWith("-"))
		path=arguments[++currentIndex];
	return ++currentIndex;
    }
}
