package edu.jhu.Cas.CasJobsCL;

public class DownloadArgument extends Argument{
    public DownloadArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "download";
	SHORT_NAME = "d";
	HELP = "-download [<string>]\n"+
	    "\t\tInclusion of this option indicates output should be downloaded.\n"+
	    "\t\tTakes a path parameter to indicate where to download files.\n"+
	    "\t\tThis param must end in a separator, ie '/' or '\\'\n"+
	    "\t\tNo path parameter will download to current directory.\n"+
	    "\t\tTo download specific table output use the 'table' option in\n"+
	    "\t\taddition to this one.";
    }

    public Object DoWork(Object[] o) throws Exception{
	cjActions.DOWNLOAD = true;
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
