package edu.jhu.Cas.CasJobsCL;

import java.util.*;

public class QueryIsFileArgument extends Argument{

    public QueryIsFileArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "file";
	SHORT_NAME = "f";
	HELP = "-"+LONG_NAME+"\n"+
	    "\t\tInclusion of this option indicates the <query> arg\n"+
	    "\t\tis a path to a file, not a query string.";
    }

    public Object DoWork(Object[] o) throws Exception{
	return null;
    }

    public int LoadArgs(int currentIndex,String[] arguments) {
	cjActions.QUERY_IS_FILE = true;
	if(CasJobsCL.DEBUG)System.out.println("Query is a file.");
	return ++currentIndex;
    }
    
}
