package edu.jhu.Cas.CasJobsCL;

import java.util.*;

public class QueueArgument extends Argument{

    public QueueArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "queue";
	SHORT_NAME = "q";
	HELP = "-"+LONG_NAME+" <int>\n"+
	    "\t\tSets the queue for a job.";
    }

    public Object DoWork(Object[] o) throws Exception{
	return null;
    }

    public int LoadArgs(int currentIndex,String[] arguments) {
	if(currentIndex+1 > arguments.length -1)Fail("Queue option is missing an argument.");
	String arg = arguments[++currentIndex];
	try{
	    cjActions.QUEUE = Integer.parseInt(arg);
	}catch(Exception e){
	    Fail("Could not parse queue argument.  It should be an integer.");
	}
	if(CasJobsCL.DEBUG)System.out.println("Set queue to "+cjActions.QUEUE);
	return ++currentIndex;
    }
    
}
