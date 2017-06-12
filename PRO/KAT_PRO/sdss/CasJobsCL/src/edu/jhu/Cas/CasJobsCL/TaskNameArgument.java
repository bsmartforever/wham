package edu.jhu.Cas.CasJobsCL;

import java.util.*;

public class TaskNameArgument extends Argument{

    public TaskNameArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "taskname";
	SHORT_NAME = "n";
	HELP = "-"+LONG_NAME+" <string>\n"+
	    "\t\tSets the taskname for a job.";
    }

    public Object DoWork(Object[] o) throws Exception{
	return null;
    }

    public int LoadArgs(int currentIndex,String[] arguments) {
	if(currentIndex+1 > arguments.length -1)Fail("TaskName option is missing an argument.");
	cjActions.TASKNAME = arguments[++currentIndex];
	if(CasJobsCL.DEBUG)System.out.println("Set taskname to "+cjActions.TASKNAME);
	return ++currentIndex;
    }
    
}
