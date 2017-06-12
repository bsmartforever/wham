package edu.jhu.Cas.CasJobsCL;

import java.util.*;

public class TargetArgument extends Argument{

    public TargetArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "target";
	SHORT_NAME = "t";
	HELP = "-"+LONG_NAME+" (<string>/<int> | <string>)\n"+
	    "\t\tSets either just the target or both target and queue for a job.";
    }

    public Object DoWork(Object[] o) throws Exception{
	return null;
    }

    public int LoadArgs(int currentIndex,String[] arguments) {
	if(currentIndex+1 > arguments.length -1)Fail("Target requires a <target>/<queue> option.");
	String[] splitted = arguments[++currentIndex].split("/");
	cjActions.TARGET = splitted[0];
	if(splitted.length > 1)
	    try{
		cjActions.QUEUE = Integer.parseInt(splitted[1]);
	    }catch(Exception e){Fail("Queue segment must be an integer.");}
	if(CasJobsCL.DEBUG)System.out.println("Set target to "+cjActions.TARGET);
	if(CasJobsCL.DEBUG)System.out.println("Set queue to "+cjActions.QUEUE);
	return ++currentIndex;
    }
    
}
