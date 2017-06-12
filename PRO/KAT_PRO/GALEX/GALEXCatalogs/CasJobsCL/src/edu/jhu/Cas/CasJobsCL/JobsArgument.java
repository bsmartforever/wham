package edu.jhu.Cas.CasJobsCL;

import java.util.*;

public class JobsArgument extends Argument{
    public JobsArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "jobs";
	SHORT_NAME = "j";
	HELP = "jobs [-options]\n"+
	    "\t\tReturns a listing of job info, including status.\n"+
	    "\t\tDefault operations returns all jobs within 24 hrs.";
	littleArgs = loadMap();
    }

    public Object DoWork(Object[] o) throws Exception{
	if(CasJobsCL.VERBOSE)System.out.println("Fetching jobs...\n");
	int cnt = cjActions.PrintJobs();
	if(CasJobsCL.VERBOSE && cnt > -1)System.out.println(cnt+" jobs found.\n");
	return null;
    }

    public int LoadArgs(int currentIndex,String[] arguments) throws Exception{
	if(HasOption(currentIndex,arguments)){
	    ++currentIndex;
	    SmallArgumentProcessor sap = 
		new SmallArgumentProcessor(currentIndex,arguments,littleArgs);
	    try{
		currentIndex = sap.process();
	    }catch(Exception e){Fail("Incorrect 'jobs' command usage.");}
	}
	return ++currentIndex;
    }

    private Hashtable loadMap(){
	Argument[] tmp = new Argument[3];
	tmp[0] = new StatusOptionArgument(cjActions);
	tmp[1] = new DaysArgument(cjActions);
	tmp[2] = new JobIDArgument(cjActions);
	return super.loadMap(tmp);
    }
}
