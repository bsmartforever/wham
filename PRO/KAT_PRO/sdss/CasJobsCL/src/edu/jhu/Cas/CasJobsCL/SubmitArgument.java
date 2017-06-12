package edu.jhu.Cas.CasJobsCL;

import java.util.*;

public class SubmitArgument extends Argument{

    /**
     * no args and it submits the " " query
     */
    public SubmitArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "submit";
	SHORT_NAME = "s";
	littleArgs = loadMap();
	HELP = "submit [-options] <query>\n"+
	    "\t\tSubmits a query and returns a jobid.\n"+
	    "\t\tArg is either a file path or a query, depending on options.";
    }

    public Object DoWork(Object[] o) throws Exception{
	System.out.println("Submitting new query...");
	long jobid = -1;
	try{
	    jobid = cjActions.SubmitJob();
	}catch(Exception e){
	    Fail(Util.OnlyTheError(e));
	}
	if(jobid==-1)
	    Fail("Unknown syntax error");
	System.out.println("Query succesfully submitted!");
	System.out.println("JobID is "+jobid+"\n");
	return new Long(jobid);
    }

    public int LoadArgs(int currentIndex,String[] arguments)throws Exception {
	//must have at least one arg
	if(!HasOption(currentIndex,arguments))Fail("'submit or run' requires at least a <query> option");
	currentIndex = currentIndex+1;
	SmallArgumentProcessor sap = 
	    new SmallArgumentProcessor(currentIndex,arguments,littleArgs);
	currentIndex = sap.process();
	cjActions.QUERY = arguments[currentIndex];
	if(CasJobsCL.DEBUG)System.out.println("query set:\n"+cjActions.QUERY+"\n");
	return ++currentIndex;
    }

    private Hashtable loadMap(){
	Argument[] tmp = new Argument[4];
	tmp[0] = new TargetArgument(cjActions);
	tmp[1] = new QueueArgument(cjActions);
	tmp[2] = new TaskNameArgument(cjActions);
	tmp[3] = new QueryIsFileArgument(cjActions);
	return super.loadMap(tmp);
    }

}
