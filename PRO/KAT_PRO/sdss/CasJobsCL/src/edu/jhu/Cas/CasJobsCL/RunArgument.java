package edu.jhu.Cas.CasJobsCL;

import java.lang.*;
import java.util.*;
import edu.jhu.Cas.Services.*;

public class RunArgument extends SubmitArgument{
    /**
     * no args and it submits the " " query
     */
    public RunArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "run";
	SHORT_NAME = "r";
	//littleArgs = loadMap();
	HELP = "run [-options] <query>\n"+
	    "\t\tSubmits a query and waits for it to complete.\n"+
	    "\t\tArg is either a file path or a query, depending on options.";
    }

    public Object DoWork(Object[] o) throws Exception{
	cjActions.JOBID = ((Long)super.DoWork(null)).longValue();
	CJJob j;
	System.out.print("Running  ");
	int w = CasJobsCL.WAITSTART;
	for(;;){
	    j = cjActions.GetJob();
	    Util.PrintWorking();
	    if(j.getStatus() == 4 || j.getStatus() ==5)
		break;
	    Thread.sleep(w);
	    w = getWait(w);
	}
	System.out.println("\r");
	switch(j.getStatus()){
	    case 5:
		System.out.println("Query complete!");
		System.out.println("Time: "+Util.Calendar2String(j.getTimeEnd())+" Rows: "+j.getRows());
		break;
	    default:
		System.out.println("Query failed!");
		System.out.println("Time: "+Util.Calendar2String(j.getTimeEnd()));
		System.out.println("Error: "+j.getError());
	}
	System.out.println();

	return null;
    }

    public int LoadArgs(int currentIndex,String[] arguments)throws Exception {
	currentIndex = super.LoadArgs(currentIndex, arguments);
	return currentIndex;
    }


}
