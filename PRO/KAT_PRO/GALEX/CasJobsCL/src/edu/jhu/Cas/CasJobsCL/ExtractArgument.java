package edu.jhu.Cas.CasJobsCL;
import java.util.*;
import edu.jhu.Cas.Services.*;

public class ExtractArgument extends Argument{
    public ExtractArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "extract";
	SHORT_NAME = "e";
	littleArgs = loadMap();
	HELP = Util.Format("extract [-options] -table <tableName>\n"+
		"\t\tWith this command you can submit an extraction job.\n"+
		"\t\tThe table option is required.  Default output type\n"+
		"\t\tis CSV.");
    }

    public Object DoWork(Object[] o) throws Exception{
	if(cjActions.OUTPUT_TYPE == null)
	    cjActions.OUTPUT_TYPE = "CSV";
	if(cjActions.TABLE == null)
	    Fail("Missing 'table' option for 'extract' command");
	if(CasJobsCL.VERBOSE)
	    System.out.println("Submitting "+cjActions.OUTPUT_TYPE+" job for "+
		    "table "+cjActions.TABLE);
	long jobid = cjActions.ExtractJob();
	System.out.println("JobID is "+jobid+"\n");
	int w = CasJobsCL.WAITSTART;
	if(cjActions.DOWNLOAD || cjActions.URL){
	    cjActions.JOBID = jobid;
	    CJJob j;
	    System.out.println("Running  ");
	    for(;;){
		j = cjActions.GetJob();
		Util.PrintWorking();
		if(j.getStatus() == 4 || j.getStatus() ==5)
		    break;
		Thread.sleep(w);
		w = getWait(w);
	    }
	    System.out.println("\r ");
	    switch(j.getStatus()){
		case 5:
		    System.out.println("Query complete!");
		    System.out.println("Time: "
			    +Util.Calendar2String(j.getTimeEnd()));
		    CJJob[] tmp = new CJJob[1];
		    tmp[0] = j;
		    if(cjActions.DOWNLOAD)
			cjActions.DownloadOutput(tmp);
		    else if(cjActions.URL)
			cjActions.PrintURL(tmp);
		    else
			System.out.println("Don't know how to handle output.");
		    break;
		default:
		    System.out.println("Query failed!");
		    System.out.println("Time: "+Util.Calendar2String(j.getTimeEnd()));
		    System.out.println("Error: "+j.getError());
	    }
	    System.out.println();
	}
	return null;
    }

    public int LoadArgs(int currentIndex,String[] arguments)throws Exception {
	++currentIndex;
	SmallArgumentProcessor sap = 
	    new SmallArgumentProcessor(currentIndex,arguments,littleArgs);
	currentIndex = sap.process();
	return ++currentIndex;
    }

    private Hashtable loadMap(){
	Argument[] tmp = new Argument[5];
	tmp[0] = new TableArgument(cjActions);
	tmp[1] = new OutputTypeArgument(cjActions);
	tmp[2] = new DownloadArgument(cjActions);
	tmp[3] = new ForceArgument(cjActions);
	tmp[4] = new URLArgument(cjActions);
	return super.loadMap(tmp);
    }
}
