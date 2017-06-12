package edu.jhu.Cas.CasJobsCL;
import java.util.*;
import edu.jhu.Cas.Services.*;

public class OutputArgument extends Argument{
    public OutputArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "output";
	SHORT_NAME = "o";
	littleArgs = loadMap();
	HELP = Util.Format("output [-options] [-download]\n"+
		"\t\tWith this command you can view and download available output.\n"+
		"\t\tDefault is to display complete, available output.");
    }

    public Object DoWork(Object[] o) throws Exception{
	if(cjActions.DOWNLOAD && (cjActions.STATUS == 4 || cjActions.STATUS==1))
	    Fail("You can only download completed output.");
	switch(cjActions.STATUS){
	    case 4:
		cjActions.PrintFailedOutput();
		break;
	    case 1:
		cjActions.PrintPendingOutput();
		break;
	    default:
		if(cjActions.DOWNLOAD){
		    CJJob[] jobs = cjActions.ListAvailableOutput();
		    cjActions.DownloadOutput(jobs);
		}else
		    cjActions.PrintAvailableOutput();
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
	Argument[] tmp = new Argument[4];
	tmp[0] = new StatusOptionArgument(cjActions);
	tmp[1] = new DownloadArgument(cjActions);
	tmp[2] = new TableArgument(cjActions);
	tmp[3] = new ForceArgument(cjActions);
	return super.loadMap(tmp);
    }
}
