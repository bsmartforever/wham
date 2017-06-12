package edu.jhu.Cas.CasJobsCL;

import java.util.*;

public class ExecArgument extends Argument{

    /**
     * no args and it submits the " " query
     */
    public ExecArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "execute";
	SHORT_NAME = "x";
	littleArgs = loadMap();
	HELP = "exec [-options] <query>\n"+
	    "\t\tSends a short query and returns results.\n"+
	    "\t\tArg is either a file path or a query, depending on options.\n"+
	    "\t\tQueue option is ignored; this is the same as submitting a \n"+
	    "\t\tshort job from the website.";
    }

    public Object DoWork(Object[] o) throws Exception{
	System.out.println("Executing new query...\n");
	String rst = null;
	try{
	    rst = cjActions.ExecJob();
	}catch(Exception e){
	    Fail(Util.OnlyTheError(e),false);
	}
	if(rst != null){
	    System.out.println(rst);
	    System.out.println("\nQuery complete!\n");
	}
	return null;
    }

    public int LoadArgs(int currentIndex,String[] arguments)throws Exception {
	//must have at least one arg
	if(!HasOption(currentIndex,arguments))Fail("'execute' requires at least a <query> option");
	currentIndex = currentIndex+1;
	SmallArgumentProcessor sap = 
	    new SmallArgumentProcessor(currentIndex,arguments,littleArgs);
	currentIndex = sap.process();
	cjActions.QUERY = arguments[currentIndex];
	cjActions.QUEUE = 1;
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
