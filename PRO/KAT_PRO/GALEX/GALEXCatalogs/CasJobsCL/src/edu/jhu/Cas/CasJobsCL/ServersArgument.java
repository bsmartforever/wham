package edu.jhu.Cas.CasJobsCL;

public class ServersArgument extends Argument{
    public ServersArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "servers";
	SHORT_NAME = "v";
	HELP = "servers\n"+
	    "\t\tSpecify to return a list of all <server>/<queue> combinations.";
    }

    public Object DoWork(Object[] o) throws Exception{
	if(CasJobsCL.VERBOSE){
	    System.out.println("Fetching servers...\n");
	    System.out.println("Format is <SERVER>/<QUEUE>:\n");
	}
	cjActions.PrintServers();
	if(CasJobsCL.VERBOSE)System.out.println("\nServer listing succesful.");
	System.out.println();
	return null;
    }

    public int LoadArgs(int currentIndex,String[] arguments) {
	return ++currentIndex;
    }
}
