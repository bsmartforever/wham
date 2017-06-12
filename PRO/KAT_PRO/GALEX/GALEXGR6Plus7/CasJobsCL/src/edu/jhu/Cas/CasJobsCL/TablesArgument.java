package edu.jhu.Cas.CasJobsCL;

public class TablesArgument extends Argument{
    public TablesArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "tables";
	SHORT_NAME = "b";
	HELP = "tables\n"+
	    "\t\tLists all tables in MyDB.";
    }

    public Object DoWork(Object[] o) throws Exception{
	if(CasJobsCL.VERBOSE) System.out.println("Fetching tables for "+cjActions.WSID+"...\n");
	cjActions.PrintTables();
	if(CasJobsCL.VERBOSE)System.out.println("\nTable listing complete.");
	System.out.println();
	return null;
    }

    public int LoadArgs(int currentIndex,String[] arguments) {
	return ++currentIndex;
    }
}
