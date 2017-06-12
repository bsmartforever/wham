package edu.jhu.Cas.CasJobsCL;

import java.util.*;

public class HelpArgument extends Argument{
    public HelpArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "help";
	SHORT_NAME = "h";
	SKIP_HELP = true;
    }

    private String option;

    public Object DoWork(Object[] o) throws Exception{
	if(option == null){
	    System.out.println("Usage: CasJobs [general-options] command [options] [args]");
	    System.out.println("  specify --help commands for a list of commands");
	    System.out.println("  specify --help options for a list of general-options.");
	    System.out.println("  specify --help <command> to see help for a specific command.");
	    System.out.println("  specify --help to see this message.");
	    System.out.println("CasJobs Command Line Tool v0.03");
	    System.out.println("For more info visit http://casjobs.sdss.org/casjobs\n");
	}else if(option.compareTo("commands") == 0){
	    (new CommandsArgument(null)).DoWork(null);
	}else if(option.compareTo("options") == 0){
	    (new OptionsArgument(null)).DoWork(null);
	}else{
	    Argument a = (Argument)ArgumentProcessor.primaryMap.get(option);
	    if(a == null)Fail("'"+option+"' is not a vaild command.");
	    Util.PrintHelp(a);
	}
	return null;
    }

    public int LoadArgs(int currentIndex,String[] arguments) {
	if(HasOption(currentIndex,arguments))
	    option = arguments[++currentIndex];
	return ++currentIndex;
    }
}
