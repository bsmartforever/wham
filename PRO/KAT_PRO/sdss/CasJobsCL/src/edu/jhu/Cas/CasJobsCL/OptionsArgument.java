package edu.jhu.Cas.CasJobsCL;

import java.util.*;

public class OptionsArgument extends Argument{
    public OptionsArgument(CasJobsActions cja){
	super(cja);
	LONG_NAME = "options";
	SHORT_NAME = LONG_NAME;
	SKIP_HELP = true;
    }

    public Object DoWork(Object[] o) throws Exception{
	System.out.println("Available general options for CasJobs:\n");
	for (Enumeration e = ArgumentProcessor.generalMap.keys() ; e.hasMoreElements() ;) {
	    String s = e.nextElement().toString();
	    if(s.length() > 1){
		Argument a = (Argument)ArgumentProcessor.generalMap.get(s);
		if(a.SKIP_HELP)continue;
		System.out.println("  "+s+", "+a.SHORT_NAME+"\t"+a.HELP);
		System.out.println();
	    }
	}
	System.out.println();
	return null;
    }

    public int LoadArgs(int currentIndex,String[] arguments) {
	return ++currentIndex;
    }
}
