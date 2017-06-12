package	edu.jhu.Cas.CasJobsCL;  

import edu.jhu.Cas.Services.*;

public class CasJobsCL{

    public static long wsid;
    public static  boolean DEBUG = true;
    public static boolean VERBOSE = false;
    public static final String CONFIG_PATH = "CasJobs.config";
    public static int WAITSTART = 1000;
    public static int WAITMAX = 2*60*1000;

    public static void main(String[] args) throws Exception{
	if(args.length == 0){Help(); System.exit(0);}
	try{
	    CJobsProps.loadProps(CONFIG_PATH);
	    DEBUG = CJobsProps.get(CJobsProps.DEBUG).trim().compareTo("true")==0;
	    VERBOSE = CJobsProps.get(CJobsProps.VERBOSE).trim().compareTo("true") == 0;
	}catch(Exception e){
	    Fail("Could not load config from "+CONFIG_PATH,false);
	}
	ArgumentProcessor ap = new ArgumentProcessor(args);
	ap.process();
    }

    public static void Fail(String message){Fail(message,false);}
    public static void Fail(String message,boolean showhelp){
	System.out.println("\nERROR: "+message+"\n");
	if(showhelp)Help();
	System.exit(0);
    }

    public static void Help(){
	try{
	    (new HelpArgument(null)).DoWork(null);
	}catch(Exception e){}
    }
	
}

