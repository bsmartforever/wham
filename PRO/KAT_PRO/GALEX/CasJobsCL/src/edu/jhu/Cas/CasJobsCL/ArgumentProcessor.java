package edu.jhu.Cas.CasJobsCL;

import java.util.*;

public class ArgumentProcessor{
    private String[] args;
    public static Hashtable primaryMap;
    public static Hashtable generalMap;
    private CasJobsActions cjActions;

    public ArgumentProcessor(String[] args_) throws Exception{
	args = args_;
	cjActions = new CasJobsActions();
	primaryMap = loadPrimaryArgMap();
	generalMap = loadGeneralMap();
    }

    public void process()throws Exception{
	int index = 0;
	while(index < args.length){
	    String arg = args[index].trim();
	    if(arg.startsWith("--"))
		index = doArg(index,arg.replaceFirst("--",""),generalMap);
	    else if(arg.startsWith("-"))
		index = doArg(index,arg.replaceFirst("-",""),generalMap);
	    else
		index = doArg(index,arg,primaryMap);
	}
    }

    private int doArg(int index,String arg,Hashtable map)throws Exception{
	Object o = map.get(arg);
	if(o != null){
	    Argument argument = (Argument)o;
	    index = argument.LoadArgs(index,args);
	    argument.DoWork(null);
	    return index;
	}else
	    CasJobsCL.Fail("'"+arg+"' is not a recognized command.");
	return -1;	
    }


    private Hashtable loadPrimaryArgMap(){
	Hashtable ht = new Hashtable();
	Argument[] tmp = new Argument[9];
	tmp[0] = new ServersArgument(cjActions);
	tmp[1] = new SubmitArgument(cjActions);
	tmp[2] = new TablesArgument(cjActions);
	tmp[3] = new OutputArgument(cjActions);
	tmp[4] = new JobsArgument(cjActions);
	tmp[5] = new HelpArgument(cjActions);
	tmp[6] = new RunArgument(cjActions);
	tmp[7] = new ExecArgument(cjActions);
	tmp[8] = new ExtractArgument(cjActions);
	for(int x=0;x<tmp.length;x++){
	    ht.put(tmp[x].SHORT_NAME,tmp[x]);
	    ht.put(tmp[x].LONG_NAME,tmp[x]);
	}
	return ht;
    }
    
    private Hashtable loadGeneralMap(){
	Hashtable ht = new Hashtable();
	Argument[] tmp = new Argument[4];
	tmp[0] = new HelpArgument(null);
	tmp[1] = new CommandsArgument(null);
	tmp[2] = new VerboseArgument(null);
	tmp[3] = new WSIDArgument(null);
	for(int x=0;x<tmp.length;x++){
	    ht.put(tmp[x].SHORT_NAME,tmp[x]);
	    ht.put(tmp[x].LONG_NAME,tmp[x]);
	}
	return ht;
    }

}
