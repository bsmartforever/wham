package edu.jhu.Cas.CasJobsCL;

import java.util.*;

public class SmallArgumentProcessor{
    private String[] args;
    private Hashtable map;
    private int index;

    public SmallArgumentProcessor(int index_,String[] args_,Hashtable map_) throws Exception{
	index = index_;
	args = args_;
	map = map_;
    }

    /**
     * goes through args, returns as soon as it does not have a map
     */
    public int process()throws Exception{
	int lastindex=-1;
	while(index < args.length){
	    if(index == lastindex)return index;
	    lastindex = index;
	    String arg = args[index].trim();
	    if(arg.startsWith("--"))
		index = doArg(index,arg.replaceFirst("--",""),map);
	    else if(arg.startsWith("-"))
		index = doArg(index,arg.replaceFirst("-",""),map);
	    else
		index = doArg(index,arg,map);
	}
	return index;
    }

    private int doArg(int index,String arg,Hashtable map)throws Exception{
	Object o = map.get(arg);
	if(o != null){
	    Argument argument = (Argument)o;
	    index = argument.LoadArgs(index,args);
	    argument.DoWork(null);
	}
	return index;
    }

}
