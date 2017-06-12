package edu.jhu.Cas.CasJobsCL;

import java.util.*;

public abstract class Argument{
    public String LONG_NAME;
    public String SHORT_NAME;
    protected String HELP;
    protected CasJobsActions cjActions;
    protected Hashtable littleArgs;
    public boolean SKIP_HELP = false;


    public Argument(CasJobsActions cja){
	cjActions = cja;
    }

    public abstract Object DoWork(Object[] o) throws Exception;

    /**
     * should return index next relevant index
     */
    public abstract int LoadArgs(int currentIndex,String[] arguments) throws Exception;

    //all options

    protected void Fail(String msg){Fail(msg,true);}
    protected void Fail(String msg,boolean showhelp){
	msg = msg!=null?"\n"+msg+"\n":"";
	HELP = HELP != null?HELP:"";
	msg = showhelp?msg+"\n"+HELP:msg;
	CasJobsCL.Fail(msg,false);
    }

    protected String GetHelp(){
	return HELP;
    }
    public String GetOptionsHelp(){
	if(littleArgs == null)
	    return "No options";
	Enumeration e = littleArgs.keys();
	String rst = "";
	while(e.hasMoreElements()){
	    String s = (String)e.nextElement();
	    if(s.length() > 2){//skip nicknames
		Argument a = (Argument)littleArgs.get(s);
		rst+="  "+a.LONG_NAME+", "+a.SHORT_NAME+"\t"+a.GetHelp()+"\n\n";
	    }
	}
	return rst;
    }

    protected void GetOptions(){
    }

    protected boolean HasOption(int index, String[] args){
	return index < (args.length-1);
    }

    protected Hashtable loadMap(Argument[] tmp){
	Hashtable ht = new Hashtable();
	for(int x=0;x<tmp.length;x++){
	    ht.put(tmp[x].SHORT_NAME,tmp[x]);
	    ht.put(tmp[x].LONG_NAME,tmp[x]);
	}
	return ht;
    }

    public int getWait(int w){
	int nextwait = w*2;
	return nextwait < CasJobsCL.WAITMAX?nextwait:CasJobsCL.WAITMAX;
    }
}
