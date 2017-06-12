package	edu.jhu.Cas.CasJobsCL;  

import edu.jhu.Cas.Services.*;

public class CasJobsActions extends CasJobsActionsBase{

    public CasJobsActions() throws Exception{
	super();
    }

    public void PrintServers() throws java.rmi.RemoteException{
	CJQueue[] servers = ListServers();
	for(int x=0; x<servers.length; x++)
	    System.out.println(servers[x].getContext()+"/"+servers[x].getTimeout());
    }

    public long SubmitJob() throws java.rmi.RemoteException{
	try{
	    QUERY = QUERY_IS_FILE?Util.path2String(QUERY):QUERY;
	}catch(Exception e){CasJobsCL.Fail("Could not read file: "+QUERY);}
	PrintSettings();
	return super.submitJob();
    }

    public void PrintTables() throws java.rmi.RemoteException{
	System.out.println(ListTables());
    }

    public void PrintAvailableOutput() throws java.rmi.RemoteException{
	PrintOutput(ListAvailableOutput());
    }
    public void PrintPendingOutput() throws java.rmi.RemoteException{
	PrintOutput(ListPendingOutput());
    }
    public void PrintFailedOutput() throws java.rmi.RemoteException{
	PrintOutput(ListFailedOutput());
    }

    public int PrintJobs()throws java.rmi.RemoteException{
	CJJob[] jobs = GetJobs();
	PrintJobs(jobs);
	return jobs!=null?jobs.length:-1;
    }

    private void PrintOutput(CJJob[] jobs){
	if(jobs == null || jobs.length == 0){
	    System.out.println("No output found.\n");
	    return;
	}
	for(int x=0; x<jobs.length;x++){
	    System.out.println("Table: "+jobs[x].getTaskName());
	    System.out.println("Type: "+jobs[x].getType());
	    System.out.println("URL: "+jobs[x].getOutputLoc());
	    System.out.println("Completed: "+Util.Calendar2String(jobs[x].getTimeEnd()));
	}
    }

    protected void PrintJobs(CJJob[] jobs){
	if(jobs == null || jobs.length == 0){
	    System.out.println("No jobs found\n");
	    return;
	}
	for(int x=0; x<jobs.length;x++){
	    System.out.println("JobID: "+jobs[x].getJobID()+"\tName: "+jobs[x].getTaskName());
	    System.out.println("Status: "+Util.Status2String(jobs[x].getStatus())+
			"\tSubmitted: "+Util.Calendar2String(jobs[x].getTimeSubmit())+
			"\tCompleted: "+Util.Calendar2String(jobs[x].getTimeEnd()));
	    System.out.println("Message: "+jobs[x].getError());
	    System.out.println();
	}
    }


    private void PrintSettings(){
	if(CasJobsCL.VERBOSE){
	    System.out.println("\nTarget/Queue:\t"+TARGET+"/"+QUEUE);
	    System.out.println("Taskname:\t"+TASKNAME);
	    System.out.println("Query is file?:\t"+QUERY_IS_FILE);
	    System.out.println("Query:\t"+QUERY);
	    System.out.println();
	}
    }

}

