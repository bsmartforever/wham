package	edu.jhu.Cas.CasJobsCL;  

import edu.jhu.Cas.Services.*;
import java.net.*;
import java.io.*;
import java.util.Date;
import java.text.DateFormat;
import java.text.SimpleDateFormat;


public class CasJobsActionsBase{
    public long WSID,JOBID;
    public String TARGET,TASKNAME="MyQuery",QUERY,PATH,TABLE,OUTPUT_TYPE, PW;
    public int QUEUE,STATUS,DAYS;
    public boolean QUERY_IS_FILE,DOWNLOAD,FORCE,URL;

    private JobsSoap cJobs;

    public CasJobsActionsBase() throws Exception{
	JobsLocator locator = new JobsLocator();
	locator.JobsSoap12_address = CJobsProps.get("jobs_location");
	cJobs = locator.getJobsSoap12();
	WSID = Long.parseLong(CJobsProps.get(CJobsProps.WSID));
	TARGET = CJobsProps.get(CJobsProps.DEFAULT_TARGET);
	QUEUE = Integer.parseInt(CJobsProps.get(CJobsProps.DEFAULT_QUEUE));
	DAYS= Integer.parseInt(CJobsProps.get(CJobsProps.DEFAULT_DAYS));
	PW = CJobsProps.get(CJobsProps.PW);
	STATUS = -1;
	JOBID = -1;
    }
    public CJQueue[] ListServers() throws java.rmi.RemoteException{
	return cJobs.getQueues(WSID,PW);	
    }
    public long submitJob() throws java.rmi.RemoteException{
	return cJobs.submitJob(WSID,PW,QUERY,TARGET,TASKNAME,QUEUE);
    }
    public long ExtractJob() throws java.rmi.RemoteException{
	    return cJobs.submitExtractJob(WSID,PW,TABLE,OUTPUT_TYPE);
    }
    public String ListTables() throws java.rmi.RemoteException{
	    String qry = "select table_name as Tables from "+
		    "INFORMATION_SCHEMA.Tables where TABLE_TYPE = 'BASE TABLE'";
	    return cJobs.executeQuickJob(WSID,PW,qry,"MyDB","casjobscl list tables",true);
    }
    final String OUTPUT_TYPE_CLAUSE = "Type : CSV|DataSet|TCSV|VOTable|plot|FITS;";
    public CJJob[] ListAvailableOutput() throws java.rmi.RemoteException{
	    String conditions = OUTPUT_TYPE_CLAUSE+"Status : 5;";
	    CJJob[] rst = cJobs.getJobs(WSID, PW, conditions,false);
	    if(TABLE == null)
		    return rst;
	    else{
		    CJJob[] j = new CJJob[1];
		    for(int x=0;x<rst.length;x++)
			    if(rst[x].getTaskName().toUpperCase().trim().compareTo(TABLE.toUpperCase()) == 0){
				    j[0] = rst[0];
				    return j;
			    }
		    CasJobsCL.Fail(TABLE+" not found.",false);
		    return null;
	}
    }
    public CJJob[] ListPendingOutput() throws java.rmi.RemoteException{
	    String conditions = OUTPUT_TYPE_CLAUSE+"Status : 1;";
	    return cJobs.getJobs(WSID,PW,conditions,false);
    }
    public CJJob[] ListFailedOutput() throws java.rmi.RemoteException{
	    String conditions = OUTPUT_TYPE_CLAUSE+"Status:4;";
	    return cJobs.getJobs(WSID,PW,conditions,false);
    }
    public CJJob[] GetJobs() throws java.rmi.RemoteException{
	    String conditions = ""; 
	    System.out.println(getDateTime(DAYS));
	    if(DAYS > -1)
		    conditions+=" timesubmit : "+getDateTime(DAYS)+",;";
	    if(STATUS > -1)
		    conditions+=" status : "+STATUS+";";
	    if(JOBID > -1)
		    conditions+=" jobid : "+JOBID + ";";
	    return cJobs.getJobs(WSID,PW,conditions,false);
    }

    public CJJob GetJob() throws java.rmi.RemoteException{
	    return cJobs.getJobs(WSID,PW,"jobid : "+JOBID,false)[0];
    }
    private String getDateTime(int daysbefore) {
	    DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
	    Date myDate = new Date();
	    long dateMillis = myDate.getTime();
	    // subtract a day
	    long dayInMillis = 1000 * 60 * 60 *24 * daysbefore;
	    dateMillis = dateMillis - dayInMillis;
	    // set myDate to new time
	    myDate.setTime(dateMillis);
	    return dateFormat.format(myDate);
    }
    public String ExecJob()throws java.rmi.RemoteException{
	return cJobs.executeQuickJob(WSID,PW,QUERY,TARGET,TASKNAME,false);
    }
    public void DownloadOutput(CJJob[] j){
	for(int x=0;x<j.length;x++){
	    DownloadOutput(j[x]);
	    System.out.println();
	}
	System.out.println("\nAll downloads complete!\n");
    }
    public void PrintURL(CJJob[] j){
	for(int x=0;x<j.length;x++){
	    System.out.println(j[x].getOutputLoc());
	}
    }
    public void DownloadOutput(CJJob j){
	InputStream in = null;
	FileOutputStream out = null;
	BufferedReader reader = null;
	BufferedWriter writer = null;
	String sep = System.getProperty("file.separator");
	try{
	    URL url = new URL(j.getOutputLoc());
	    String[] tokens = url.getFile().split("\\/");
	    File f = new File(PATH+tokens[tokens.length-1]);
	    boolean doDl = true;
	    if(f.exists()){
		if(true && FORCE){
		    System.out.println("Deleting old "+f.getAbsolutePath());
		    f.delete();f.createNewFile();
		}
		else{
		    System.out.println("Skipping "+j.getTaskName()+" because "+
			    "\n"+f.getAbsolutePath()+" already exists.");
		    doDl = false;
		}
	    }else
		f.createNewFile();
	    if(doDl){
		in = url.openStream();
		out = new FileOutputStream(f);
		reader = new BufferedReader(new InputStreamReader(in));
		writer = new BufferedWriter(new OutputStreamWriter(out));
		int c;
		int pos = 0;
		System.out.print("Download "+j.getTaskName()+"  ");
		try{
		    for(;;){
			++pos;
			if(pos % 5000 == 0)
			    Util.PrintWorking();
			c = reader.read();
			if(c == -1)break;
			writer.flush();
			writer.write(c);
		    }
		}catch(Exception e){}
	    System.out.println("\b\b complete!");
	    System.out.println("Saved as: "+f.getAbsolutePath());
	    }
	    
	}catch(Exception e){
	    CasJobsCL.Fail("Could not download "+j.getOutputLoc()
		    +"\n"+Util.OnlyTheError(e));
	
	}finally{
	    try{
		in.close();
	    }catch(Exception e){}
	    try{
		out.close();
	    }catch(Exception e){}
	    try{
		writer.close();
	    }catch(Exception e){}
	    try{
		reader.close();
	    }catch(Exception e){}
	}
    }
}

