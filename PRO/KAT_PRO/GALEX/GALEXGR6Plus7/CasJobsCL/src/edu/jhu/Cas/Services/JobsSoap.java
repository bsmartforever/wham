/**
 * JobsSoap.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package edu.jhu.Cas.Services;

public interface JobsSoap extends java.rmi.Remote {

    /**
     * Executes a synchronous query in the shortest queue of a given
     * context.<br/>
     * Returns job results on job success, myriad of exceptions otherwise.<br/>
     * <br/>
     * <b>Parameters:</b><br/>
     * long wsid : WebServicesID of the account the query will be run under<br/>
     * string pw : Password of said account<br/>
     * string qry : Query to run<br/>
     * string context : Context of job<br/>
     * string taskname : Optional identifier of job<br/>
     * bool isSystem : System jobs are not visible in the web UI and require
     * a flag with GetJobs().<br/>
     * <br/>
     * <b>Output:</b><br/>
     * string : A typed-csv format string containing the results of the query.<br/>
     * Column types are embedded in the column headers.  For example, <br/>
     * <p>'[ra]:Float,[dec]:Float'</p>
     * describes two columns, named 'ra' and 'dec', both floating points.
     */
    public java.lang.String executeQuickJob(long wsid, java.lang.String pw, java.lang.String qry, java.lang.String context, java.lang.String taskname, boolean isSystem) throws java.rmi.RemoteException;

    /**
     * Submits an asynchronous query to a queue longer than the shortest
     * queue of a given context.<br/>
     * Returns a JobID on successful submission, exceptions otherwise.<br/>
     * <br/>
     * <b>Parameters:</b><br/>
     * long wsid : WebServicesID of the account the query will be run under.<br/>
     * string pw : Password of said account<br/>
     * string qry : Query to run<br/>
     * string context : Context of job<br/>
     * string taskname : Optional identifier of job.<br/>
     * int estimate : Job will be run in the queue with the time limit nearest
     * to 'estimate'.  Values are in minutes.<br/>
     * <br/>
     * <b>Output:</b><br/>
     * long : A unique identifier for the newly submitted job
     */
    public long submitJob(long wsid, java.lang.String pw, java.lang.String qry, java.lang.String context, java.lang.String taskname, int estimate) throws java.rmi.RemoteException;

    /**
     * Schedules a non-quick job for cancellation.  Has no effect
     * on quick jobs.<br/>
     * Users may only cancel jobs they own, otherwise an exception is thrown.<br/>
     * This method does nothing and returns nothing if jobid is not an actual
     * job.<br/>
     * Otherwise, this method returns once the status of the specified job
     * has changed to 'Canceling' (int = 2).<br/>
     * Note that a job is not completely cancelled until the status of that
     * job is 'Cancelled' (or 'Failed'); until then it still consumes a queue
     * slot.<br/>
     * Transition from 'Canceling' to 'Cancelled' is an internal operation
     * and is not controlled though web services.<br/>
     * This method returns nothing on success.<br/>
     * <br/>
     * <b>Parameters:</b><br/>
     * long wsid : WebServicesID of the account the cancellation will be
     * run under.<br/>
     * string pw : Password of said account<br/>
     * long jobId : Identifier of the job to cancel<br/>
     * <br/>
     * <b>Output:</b><br/>
     * Void
     */
    public void cancelJob(long wsId, java.lang.String pw, long jobId) throws java.rmi.RemoteException;

    /**
     * Returns an int that represents the current status of a job.<br/>
     * Possible values and their meanings are shown below.<br/>
     * <br/>
     * &nbsp;0 = ready<br/>
     * &nbsp;1 = started<br/>
     * &nbsp;2 = canceling<br/>
     * &nbsp;3 = cancelled<br/> 
     * &nbsp;4 = failed<br/>
     * &nbsp;5 = finished<br/>
     * <br/>
     * 'ready' is an idle state assigned to jobs created, but not yet processed.<br/>
     * 'started' and 'canceling' are active states, describing jobs that
     * are currently processing<br/>
     * 'cancelled', 'failed' and 'finished' are final states, indicating
     * a job's execution has terminated.<br/>
     * <br/>
     * <b>Parameters:</b><br/>
     * long wsid : WebServicesID of the account the status will be run under.<br/>
     * string pw : Password of said account<br/>
     * long jobId : Identifier of the job whose status will be retrieved.<br/>
     * <br/>
     * <b>Output:</b><br/>
     * int : A number representing the current status of the requested job.
     * See above for what those numbers means.
     */
    public int getJobStatus(long wsId, java.lang.String pw, long jobId) throws java.rmi.RemoteException;

    /**
     * Returns an array of CJJob objects relevant to the conditions
     * parameter.<br/>
     * GetJobs() will only return jobs created by the owner_wsid parameter,<br/>
     * unless owner_wsid has the 'admin' privilege, in which case all jobs
     * are returnable.<br/>
     * The 'conditions' parameter is a specifically formatted string describing
     * which jobs should be retrieved.<br/>
     * At the top level, this parameter is a list of keys, separated by ';'.<br/>
     * Each key set is described by a string key followed by ':' followed
     * by a '|' separated list of conditions.<br/>
     * Each condition may have one of the following possible formats:<br/>
     * <br/>
     * VALUE  (equality) <br/>
     * VALUE, (equal or greater to VALUE)<br/>
     * ,VALUE (less than or equal to VALUE)<br/>
     * V1,V2  (between V1 and V2 (inclusive))<br/>
     * <br/>
     * String and DateTime values should not be quoted and may not contain
     * any special characters (:;,|).<br/>
     * DateTime values can be any format that .net will <a href="http://msdn2.microsoft.com/en-us/library/az4se3k1.aspx">parse</a>,
     * so long as they do not contain any of the above special characters.<br/>
     * <br/>
     * The jobs returned are determined by the intersection of the keys,
     * given the union of their conditions<br/>
     * <br/>
     * Example 1:<br/>
     * jobid : 12345;<br/>
     * Jobs with a jobid equal to 12345.<br/>
     * <br/>
     * Example 2:<br/>
     * jobid : 123|321|132;<br/>
     * Jobs with a jobid equal to 123 or 321 or 132.<br/>
     * <br/>
     * Example 3:<br/>
     * jobid : 123|321; status : 5<br/>
     * Jobs with a status of 5 and a jobid of 123 of 321.<br/>
     * <br/>
     * Example 4:<br/>
     * jobid : 123,|,122<br/>
     * Returns jobs with id greater or equal to 123 or less than or equal
     * 122 (all of a users jobs).<br/>
     * <br/>
     * Example 5:<br/>
     * TimeEnd : 2008-04-5,<br/>
     * All jobs that ended after April 5, 2008.<br/>
     * <br/>
     * The following are valid keys:<br/>
     * long JobID : Unique identifier of job<br/>
     * DateTime TimeSubmit : When the job was submitted<br/>
     * DateTime TimeStart : When the job was started<br/>
     * DateTime TimeEnd : When the job completed/cancelled/failed<br/>
     * int Status : Current status of the job.  See the StatusJob description
     * for possible Status values.<br/>
     * int Queue : The queue the job ran in.<br/>
     * string TaskName : The user submitted descriptor for the job.  This
     * can be null.<br/>
     * string Error : Error message for this job, if any<br/>
     * string Query : The query submitted for this job<br/>
     * string Context : The context of this job<br/>
     * string Type : The type of this job.  See GetJobTypes for a listing
     * of possible values.<br/>
     * long WSID : The WebServicesID of the owner of this job.  If owner_wsid
     * does not have the 'admin' privilege, then this has no effect.<br/>
     * <br/>
     * <b>Parameters:</b><br/>
     * long owner_wsid : ID of owner of jobs<br/>
     * string owner_pw : Password of said owner.<br/>
     * string conditions : A formatted string describing the conditions for
     * the search.  See above for details on this.<br/>
     * bool includeSystem : Determines whether to return system jobs<br/>
     * <br/>
     * <b>Output:</b><br/>
     * CJJob[] : An array of all jobs matching the input conditions.<br/>
     * <br/>
     * A CJJob object contains:<br/>
     * long JobID : Unique identifier of job<br/>
     * long Rows : Number of rows this job modified, if applicable<br/>
     * WebServicesID : Identifier of user that submitted this job<br/>
     * DateTime TimeSubmit : When the job was submitted<br/>
     * DateTime TimeStart : When the job was started<br/>
     * DateTime TimeEnd : When the job completed/cancelled/failed<br/>
     * int Status : Current status of the job.  See the StatusJob description
     * for possible Status values.<br/>
     * int Queue : The queue the job ran in.<br/>
     * string TaskName : The user submitted descriptor for the job.  This
     * can be null.<br/>
     * string OutputLoc : The URL of the output.  This field is only non-null
     * for completed jobs where OutputType != Query<br/>
     * string Error : Error message for this job, if any<br/>
     * string Query : The query submitted for this job<br/>
     * string Context : The context of this job<br/>
     * string Type : The type of this job.  See GetJobTypes for a listing
     * of possible values.<br/>
     */
    public edu.jhu.Cas.Services.CJJob[] getJobs(long owner_wsid, java.lang.String owner_pw, java.lang.String conditions, boolean includeSystem) throws java.rmi.RemoteException;

    /**
     * Returns an array of CJQueue objects that summarize, for a particular
     * user, their available contexts and the respective timeouts of.<br/>
     * <br/>
     * <b>Parameters:</b><br/>
     * long wsid : WebServicesID of the account of whose contexts will be
     * retrieved.<br/>
     * string pw : Password of said account<br/>
     * <br/>
     * <b>Output:</b><br/>
     * CJQueue[] : An array of CJQueue objects representing all available
     * queues for the input user.<br/>
     * <br/>
     * A CJQueue object contains:<br/>
     * string Context : Name of the context<br/>
     * int Timeout : The maximum time a job is allowed to run.  The 'estimate'
     * parameter in SubmitJob maps to the closest Timeout of a given context.<br/>
     */
    public edu.jhu.Cas.Services.CJQueue[] getQueues(long wsid, java.lang.String pw) throws java.rmi.RemoteException;

    /**
     * Returns an array of CJType objects that summarize, for a particular
     * user, all available job types.<br/>
     * <br/>
     * <b>Parameters:</b><br/>
     * long wsid : WebServicesID of the account of whose types will be retrieved.<br/>
     * string pw : Password of said account<br/>
     * <br/>
     * <b>Output:</b><br/>
     * CJType[] : An array representing all available job types for a user.<br/>
     * <br/>
     * A CJType object contains:<br/>
     * string Type : Name of the type<br/>
     * string Description : Description of this type<br/>
     */
    public edu.jhu.Cas.Services.CJType[] getJobTypes(long wsid, java.lang.String pw) throws java.rmi.RemoteException;

    /**
     * Submits a table extraction job.<br/>
     * On success, returns the jobid of the newly created job.<br/>
     * <br/>
     * <b>Parameters:</b><br/>
     * long wsid : WebServicesID of account<br/>
     * string pw : Password of said account<br/>
     * string tableName : Name of mydb table to extract.<br/>
     * string type : describes type of output to create.  Valid types are
     * all types returned from GetJobTypes(), excluding 'QUERY'<br/>
     * <br/>
     * <b>Output:</b><br/>
     * long : A unique identifier (jobid) of the newly created job.
     */
    public long submitExtractJob(long wsid, java.lang.String pw, java.lang.String tableName, java.lang.String type) throws java.rmi.RemoteException;

    /**
     * Loads data into a table in mydb. <br/>
     * <br/>
     * <b>Parameters:</b><br/>
     * long wsid : WebServicesID of account<br/>
     * string pw : Password of said account<br/>
     * string tableName : Name of mydb table into which data will be loaded.<br/>
     * string data: ASCII encoded CSV data.<br/>
     * bool tableExists: If true, expects 'tableName' to exists and tries
     * to load data into said table using the schema to determine types.<br/>
     * If false, creates a new table and tries to guess an appropriate schema.<br/>
     * <br/>
     * <b>Output:</b><br/>
     * Void
     */
    public void uploadData(long wsid, java.lang.String pw, java.lang.String tableName, java.lang.String data, boolean tableExists) throws java.rmi.RemoteException;
}
