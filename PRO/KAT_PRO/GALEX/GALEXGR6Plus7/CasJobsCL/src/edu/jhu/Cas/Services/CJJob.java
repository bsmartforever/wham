/**
 * CJJob.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package edu.jhu.Cas.Services;

public class CJJob  implements java.io.Serializable {
    private long jobID;

    private long rows;

    private long webServicesID;

    private java.util.Calendar timeSubmit;

    private java.util.Calendar timeStart;

    private java.util.Calendar timeEnd;

    private int status;

    private int queue;

    private java.lang.String taskName;

    private java.lang.String outputLoc;

    private java.lang.String error;

    private java.lang.String query;

    private java.lang.String context;

    private java.lang.String type;

    public CJJob() {
    }

    public CJJob(
           long jobID,
           long rows,
           long webServicesID,
           java.util.Calendar timeSubmit,
           java.util.Calendar timeStart,
           java.util.Calendar timeEnd,
           int status,
           int queue,
           java.lang.String taskName,
           java.lang.String outputLoc,
           java.lang.String error,
           java.lang.String query,
           java.lang.String context,
           java.lang.String type) {
           this.jobID = jobID;
           this.rows = rows;
           this.webServicesID = webServicesID;
           this.timeSubmit = timeSubmit;
           this.timeStart = timeStart;
           this.timeEnd = timeEnd;
           this.status = status;
           this.queue = queue;
           this.taskName = taskName;
           this.outputLoc = outputLoc;
           this.error = error;
           this.query = query;
           this.context = context;
           this.type = type;
    }


    /**
     * Gets the jobID value for this CJJob.
     * 
     * @return jobID
     */
    public long getJobID() {
        return jobID;
    }


    /**
     * Sets the jobID value for this CJJob.
     * 
     * @param jobID
     */
    public void setJobID(long jobID) {
        this.jobID = jobID;
    }


    /**
     * Gets the rows value for this CJJob.
     * 
     * @return rows
     */
    public long getRows() {
        return rows;
    }


    /**
     * Sets the rows value for this CJJob.
     * 
     * @param rows
     */
    public void setRows(long rows) {
        this.rows = rows;
    }


    /**
     * Gets the webServicesID value for this CJJob.
     * 
     * @return webServicesID
     */
    public long getWebServicesID() {
        return webServicesID;
    }


    /**
     * Sets the webServicesID value for this CJJob.
     * 
     * @param webServicesID
     */
    public void setWebServicesID(long webServicesID) {
        this.webServicesID = webServicesID;
    }


    /**
     * Gets the timeSubmit value for this CJJob.
     * 
     * @return timeSubmit
     */
    public java.util.Calendar getTimeSubmit() {
        return timeSubmit;
    }


    /**
     * Sets the timeSubmit value for this CJJob.
     * 
     * @param timeSubmit
     */
    public void setTimeSubmit(java.util.Calendar timeSubmit) {
        this.timeSubmit = timeSubmit;
    }


    /**
     * Gets the timeStart value for this CJJob.
     * 
     * @return timeStart
     */
    public java.util.Calendar getTimeStart() {
        return timeStart;
    }


    /**
     * Sets the timeStart value for this CJJob.
     * 
     * @param timeStart
     */
    public void setTimeStart(java.util.Calendar timeStart) {
        this.timeStart = timeStart;
    }


    /**
     * Gets the timeEnd value for this CJJob.
     * 
     * @return timeEnd
     */
    public java.util.Calendar getTimeEnd() {
        return timeEnd;
    }


    /**
     * Sets the timeEnd value for this CJJob.
     * 
     * @param timeEnd
     */
    public void setTimeEnd(java.util.Calendar timeEnd) {
        this.timeEnd = timeEnd;
    }


    /**
     * Gets the status value for this CJJob.
     * 
     * @return status
     */
    public int getStatus() {
        return status;
    }


    /**
     * Sets the status value for this CJJob.
     * 
     * @param status
     */
    public void setStatus(int status) {
        this.status = status;
    }


    /**
     * Gets the queue value for this CJJob.
     * 
     * @return queue
     */
    public int getQueue() {
        return queue;
    }


    /**
     * Sets the queue value for this CJJob.
     * 
     * @param queue
     */
    public void setQueue(int queue) {
        this.queue = queue;
    }


    /**
     * Gets the taskName value for this CJJob.
     * 
     * @return taskName
     */
    public java.lang.String getTaskName() {
        return taskName;
    }


    /**
     * Sets the taskName value for this CJJob.
     * 
     * @param taskName
     */
    public void setTaskName(java.lang.String taskName) {
        this.taskName = taskName;
    }


    /**
     * Gets the outputLoc value for this CJJob.
     * 
     * @return outputLoc
     */
    public java.lang.String getOutputLoc() {
        return outputLoc;
    }


    /**
     * Sets the outputLoc value for this CJJob.
     * 
     * @param outputLoc
     */
    public void setOutputLoc(java.lang.String outputLoc) {
        this.outputLoc = outputLoc;
    }


    /**
     * Gets the error value for this CJJob.
     * 
     * @return error
     */
    public java.lang.String getError() {
        return error;
    }


    /**
     * Sets the error value for this CJJob.
     * 
     * @param error
     */
    public void setError(java.lang.String error) {
        this.error = error;
    }


    /**
     * Gets the query value for this CJJob.
     * 
     * @return query
     */
    public java.lang.String getQuery() {
        return query;
    }


    /**
     * Sets the query value for this CJJob.
     * 
     * @param query
     */
    public void setQuery(java.lang.String query) {
        this.query = query;
    }


    /**
     * Gets the context value for this CJJob.
     * 
     * @return context
     */
    public java.lang.String getContext() {
        return context;
    }


    /**
     * Sets the context value for this CJJob.
     * 
     * @param context
     */
    public void setContext(java.lang.String context) {
        this.context = context;
    }


    /**
     * Gets the type value for this CJJob.
     * 
     * @return type
     */
    public java.lang.String getType() {
        return type;
    }


    /**
     * Sets the type value for this CJJob.
     * 
     * @param type
     */
    public void setType(java.lang.String type) {
        this.type = type;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof CJJob)) return false;
        CJJob other = (CJJob) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            this.jobID == other.getJobID() &&
            this.rows == other.getRows() &&
            this.webServicesID == other.getWebServicesID() &&
            ((this.timeSubmit==null && other.getTimeSubmit()==null) || 
             (this.timeSubmit!=null &&
              this.timeSubmit.equals(other.getTimeSubmit()))) &&
            ((this.timeStart==null && other.getTimeStart()==null) || 
             (this.timeStart!=null &&
              this.timeStart.equals(other.getTimeStart()))) &&
            ((this.timeEnd==null && other.getTimeEnd()==null) || 
             (this.timeEnd!=null &&
              this.timeEnd.equals(other.getTimeEnd()))) &&
            this.status == other.getStatus() &&
            this.queue == other.getQueue() &&
            ((this.taskName==null && other.getTaskName()==null) || 
             (this.taskName!=null &&
              this.taskName.equals(other.getTaskName()))) &&
            ((this.outputLoc==null && other.getOutputLoc()==null) || 
             (this.outputLoc!=null &&
              this.outputLoc.equals(other.getOutputLoc()))) &&
            ((this.error==null && other.getError()==null) || 
             (this.error!=null &&
              this.error.equals(other.getError()))) &&
            ((this.query==null && other.getQuery()==null) || 
             (this.query!=null &&
              this.query.equals(other.getQuery()))) &&
            ((this.context==null && other.getContext()==null) || 
             (this.context!=null &&
              this.context.equals(other.getContext()))) &&
            ((this.type==null && other.getType()==null) || 
             (this.type!=null &&
              this.type.equals(other.getType())));
        __equalsCalc = null;
        return _equals;
    }

    private boolean __hashCodeCalc = false;
    public synchronized int hashCode() {
        if (__hashCodeCalc) {
            return 0;
        }
        __hashCodeCalc = true;
        int _hashCode = 1;
        _hashCode += new Long(getJobID()).hashCode();
        _hashCode += new Long(getRows()).hashCode();
        _hashCode += new Long(getWebServicesID()).hashCode();
        if (getTimeSubmit() != null) {
            _hashCode += getTimeSubmit().hashCode();
        }
        if (getTimeStart() != null) {
            _hashCode += getTimeStart().hashCode();
        }
        if (getTimeEnd() != null) {
            _hashCode += getTimeEnd().hashCode();
        }
        _hashCode += getStatus();
        _hashCode += getQueue();
        if (getTaskName() != null) {
            _hashCode += getTaskName().hashCode();
        }
        if (getOutputLoc() != null) {
            _hashCode += getOutputLoc().hashCode();
        }
        if (getError() != null) {
            _hashCode += getError().hashCode();
        }
        if (getQuery() != null) {
            _hashCode += getQuery().hashCode();
        }
        if (getContext() != null) {
            _hashCode += getContext().hashCode();
        }
        if (getType() != null) {
            _hashCode += getType().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(CJJob.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "CJJob"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("jobID");
        elemField.setXmlName(new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "JobID"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "long"));
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("rows");
        elemField.setXmlName(new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "Rows"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "long"));
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("webServicesID");
        elemField.setXmlName(new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "WebServicesID"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "long"));
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("timeSubmit");
        elemField.setXmlName(new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "TimeSubmit"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "dateTime"));
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("timeStart");
        elemField.setXmlName(new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "TimeStart"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "dateTime"));
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("timeEnd");
        elemField.setXmlName(new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "TimeEnd"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "dateTime"));
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("status");
        elemField.setXmlName(new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "Status"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "int"));
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("queue");
        elemField.setXmlName(new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "Queue"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "int"));
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("taskName");
        elemField.setXmlName(new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "TaskName"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("outputLoc");
        elemField.setXmlName(new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "OutputLoc"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("error");
        elemField.setXmlName(new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "Error"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("query");
        elemField.setXmlName(new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "Query"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("context");
        elemField.setXmlName(new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "Context"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("type");
        elemField.setXmlName(new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "Type"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
    }

    /**
     * Return type metadata object
     */
    public static org.apache.axis.description.TypeDesc getTypeDesc() {
        return typeDesc;
    }

    /**
     * Get Custom Serializer
     */
    public static org.apache.axis.encoding.Serializer getSerializer(
           java.lang.String mechType, 
           java.lang.Class _javaType,  
           javax.xml.namespace.QName _xmlType) {
        return 
          new  org.apache.axis.encoding.ser.BeanSerializer(
            _javaType, _xmlType, typeDesc);
    }

    /**
     * Get Custom Deserializer
     */
    public static org.apache.axis.encoding.Deserializer getDeserializer(
           java.lang.String mechType, 
           java.lang.Class _javaType,  
           javax.xml.namespace.QName _xmlType) {
        return 
          new  org.apache.axis.encoding.ser.BeanDeserializer(
            _javaType, _xmlType, typeDesc);
    }

}
