/**
 * Jobs.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package edu.jhu.Cas.Services;

public interface Jobs extends javax.xml.rpc.Service {

/**
 * Provides basic job management functionality.
 */
    public java.lang.String getJobsSoap12Address();

    public edu.jhu.Cas.Services.JobsSoap getJobsSoap12() throws javax.xml.rpc.ServiceException;

    public edu.jhu.Cas.Services.JobsSoap getJobsSoap12(java.net.URL portAddress) throws javax.xml.rpc.ServiceException;
    public java.lang.String getJobsSoapAddress();

    public edu.jhu.Cas.Services.JobsSoap getJobsSoap() throws javax.xml.rpc.ServiceException;

    public edu.jhu.Cas.Services.JobsSoap getJobsSoap(java.net.URL portAddress) throws javax.xml.rpc.ServiceException;
}
