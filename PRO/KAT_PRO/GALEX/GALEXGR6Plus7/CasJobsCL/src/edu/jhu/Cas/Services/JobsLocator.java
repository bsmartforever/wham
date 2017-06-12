/**
 * JobsLocator.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package edu.jhu.Cas.Services;

public class JobsLocator extends org.apache.axis.client.Service implements edu.jhu.Cas.Services.Jobs {

/**
 * Provides basic job management functionality.
 */

    public JobsLocator() {
    }


    public JobsLocator(org.apache.axis.EngineConfiguration config) {
        super(config);
    }

    public JobsLocator(java.lang.String wsdlLoc, javax.xml.namespace.QName sName) throws javax.xml.rpc.ServiceException {
        super(wsdlLoc, sName);
    }

    // Use to get a proxy class for JobsSoap12
    public java.lang.String JobsSoap12_address = "http://localhost/casjobs/services/jobs.asmx";

    public java.lang.String getJobsSoap12Address() {
        return JobsSoap12_address;
    }

    // The WSDD service name defaults to the port name.
    private java.lang.String JobsSoap12WSDDServiceName = "JobsSoap12";

    public java.lang.String getJobsSoap12WSDDServiceName() {
        return JobsSoap12WSDDServiceName;
    }

    public void setJobsSoap12WSDDServiceName(java.lang.String name) {
        JobsSoap12WSDDServiceName = name;
    }

    public edu.jhu.Cas.Services.JobsSoap getJobsSoap12() throws javax.xml.rpc.ServiceException {
       java.net.URL endpoint;
        try {
            endpoint = new java.net.URL(JobsSoap12_address);
        }
        catch (java.net.MalformedURLException e) {
            throw new javax.xml.rpc.ServiceException(e);
        }
        return getJobsSoap12(endpoint);
    }

    public edu.jhu.Cas.Services.JobsSoap getJobsSoap12(java.net.URL portAddress) throws javax.xml.rpc.ServiceException {
        try {
            edu.jhu.Cas.Services.JobsSoap12Stub _stub = new edu.jhu.Cas.Services.JobsSoap12Stub(portAddress, this);
            _stub.setPortName(getJobsSoap12WSDDServiceName());
            return _stub;
        }
        catch (org.apache.axis.AxisFault e) {
            return null;
        }
    }

    public void setJobsSoap12EndpointAddress(java.lang.String address) {
        JobsSoap12_address = address;
    }


    // Use to get a proxy class for JobsSoap
    private java.lang.String JobsSoap_address = "http://localhost/casjobs/services/jobs.asmx";

    public java.lang.String getJobsSoapAddress() {
        return JobsSoap_address;
    }

    // The WSDD service name defaults to the port name.
    private java.lang.String JobsSoapWSDDServiceName = "JobsSoap";

    public java.lang.String getJobsSoapWSDDServiceName() {
        return JobsSoapWSDDServiceName;
    }

    public void setJobsSoapWSDDServiceName(java.lang.String name) {
        JobsSoapWSDDServiceName = name;
    }

    public edu.jhu.Cas.Services.JobsSoap getJobsSoap() throws javax.xml.rpc.ServiceException {
       java.net.URL endpoint;
        try {
            endpoint = new java.net.URL(JobsSoap_address);
        }
        catch (java.net.MalformedURLException e) {
            throw new javax.xml.rpc.ServiceException(e);
        }
        return getJobsSoap(endpoint);
    }

    public edu.jhu.Cas.Services.JobsSoap getJobsSoap(java.net.URL portAddress) throws javax.xml.rpc.ServiceException {
        try {
            edu.jhu.Cas.Services.JobsSoapStub _stub = new edu.jhu.Cas.Services.JobsSoapStub(portAddress, this);
            _stub.setPortName(getJobsSoapWSDDServiceName());
            return _stub;
        }
        catch (org.apache.axis.AxisFault e) {
            return null;
        }
    }

    public void setJobsSoapEndpointAddress(java.lang.String address) {
        JobsSoap_address = address;
    }

    /**
     * For the given interface, get the stub implementation.
     * If this service has no port for the given interface,
     * then ServiceException is thrown.
     * This service has multiple ports for a given interface;
     * the proxy implementation returned may be indeterminate.
     */
    public java.rmi.Remote getPort(Class serviceEndpointInterface) throws javax.xml.rpc.ServiceException {
        try {
            if (edu.jhu.Cas.Services.JobsSoap.class.isAssignableFrom(serviceEndpointInterface)) {
                edu.jhu.Cas.Services.JobsSoap12Stub _stub = new edu.jhu.Cas.Services.JobsSoap12Stub(new java.net.URL(JobsSoap12_address), this);
                _stub.setPortName(getJobsSoap12WSDDServiceName());
                return _stub;
            }
            if (edu.jhu.Cas.Services.JobsSoap.class.isAssignableFrom(serviceEndpointInterface)) {
                edu.jhu.Cas.Services.JobsSoapStub _stub = new edu.jhu.Cas.Services.JobsSoapStub(new java.net.URL(JobsSoap_address), this);
                _stub.setPortName(getJobsSoapWSDDServiceName());
                return _stub;
            }
        }
        catch (java.lang.Throwable t) {
            throw new javax.xml.rpc.ServiceException(t);
        }
        throw new javax.xml.rpc.ServiceException("There is no stub implementation for the interface:  " + (serviceEndpointInterface == null ? "null" : serviceEndpointInterface.getName()));
    }

    /**
     * For the given interface, get the stub implementation.
     * If this service has no port for the given interface,
     * then ServiceException is thrown.
     */
    public java.rmi.Remote getPort(javax.xml.namespace.QName portName, Class serviceEndpointInterface) throws javax.xml.rpc.ServiceException {
        if (portName == null) {
            return getPort(serviceEndpointInterface);
        }
        java.lang.String inputPortName = portName.getLocalPart();
        if ("JobsSoap12".equals(inputPortName)) {
            return getJobsSoap12();
        }
        else if ("JobsSoap".equals(inputPortName)) {
            return getJobsSoap();
        }
        else  {
            java.rmi.Remote _stub = getPort(serviceEndpointInterface);
            ((org.apache.axis.client.Stub) _stub).setPortName(portName);
            return _stub;
        }
    }

    public javax.xml.namespace.QName getServiceName() {
        return new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "Jobs");
    }

    private java.util.HashSet ports = null;

    public java.util.Iterator getPorts() {
        if (ports == null) {
            ports = new java.util.HashSet();
            ports.add(new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "JobsSoap12"));
            ports.add(new javax.xml.namespace.QName("http://Services.Cas.jhu.edu", "JobsSoap"));
        }
        return ports.iterator();
    }

    /**
    * Set the endpoint address for the specified port name.
    */
    public void setEndpointAddress(java.lang.String portName, java.lang.String address) throws javax.xml.rpc.ServiceException {
        
if ("JobsSoap12".equals(portName)) {
            setJobsSoap12EndpointAddress(address);
        }
        else 
if ("JobsSoap".equals(portName)) {
            setJobsSoapEndpointAddress(address);
        }
        else 
{ // Unknown Port Name
            throw new javax.xml.rpc.ServiceException(" Cannot set Endpoint Address for Unknown Port" + portName);
        }
    }

    /**
    * Set the endpoint address for the specified port name.
    */
    public void setEndpointAddress(javax.xml.namespace.QName portName, java.lang.String address) throws javax.xml.rpc.ServiceException {
        setEndpointAddress(portName.getLocalPart(), address);
    }

}
