package edu.jhu.Cas.CasJobsCL;

import java.util.*;
import java.io.*;
import edu.jhu.Cas.Services.*;

public abstract class CJobsProps { 

    public static final String WSID = "wsid";
    public static final String PW = "password";
    public static final String DEFAULT_TARGET = "default_target";
    public static final String DEFAULT_QUEUE = "default_queue";
    public static final String DEFAULT_DAYS = "default_days";
    public static final String VERBOSE = "verbose";
    public static final String DEBUG = "debug";
    public static final String USE_PROXY = "proxySet";
    public static final String PROXY_HOST = "proxyHost";
    public static final String PROXY_PORT = "proxyPort";

    public static void loadProps(String path2File) throws Exception{
	FileInputStream propFile = new FileInputStream(path2File);
        Properties p = new Properties(System.getProperties());
        p.load(propFile);
        System.setProperties(p);    
       // System.getProperties().list(System.out);
    }

    //call this with above consts
    public static String get(String prop){
	return System.getProperty(prop);
    }
    public static String set(String prop,String value){
	return System.setProperty(prop,value);
    }
}
