package edu.jhu.Cas.CasJobsCL;

import java.util.*;
import java.io.*;


public abstract class Util { 

   public static String path2String(String path) throws Exception{
       FileReader out = new FileReader(path);
       BufferedReader reader = new BufferedReader(out);
       String line = null;
       String rst = "";
       while ((line = reader.readLine()) != null) 
	   rst+=line+"\n";
       out.close();
       reader.close();
       return rst;
   }

   public static String OnlyTheError(Exception e){
       String msg = e.getMessage();
       if(e instanceof org.apache.axis.AxisFault)
	   msg = msg.replaceFirst("[\\d\\D]*-->[^:]*: ","");
       msg = RemoveStackTrace(msg);
       return msg;
   }

   public static String RemoveStackTrace(String s){
       s = s.replaceAll("at[^\\n]*\\n","");
       s = s.replaceAll("\\s*---[^-]*---\\s*","");
       return s;
   }

   public static String Calendar2String(Calendar c){
       if(c != null)
	   return c.get(Calendar.MONTH)+"/"+c.get(Calendar.DATE)+"/"+c.get(Calendar.YEAR)+" "+
	       c.get(Calendar.HOUR)+":"+c.get(Calendar.MINUTE);
       else
	   return "N/A";
   }

   public static String Status2String(int status){
       switch(status){
	   case 0:
	       return "ready";
	   case 1:
	       return "started";
	   case 2:
	       return "cancelling";
	   case 3:
	       return "cancelled";
	   case 4:
	       return "failed";
	   case 5:
	       return "complete";
	   default:
	       return "unknown status";
       }
   }

   public static int String2Status(String status){
       return status.compareTo("ready")==0?0:status.compareTo("started")==0?1:
	   status.compareTo("cancelling")==0?2:status.compareTo("cancelled")==0?3:status.compareTo("failed")==0?4:status.compareTo("complete")==0?5:status.compareTo("running")==0?1:-1;

   }

   public static void DumpObjectArray(Object[][] o){
       for(int a=0;a<o.length;a++){
	   Object[] row = (Object[])o[a];
	   for(int b=0;b<row.length;b++){
	       System.out.print(row[b]);
	       if(b<(row.length-1))
		   System.out.print(", ");
	   }
	   System.out.println();
       }
   }


   private static final int width = 80;
   public static String Format(String s){
       char[] chars = s.toCharArray();
       int i =0;
       int linechars = 0;
       return s;
   }

   public static void PrintHelp(Argument a){
       System.out.println(a.GetHelp());
       System.out.println("Aliases:\n  "+a.LONG_NAME+", "+a.SHORT_NAME);
       System.out.println("\nOptions:\n");
       System.out.println(a.GetOptionsHelp().length() > 2?a.GetOptionsHelp():"No options\n\n");
   }

   private static int s;
   public static void PrintWorking(){
       s = (s+1) % 7;
       char a;
       switch(s){
	   case 1:
	       a = '/';
	       break;
	   case 2:
	       a = '-';
	       break;
	   case 3:
	       a = '\\';
	       break;
	   case 4:
	       a = '|';
	       break;
	   case 5:
	       a = '/';
	       break;
	   case 6:
	       a = '-';
	       break;
	   default:
	       a= '|';

       }
       System.out.print("\b"+a);
   }
}
