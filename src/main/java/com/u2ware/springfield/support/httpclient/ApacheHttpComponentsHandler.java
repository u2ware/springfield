package com.u2ware.springfield.support.httpclient;

import java.net.URI;

import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;

public class ApacheHttpComponentsHandler {

	//protected static final Log logger = LogFactory.getLog(ApacheHttpComponentsHandler.class);

	private static DefaultHttpClient httpclient = getHttpClient();

	static{
		httpclient.getParams().setParameter("http.protocol.expect-continue", false);
		httpclient.getParams().setParameter("http.connection.timeout", 36000);
		httpclient.getParams().setParameter("http.socket.timeout", 36000);
	}
	
	public static DefaultHttpClient getHttpClient(){
		if(httpclient != null) return httpclient;
		else return new DefaultHttpClient();
	}

	public static ApacheHttpComponentsResponse execute(ApacheHttpComponentsRequest req) throws Exception {
		return execute(httpclient, req);
	}
	public static ApacheHttpComponentsResponse execute(DefaultHttpClient httpclient, ApacheHttpComponentsRequest r) throws Exception {

		HttpResponse response = null;
		
		if("GET".equalsIgnoreCase(r.getMethod())){
			//logger.debug("executeGet");
			
			HttpGet get = new HttpGet(new URI(r.resolveUrl()));
			for(NameValuePair pair : r.resolveHeaders()){
				get.addHeader(pair.getName(), pair.getValue());
			}
			response = httpclient.execute(get);

		}else if("POST".equalsIgnoreCase(r.getMethod())){
			//logger.debug("executePost");
			
			HttpPost post = new HttpPost(r.getUri());
			for(NameValuePair pair : r.resolveHeaders()){
				post.addHeader(pair.getName(), pair.getValue());
			}
			post.setEntity(r.resolveEntity());
			
			response = httpclient.execute(post);
			
		}else{
			throw new RuntimeException();
		}

		//logger.debug("response : "+response.getStatusLine().getStatusCode());
		return new ApacheHttpComponentsResponse(response);
	}

}
