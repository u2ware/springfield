package com.u2ware.springfield.support.httpclient;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.apache.http.HttpResponse;

import com.fasterxml.jackson.databind.ObjectMapper;

public class ApacheHttpComponentsResponse {

	private ObjectMapper mapper = new ObjectMapper();
	private HttpResponse response;
	
	public ApacheHttpComponentsResponse(HttpResponse response){
		this.response = response;
	}
	
	public int getResponseStatus(){
		return response.getStatusLine().getStatusCode();
	}
	
	public String getResponseContent() throws IllegalStateException, IOException{
	    return IOUtils.toString(response.getEntity().getContent());
	}
	
	public InputStream getResponseStream() throws IllegalStateException, IOException {
		return response.getEntity().getContent();
	}
	
	public Reader getResponseReader() throws IllegalStateException, IOException {
		return new InputStreamReader(response.getEntity().getContent());
	}

	@SuppressWarnings("rawtypes")
	public Map getResponseJson() throws IllegalStateException, IOException{
		return mapper.readValue(getResponseReader(), Map.class);
	}
	
}
