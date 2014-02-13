package com.u2ware.springfield.support.httpclient;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.http.HttpEntity;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.utils.URLEncodedUtils;
import org.apache.http.entity.mime.HttpMultipartMode;
import org.apache.http.entity.mime.MultipartEntity;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.message.BasicNameValuePair;

public class ApacheHttpComponentsRequest {

	protected final Log logger = LogFactory.getLog(getClass());
	
	private boolean multipart;
	private String uri;
	private String method;
	private List<NameValuePair> headers;
	private List<NameValuePair> parameters;
	private MultipartEntity multipartParameters;
	private HttpEntity entity;
	
	public ApacheHttpComponentsRequest(String method){
		this(method, false);
	}
	public ApacheHttpComponentsRequest(String method, boolean multipart){
		this.method = method;
		if(multipart && "POST".equals(method)){
			multipartParameters = new MultipartEntity( HttpMultipartMode.BROWSER_COMPATIBLE);
			this.multipart = true;
		}else{
			parameters = new ArrayList<NameValuePair>();
			this.multipart = false;
		}
		headers = new ArrayList<NameValuePair>();
	}
	public String getMethod() {
		return method;
	}
	public String getUri() {
		return uri;
	}

	///////////////////////////////////////////////
	//
	///////////////////////////////////////////////
	public void setEntity(HttpEntity entity) {
		this.entity = entity;
	}
	public void setUri(String uri) {
		this.uri = uri;
	}
	/*
	public void setUri(String uri, Object... uriVariableValues) {
		UriComponents uriComponents = UriComponentsBuilder.fromUriString(uri).build().expand(uriVariableValues).encode();
		this.uri = uriComponents.toUri().toString();
	}
	*/
	
	public void addParameter(String name, String value) throws Exception{
		if(this.multipart){
			multipartParameters.addPart(name, new StringBody(value));
		}else{
			parameters.add(new BasicNameValuePair(name, value));
		}
	}
	public void addParameter(String name, File value)throws Exception{
		if(this.multipart){
			multipartParameters.addPart(name, new FileBody(value));
		}
	}
	public void addHeader(String name, String value){
		headers.add(new BasicNameValuePair(name, value));
	}

	///////////////////////////////////////////////
	//
	///////////////////////////////////////////////
	List<NameValuePair> resolveHeaders() throws Exception{
		return headers;
	}
	
	String resolveUrl() throws Exception {
		if(multipart){
			return null;
		}else{
			StringBuilder url = new StringBuilder();
			url.append(uri);
			if(parameters.size() > 0){
				if(! uri.endsWith("?")){
					url.append("?");
				}
				url.append(URLEncodedUtils.format(parameters, "utf-8"));
			}
			return url.toString();
		}
	}
	
	HttpEntity resolveEntity() throws Exception {
		if(multipart){
			return multipartParameters;
		}else{
			if(entity != null){
				return entity;
			}else{
				return new UrlEncodedFormEntity(parameters, "utf-8");
			}
		}
	}
}
