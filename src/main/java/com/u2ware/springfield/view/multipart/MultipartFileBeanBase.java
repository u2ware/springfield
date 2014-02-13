package com.u2ware.springfield.view.multipart;

public class MultipartFileBeanBase implements MultipartFileBean{

	
	private String contentFile;
	private String contentName;
	private String contentType;
	private Long contentSize;
	
	public MultipartFileBeanBase(String contentFile, String contentName,String contentType, long contentSize) {
		super();
		this.contentFile = contentFile;
		this.contentType = contentType;
		this.contentName = contentName;
		this.contentSize = contentSize;
	}

	public String getContentFile() {
		return contentFile;
	}
	public String getContentType() {
		return contentType;
	}
	public String getContentName() {
		return contentName;
	}
	public Long getContentSize() {
		return contentSize;
	}
}
