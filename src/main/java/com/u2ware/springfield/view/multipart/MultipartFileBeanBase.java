package com.u2ware.springfield.view.multipart;

public class MultipartFileBeanBase implements MultipartFileBean{

	
	private String contentFile;
	private String contentName;
	private String contentType;
	private Long contentSize;
	private boolean isDownload;
	
	public MultipartFileBeanBase(String contentFile, String contentName,String contentType, long contentSize, boolean isDownload) {
		super();
		this.contentFile = contentFile;
		this.contentType = contentType;
		this.contentName = contentName;
		this.contentSize = contentSize;
		this.isDownload = isDownload;
	}

	public String getContentFile() {
		return contentFile;
	}
	public void setContentFile(String contentFile) {
		this.contentFile = contentFile;
	}
	public String getContentType() {
		return contentType;
	}
	public void setContentType(String contentType) {
		this.contentType = contentType;
	}
	public String getContentName() {
		return contentName;
	}
	public void setContentName(String contentName) {
		this.contentName = contentName;
	}
	public Long getContentSize() {
		return contentSize;
	}
	public void setContentSize(Long contentSize) {
		this.contentSize = contentSize;
	}

	@Override
	public boolean isDownload() {
		return isDownload;
	}	
}
