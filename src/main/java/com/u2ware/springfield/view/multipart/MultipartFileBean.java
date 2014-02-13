package com.u2ware.springfield.view.multipart;


public interface MultipartFileBean {

	public String getContentFile();
	public String getContentName();
	public String getContentType();
	public Long getContentSize();
	public boolean isDownload();
	
	
}
