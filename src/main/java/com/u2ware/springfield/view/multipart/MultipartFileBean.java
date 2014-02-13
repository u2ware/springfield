package com.u2ware.springfield.view.multipart;



public interface MultipartFileBean extends DownloadBean{

	public String getContentFile();
	public String getContentType();
	public Long getContentSize();
	
	
}
