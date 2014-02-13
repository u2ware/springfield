package com.u2ware.springfield.support.multipart;

import org.springframework.web.multipart.MultipartFile;



public interface UploadFile extends DownloadFile{

	public MultipartFile getMultipartFile();

	public void setContentFile(String contentFile);
	public void setContentName(String contentName);
	public void setContentType(String contentType);
	public void setContentSize(long contentSize);
}



//enctype="multipart/form-data" 
//public class UploadBeanImpl implements UploadFile{
//	
//	private String contentFile;
//	private String contentName;
//	private String contentType;
//	private long contentSize;
//	private MultipartFile multipartFile;
//	
//	public UploadBeanImpl(String contentFile){
//		this.contentFile = contentFile;
//	}
//	public UploadBeanImpl(MultipartFile multipartFile){
//		this.multipartFile = multipartFile;
//	}
//	
//	public String getContentFile() {
//		return contentFile;
//	}
//	public void setContentFile(String contentFile) {
//		this.contentFile = contentFile;
//	}
//	public String getContentName() {
//		return contentName;
//	}
//	public void setContentName(String contentName) {
//		this.contentName = contentName;
//	}
//	public String getContentType() {
//		return contentType;
//	}
//	public void setContentType(String contentType) {
//		this.contentType = contentType;
//	}
//	public long getContentSize() {
//		return contentSize;
//	}
//	public void setContentSize(long contentSize) {
//		this.contentSize = contentSize;
//	}
//	public MultipartFile getMultipartFile() {
//		return multipartFile;
//	}
//	
//}
