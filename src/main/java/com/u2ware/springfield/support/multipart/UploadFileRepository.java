package com.u2ware.springfield.support.multipart;




public interface UploadFileRepository {

	public void saveFile(UploadFile bean) ;

	public void deleteFile(UploadFile bean) ;
	
}