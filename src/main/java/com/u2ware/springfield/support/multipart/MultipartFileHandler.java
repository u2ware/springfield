package com.u2ware.springfield.support.multipart;

import java.io.File;

import org.springframework.web.multipart.MultipartFile;

public interface MultipartFileHandler {

	public File saveFile(MultipartFile multipartFile);
	public File findFile(String contentFile);
	public void deleteFile(String contentFile);
	
}
