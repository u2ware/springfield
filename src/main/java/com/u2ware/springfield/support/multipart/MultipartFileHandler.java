package com.u2ware.springfield.support.multipart;

import java.io.File;
import java.io.IOException;

import org.springframework.web.multipart.MultipartFile;

public interface MultipartFileHandler {

	public String uploadFile(MultipartFile multipartFile) throws IOException;
	public String uploadFile(MultipartFile multipartFile, UploadFileNameResolver resolver) throws IOException;

	public File findFile(String contentFile) throws IOException;
	public void deleteFile(String contentFile) throws IOException;

}
