package com.u2ware.springfield.support.multipart;

import java.io.File;
import java.io.IOException;

import org.springframework.web.multipart.MultipartFile;

public interface MultipartFileHandler {

	public String saveFile(MultipartFile multipartFile) throws IOException;
	public String saveFile(MultipartFile multipartFile, String name) throws IOException;
	public String saveFile(MultipartFile multipartFile, String name, ContentFilePolicy policy) throws IOException;

	public File findFile(String contentFile) throws IOException;
	public void deleteFile(String contentFile) throws IOException;

}
