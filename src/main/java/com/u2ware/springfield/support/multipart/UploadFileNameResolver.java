package com.u2ware.springfield.support.multipart;

import java.io.IOException;

import org.springframework.web.multipart.MultipartFile;

public interface UploadFileNameResolver {

	public String resolveFileName(MultipartFile multipartFile) throws IOException;
	
}
