package com.u2ware.springfield.support.multipart;

import org.springframework.web.multipart.MultipartFile;

public interface ContentFilePolicy {

	public String getContentFile(MultipartFile multipartFile, String name) ;

}
