package com.u2ware.springfield.sample.others.multipart;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.mock.web.MockMultipartFile;

import com.u2ware.springfield.sample.ApplicationContextTestRoot;
import com.u2ware.springfield.support.multipart.MultipartFileHandler;

public class MultipartFileHandlerTest extends ApplicationContextTestRoot{

	@Autowired 
	protected MultipartFileHandler multipartFileHandler;
	
	//////////////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////////////
	@Test
	public void testUpload() throws Exception{

		MockMultipartFile multipartFile = new MockMultipartFile("multipartFile", "data.text", "text/html", "1234567".getBytes());

		String contentKey = multipartFileHandler.saveFile(multipartFile);
		logger.debug(contentKey);
		multipartFileHandler.deleteFile(contentKey);
	}
	
	
}
