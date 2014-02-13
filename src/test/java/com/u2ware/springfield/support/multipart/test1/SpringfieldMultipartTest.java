package com.u2ware.springfield.support.multipart.test1;

import java.io.File;
import java.io.IOException;

import junit.framework.Assert;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.web.multipart.MultipartFile;

import com.u2ware.springfield.AbstractContextTestRoot;
import com.u2ware.springfield.support.multipart.MultipartFileHandler;
import com.u2ware.springfield.support.multipart.UploadFileNameResolver;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class SpringfieldMultipartTest extends AbstractContextTestRoot{

	@Autowired
	protected MultipartFileHandler multipartFileHandler;
	
	@Test
	public void test() throws Exception {


		MockMultipartFile f = new MockMultipartFile("multipartFile", "data.text", "text/html", "AAAAAA".getBytes());
		
		String contentFile1 = multipartFileHandler.uploadFile(f);
		String contentFile2 = multipartFileHandler.uploadFile(f, new UploadFileNameResolver() {
			@Override
			public String resolveFileName(MultipartFile multipartFile) throws IOException {
				return "Your Prefix"+multipartFile.getOriginalFilename()+"Your Suffix";
			}
		});
		logger.debug(contentFile1);
		logger.debug(contentFile2);

		File uploadedFile = multipartFileHandler.findFile(contentFile1);
		Assert.assertEquals(true, uploadedFile.exists());


		multipartFileHandler.deleteFile(contentFile1);
		Assert.assertEquals(false, uploadedFile.exists());
	}
	
}
