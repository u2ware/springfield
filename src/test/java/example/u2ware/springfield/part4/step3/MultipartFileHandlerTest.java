package example.u2ware.springfield.part4.step3;


import java.io.File;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;

import com.u2ware.springfield.support.multipart.MultipartFileHandler;


@RunWith(SpringJUnit4ClassRunner.class)
@WebAppConfiguration
@ContextConfiguration(locations="../../application-context.xml")
public class MultipartFileHandlerTest {

	protected final Log logger = LogFactory.getLog(getClass());

	@Autowired
	protected MultipartFileHandler handler;
	
	
	//////////////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////////////
	@Test
	public void testUploadSizeExceeded() throws Exception{

		try{
			MockMultipartFile multipartFile = new MockMultipartFile("multipartFile", "data.text", "text/html", "12345678901".getBytes());
			File f = handler.saveFile(multipartFile);
			
			logger.debug(f);
		}catch(Exception e){
			logger.debug(e.getMessage());
		}
	}
	
	@Test
	public void testUploadMediaTypeMismatch() throws Exception{

		try{
			MockMultipartFile multipartFile = new MockMultipartFile("multipartFile", "data.png", "image/png", "12345678901".getBytes());
	
			File f = handler.saveFile(multipartFile);
			logger.debug(f);
		}catch(Exception e){
			logger.debug(e.getMessage());
		}
	}
	
	@Test
	public void testUploadSuccess() throws Exception{

		MockMultipartFile multipartFile = new MockMultipartFile("multipartFile", "data.text", "text/html", "1234567".getBytes());

		File f = handler.saveFile(multipartFile);
		logger.debug(f);
	}
	
}
