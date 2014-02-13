package com.u2ware.springfield.view.multipart.test1;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;

import com.u2ware.springfield.AbstractContextWebmvcTestRoot;
import com.u2ware.springfield.support.multipart.MultipartFileHandler;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@WebAppConfiguration
public class MultipartBeanTest extends AbstractContextWebmvcTestRoot{

	@Autowired
	protected MultipartFileHandler multipartFileHandler;

	@Test
	public void test() throws Exception{
		
		MockMultipartFile f = new MockMultipartFile("multipartFile", "data.text", "text/html", "AAAAAA".getBytes());
		String contentFile = multipartFileHandler.uploadFile(f);
		
		this.mockMvc.perform(get("/view/multipart/{contentFile}.download", contentFile)
				)
				.andDo(print())
				.andExpect(status().isOk());
		
		this.mockMvc.perform(get("/view/multipart/{contentFile}.stream", contentFile)
				)
				.andDo(print())
				.andExpect(status().isOk());
	}

}
