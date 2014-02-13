package com.u2ware.springfield.sample.others.multipart;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.fileUpload;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.u2ware.springfield.sample.ApplicationContextTestRoot;

public class AjaxUploadControllerTest extends ApplicationContextTestRoot{

	
	@Test
	public void testUploadAndRemove() throws Exception{
		MockMultipartFile file = new MockMultipartFile("multipartFile", "data.text", "text/html", "AAAAAA".getBytes());
		
		MvcResult r = this.mockMvc.perform(fileUpload("/others/multipart/upload")
				.file(file))
			.andExpect(status().isOk()).andDo(print()).andReturn();


		String json = r.getResponse().getContentAsString();
		ObjectMapper m = new ObjectMapper();
		JsonNode node = m.readTree(json);
		String contentKey = node.get("contentFile").asText();
		logger.debug(contentKey);
		
		
		this.mockMvc.perform(post("/others/multipart/delete")
				.param("multipartFile" , contentKey))
			.andExpect(status().isOk());
	}
}
