package com.u2ware.springfield.sample.others.upload;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.fileUpload;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;
import org.springframework.mock.web.MockMultipartFile;

import com.u2ware.springfield.sample.ApplicationContextTestRoot;

public class AttachedFileControllerTest extends ApplicationContextTestRoot{

	@Test
	public void testUpload() throws Exception{

		MockMultipartFile file = new MockMultipartFile("multipartFile", "data.text", "text/html", "AAAAAA".getBytes());
		
		this.mockMvc.perform(fileUpload("/others/upload/new")
				.file(file)
				//.param("id", "1")
				)
			.andExpect(model().hasNoErrors())
			.andExpect(status().isOk());
	}
	//////////////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////////////	
	@Test
	public void testDownload() throws Exception{
		
		this.mockMvc.perform(
				get("/others/upload/{id}.download", 1))
			.andDo(print())
			.andExpect(status().isOk());
	}

	//////////////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////////////	
	@Test
	public void testStream() throws Exception{
		
		this.mockMvc.perform(
				get("/others/upload/{id}.stream", "1"))
			.andDo(print())
			.andExpect(status().isOk());
	}
	
	//////////////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////////////	
	@Test
	public void testDelete() throws Exception{
		
		this.mockMvc.perform(
				delete("/others/upload/{id}/edit", "1"))
			//.andDo(print())
			.andExpect(status().isOk());
	}
	
}
