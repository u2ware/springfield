package com.u2ware.springfield.sample.part4.step2;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;

import com.u2ware.springfield.sample.ApplicationContextTestRoot;

public class ViewsBeanControllerTest extends ApplicationContextTestRoot{

	@Test
	public void testGetFind() throws Exception{
		this.mockMvc.perform(
				get("/part4/step2")
			.param("id", "1")
			.param("password", "1")
			.param("name", "1")
			.param("age", "1"))
			//.andDo(print())
			.andExpect(status().isOk());
	}

	//@Test
	public void testPostFind() throws Exception{
		this.mockMvc.perform(
				post("/part4/step2")
			.param("id", "1")
			.param("password", "1")
			.param("name", "1")
			.param("age", "1"))
			//.andDo(print())
			.andExpect(status().isOk());
	}

	//@Test
	public void testMappingDo() throws Exception{
		this.mockMvc.perform(
				post("/part4/step2.do"))
			//.andDo(print())
			.andExpect(status().isOk());
	}
	
}
