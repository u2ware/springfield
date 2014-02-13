package com.u2ware.springfield.sample.others.context;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;
import org.springframework.mock.web.MockHttpSession;

import com.u2ware.springfield.sample.ApplicationContextTestRoot;

public class DayStepControllerTest extends ApplicationContextTestRoot{


	@Test
	public void testSetter() throws Exception{
		MockHttpSession httpSession = new MockHttpSession();
		
		this.mockMvc.perform(
				post("/others/context/new").session(httpSession)
				.param("code", "1")
				.param("name", "name1"))
			//.andDo(print())
			.andExpect(status().isOk());
		
		this.mockMvc.perform(
				get("/others/context/new").session(httpSession))
			//.andDo(print())
			.andExpect(status().isOk());
	}
	
}
