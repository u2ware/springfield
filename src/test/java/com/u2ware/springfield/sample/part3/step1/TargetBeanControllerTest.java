package com.u2ware.springfield.sample.part3.step1;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;

import com.u2ware.springfield.sample.ApplicationContextTestRoot;

public class TargetBeanControllerTest extends ApplicationContextTestRoot{

	@Test
	public void testCreate() throws Exception{
		this.mockMvc.perform(post("/part3/step1/new")
				.param("id", "no")
				.param("password", "22220020986")
				.param("name", "RED"))
			.andExpect(status().isOk())
			.andExpect(model().hasErrors());
	}
	
}
