package com.u2ware.springfield.sample.part3.step2;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;

import com.u2ware.springfield.sample.ApplicationContextTestRoot;

public class CustomBeanControllerTest extends ApplicationContextTestRoot{

	@Test
	public void testFind() throws Exception{
		this.mockMvc.perform(
				get("/part3/step2")
				//.param("id", "2.1")
				.param("password", "aaaaa"))
			.andExpect(status().isOk());
	}

	
}
