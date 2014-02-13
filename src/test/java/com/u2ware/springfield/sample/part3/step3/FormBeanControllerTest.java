package com.u2ware.springfield.sample.part3.step3;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;

import com.u2ware.springfield.sample.ApplicationContextTestRoot;

public class FormBeanControllerTest extends ApplicationContextTestRoot{


	@Test
	public void testFind() throws Exception{

		this.mockMvc.perform(get("/part3/step3"))
			.andExpect(status().isOk())
			.andExpect(model().attributeExists("model_query_result"));
	}
}