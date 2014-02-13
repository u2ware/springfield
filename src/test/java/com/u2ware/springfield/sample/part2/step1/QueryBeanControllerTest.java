package com.u2ware.springfield.sample.part2.step1;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;

import com.u2ware.springfield.sample.ApplicationContextTestRoot;

public class QueryBeanControllerTest extends ApplicationContextTestRoot{

	@Test
	public void testFind1() throws Exception{
		this.mockMvc.perform(
				get("/part2/step1"))
			.andExpect(status().isOk());
	}
	
	@Test
	public void testFind2() throws Exception{
		this.mockMvc.perform(
				get("/part2/step1")
			.param("id", "1"))
			.andExpect(status().isOk());
	}
	
	@Test
	public void testFind3() throws Exception{
		this.mockMvc.perform(
				get("/part2/step1")
			.param("id", "1")
			.param("password", "1"))
			.andExpect(status().isOk());
	}
	
}
