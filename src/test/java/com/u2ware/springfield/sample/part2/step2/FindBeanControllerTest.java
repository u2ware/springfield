package com.u2ware.springfield.sample.part2.step2;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;

import com.u2ware.springfield.sample.ApplicationContextTestRoot;

public class FindBeanControllerTest extends ApplicationContextTestRoot{

	@Test
	public void testFindByName() throws Exception{
		this.mockMvc.perform(
				get("/part2/step21")
			.param("name", "1"))
			.andExpect(status().isOk());
	}
	
	@Test
	public void testFindByAgeBetween() throws Exception{
		this.mockMvc.perform(
				get("/part2/step22")
			.param("ageMax", "10")
			.param("ageMin", "20"))
			.andExpect(status().isOk());
	}
	
}
