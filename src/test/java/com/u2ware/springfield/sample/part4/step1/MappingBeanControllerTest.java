package com.u2ware.springfield.sample.part4.step1;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;

import com.u2ware.springfield.sample.ApplicationContextTestRoot;

public class MappingBeanControllerTest extends ApplicationContextTestRoot{

	@Test
	public void testMapping() throws Exception{
		this.mockMvc.perform(
				get("/part4/step1"))
			.andExpect(status().isOk());
	}
	@Test
	public void testMappingJson() throws Exception{
		this.mockMvc.perform(
				get("/part4/step1.json"))
			.andExpect(status().isOk());
	}
	
	@Test
	public void testMappingDo() throws Exception{
		this.mockMvc.perform(
				get("/part4/step1.do"))
			.andExpect(status().isOk());
	}
	
	@Test
	public void testMappingXml() throws Exception{
		this.mockMvc.perform(
				get("/part4/step1.xml"))
			.andExpect(status().isOk());
	}
	
	@Test
	public void testMappingXls() throws Exception{
		this.mockMvc.perform(
				get("/part4/step1.xls"))
			.andExpect(status().isOk());
	}
}
