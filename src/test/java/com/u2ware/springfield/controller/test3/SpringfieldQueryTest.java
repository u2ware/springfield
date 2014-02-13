package com.u2ware.springfield.controller.test3;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;

import com.u2ware.springfield.AbstractContextWebmvcTestRoot;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@WebAppConfiguration
public class SpringfieldQueryTest extends AbstractContextWebmvcTestRoot{

	private static String topLevelMappig = "/controller/test3";
	
	@Test
	public void testFind1() throws Exception{
		this.mockMvc.perform(
				get(topLevelMappig+"1")
				.param("age", "1")
				.param("age", "2")
			)
			.andExpect(status().isOk());
	}
	
	@Test
	public void testFind2() throws Exception{
		this.mockMvc.perform(
				get(topLevelMappig+"2")
				.param("name", "a")
			)
			.andExpect(status().isOk());

	
	}
	
	@Test
	public void testFind3() throws Exception{
		this.mockMvc.perform(
				get(topLevelMappig+"3")
				.param("id", "1")
				.param("name", "a")
			)
			.andExpect(status().isOk());
			
	}
	
}