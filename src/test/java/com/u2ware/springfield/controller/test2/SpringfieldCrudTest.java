package com.u2ware.springfield.controller.test2;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
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
public class SpringfieldCrudTest extends AbstractContextWebmvcTestRoot{

	private static String topLevelMappig = "/controller/test2";
	
	
	@Test
	public void testCrud() throws Exception{

		//find
		this.mockMvc.perform(
				post(topLevelMappig)
			)
			.andExpect(status().isOk());
		

		//createForm
		this.mockMvc.perform(
				get(topLevelMappig+"/new")
			)
			.andExpect(status().isOk());

		
		//create
		this.mockMvc.perform(
				post(topLevelMappig+"/new")
			.param("id", "1")
			.param("name", "한글")
			.param("age", "1")
			.param("sex", "1")
			)
			.andExpect(status().isOk());

		
		//read
		this.mockMvc.perform(
				get(topLevelMappig+"/1")
			)
			.andExpect(status().isOk());
		
		
		//updateForm
		this.mockMvc.perform(
				get(topLevelMappig+"/1/edit")
			)
			.andExpect(status().isOk());

		//update
		this.mockMvc.perform(
				put(topLevelMappig+"/1/edit")
				.param("id", "1")
				.param("name", "한글")
				.param("age", "22")
				.param("sex", "1")
			)
			.andExpect(status().isOk());

		//delete
		this.mockMvc.perform(
				delete(topLevelMappig+"/1/edit")
			)
			.andExpect(status().isOk());
		
		//find
		this.mockMvc.perform(
				get(topLevelMappig)
			)
			.andExpect(status().isOk());
		
		
	}
}