package com.u2ware.springfield.validation.test1;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
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
public class SpringfieldValidationTest extends AbstractContextWebmvcTestRoot{

	@Test
	public void test() throws Exception{

		this.mockMvc.perform(post("/validation/test1/new")
				.param("intValue", "1")
				.param("stringValue", "a")
				.param("floatValue", "1231.111")
				.param("dateTimeValue", "20100908")
				)
				.andExpect(model().hasErrors())
				.andExpect(status().isOk());

	}
	
	
	
}
