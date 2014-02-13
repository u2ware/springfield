package com.u2ware.springfield.view.thymeleaf.test1;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
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
public class HtmlBeanTest extends AbstractContextWebmvcTestRoot{

	@Test
	public void test() throws Exception{


		this.mockMvc.perform(
				get("/view/themyleaf")
			)
			.andDo(print())
			.andExpect(status().isOk());

		
		
		this.mockMvc.perform(
				get("/view/themyleaf/new")
			)
			.andDo(print())
			.andExpect(status().isOk());


		
		this.mockMvc.perform(
				post("/view/themyleaf/new")
			.param("name", "a")
			.param("age", "1")
			)
			.andDo(print())
			.andExpect(status().isOk());

		
		
		this.mockMvc.perform(
				get("/view/themyleaf/a/edit")
			)
			.andDo(print())
			.andExpect(status().isOk());

		
		
		this.mockMvc.perform(
				put("/view/themyleaf/a/edit")
				.param("name", "a")
				.param("age", "22")
			)
			.andDo(print())
			.andExpect(status().isOk());

		
		
		this.mockMvc.perform(
				delete("/view/themyleaf/a/edit")
			)
			.andDo(print())
			.andExpect(status().isOk());
				
	}
}
