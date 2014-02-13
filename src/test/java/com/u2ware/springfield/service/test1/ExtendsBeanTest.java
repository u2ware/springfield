package com.u2ware.springfield.service.test1;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
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
public class ExtendsBeanTest extends AbstractContextWebmvcTestRoot{

	@Test
	public void test() throws Exception{

		this.mockMvc.perform(post("/service/test1/new")
				.param("name", "a")
				.param("age", "1")
				)
				//.andDo(print())
				.andExpect(status().isOk());

		this.mockMvc.perform(get("/service/test1/a")
				)
				.andDo(print())
				.andExpect(status().isOk());
	
	}
	
	
	
}
