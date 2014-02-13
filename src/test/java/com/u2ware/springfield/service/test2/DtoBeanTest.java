package com.u2ware.springfield.service.test2;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
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
public class DtoBeanTest extends AbstractContextWebmvcTestRoot{

	@Test
	public void test() throws Exception{

		this.mockMvc.perform(post("/service/test2/new")
				.param("param1", "a")
				.param("param2", "2")
				)
				//.andDo(print())
				.andExpect(status().isOk());

	
	}
	
	
	
}
