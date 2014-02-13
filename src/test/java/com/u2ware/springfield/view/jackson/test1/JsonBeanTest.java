package com.u2ware.springfield.view.jackson.test1;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
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
public class JsonBeanTest extends AbstractContextWebmvcTestRoot{

	@Test
	public void test() throws Exception{

		this.mockMvc.perform(post("/view/jackson/new.json")
				.param("intValue", "1")
				.param("stringValue", "abcd")
				.param("floatValue", "1.0")
				.param("dateTimeValue", "2011-01-01")
				)
				//.andDo(print())
				.andExpect(status().isOk());
		
		
		this.mockMvc.perform(get("/view/jackson/1.json")
			)
			.andDo(print())
			.andExpect(status().isOk());
	}
	
	
	
}
