package com.u2ware.springfield.controller.test1;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
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
public class SpringfieldPaginationTest extends AbstractContextWebmvcTestRoot{

	@Test
	public void test() throws Exception{
		this.mockMvc.perform(get("/controller/test1")

			//.param("model_query_pageable.pageEnable", "false")
			
			.param("model_query_pageable.pageNumber", "0")
			.param("model_query_pageable.pageSize", "1")
			
			.param("model_query_pageable.pageSort", "age,desc")
			.param("model_query_pageable.pageSort", "name")
			//.param("model_query_pageable.sort.dir", "desc")
			
		)
		.andDo(print())
		.andExpect(status().isOk());
	}
	
	
}
