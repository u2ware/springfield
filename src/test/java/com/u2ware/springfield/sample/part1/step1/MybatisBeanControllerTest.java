package com.u2ware.springfield.sample.part1.step1;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;

import com.u2ware.springfield.sample.ApplicationContextTestRoot;

public class MybatisBeanControllerTest extends ApplicationContextTestRoot{

	@Test
	public void testFind() throws Exception{

		this.mockMvc.perform(get("/part1/step1")
				.param("model_query_pageable.pageNumber" , "0")
				.param("model_query_pageable.pageSize" , "5")
				.param("model_query_pageable.sortOrders[1].property" , "age")
				.param("model_query_pageable.sortOrders[1].direction" , "-1"))
			.andExpect(status().isOk())
			.andExpect(model().attributeExists("model_query_result"));
	}	
	
}
