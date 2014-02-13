package com.u2ware.springfield.sample.part1.step2;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

import org.junit.Test;

import com.u2ware.springfield.sample.ApplicationContextTestRoot;

public class JpaBeanControllerTest extends ApplicationContextTestRoot{

	@Test
	public void testFind() throws Exception{

		this.mockMvc.perform(get("/part1/step2")
				.param("model_query_pageable.pageNumber" , "0")
				.param("model_query_pageable.pageSize" , "5")
				.param("model_query_pageable.sortOrders[1].property" , "age")
				.param("model_query_pageable.sortOrders[1].direction" , "-1"))
			.andDo(print())
			.andExpect(status().isOk())
			.andExpect(model().attributeExists("model_query_result"));
	
	}
}
