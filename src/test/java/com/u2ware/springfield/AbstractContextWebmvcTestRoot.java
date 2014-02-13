package com.u2ware.springfield;

import org.junit.Before;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

public class AbstractContextWebmvcTestRoot extends AbstractContextTestRoot{

	@Autowired
	protected WebApplicationContext applicationContext;

	protected MockMvc mockMvc;

	@Before
	public void setup() throws Exception {
		if(mockMvc == null){
			this.mockMvc = createMockMvc();
		}
		super.setup();
	}
	
	protected MockMvc createMockMvc(){
		return MockMvcBuilders.webAppContextSetup(applicationContext).build();
	}
	
}
