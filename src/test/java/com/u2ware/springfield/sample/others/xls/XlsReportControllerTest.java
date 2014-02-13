package com.u2ware.springfield.sample.others.xls;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;

import com.u2ware.springfield.sample.ApplicationContextTestRoot;

public class XlsReportControllerTest extends ApplicationContextTestRoot{

	//////////////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////////////
	@Test
	public void testCustomLayer1() throws Exception{
		this.mockMvc.perform(
				get("/others/xls"))
			.andDo(print())
			.andExpect(status().isOk());
	}
	
	
}
