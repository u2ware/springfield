package com.u2ware.springfield.security.test2;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;

import com.u2ware.springfield.AbstractContextWebmvcSecurityTestRoot;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@WebAppConfiguration
public class SpringfieldNavigationTest extends AbstractContextWebmvcSecurityTestRoot{


	@Test
	public void test1() throws Exception{
		this.mockMvc.perform(get("/security/test1")

		)
		.andExpect(status().isOk());
	}
	
	@Test
	public void test2() throws Exception{
		this.mockMvc.perform(get("/security/test1").session(login())
				.param("locale", "en")

		)
		.andExpect(status().isOk());
	}
}
