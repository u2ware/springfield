package com.u2ware.springfield.support.locale.test1;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
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
public class SpringfieldLocaleTest extends AbstractContextWebmvcTestRoot{

	@Test
	public void test() throws Exception{
		this.mockMvc.perform(get("/support/locale")
			)
			.andExpect(status().isOk());

	
		this.mockMvc.perform(get("/support/locale")
				.param("locale", "en")
			)
			.andExpect(status().isOk());
			
	}

	
	
}
