package com.u2ware.springfield.view.tiles2.test1;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.*;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;

import com.u2ware.springfield.AbstractContextWebmvcTestRoot;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@WebAppConfiguration
public class TilesBeanTest extends AbstractContextWebmvcTestRoot{


	@Test
	public void test() throws Exception{
		this.mockMvc.perform(get("/view/tiles2.tiles")
			)
			.andDo(print())
			.andExpect(status().isOk());
	}
	
}
