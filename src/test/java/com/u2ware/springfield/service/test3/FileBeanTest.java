package com.u2ware.springfield.service.test3;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.*;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;

import com.u2ware.springfield.AbstractContextWebmvcTestRoot;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@WebAppConfiguration
public class FileBeanTest extends AbstractContextWebmvcTestRoot{

	@Test
	public void test() throws Exception{

		MockMultipartFile file = new MockMultipartFile("uploadFile", "data.text", "text/html", "AAAAAA".getBytes());

		
		FileBean r = (FileBean)
		this.mockMvc.perform(fileUpload("/service/test3/new")
				.file(file)
				)
				//.andDo(print())
				.andExpect(status().isOk())
				.andReturn().getModelAndView().getModel().get("model_entity");

		logger.debug(""+r);
		
		this.mockMvc.perform(get("/service/test3/{contentFile}.download", r.getId())
				)
				.andDo(print())
				.andExpect(status().isOk());
		
		this.mockMvc.perform(get("/service/test3/{contentFile}.stream", r.getId())
				)
				.andDo(print())
				.andExpect(status().isOk());
	}
	
	
	
}
