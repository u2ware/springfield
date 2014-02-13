package com.u2ware.springfield.view.spreadsheet.test1;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;

import org.apache.commons.io.IOUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;

import com.u2ware.springfield.AbstractContextWebmvcTestRoot;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@WebAppConfiguration
public class XlsBeanTest extends AbstractContextWebmvcTestRoot{

	@Test
	public void test() throws Exception{

		this.mockMvc.perform(post("/view/spreadsheet/new.xls")
				.param("intValue", "1")
				.param("stringValue", "한글")
				.param("floatValue", "1.0")
				.param("dateTimeValue", "2011-01-01")
				)
				//.andDo(print())
				.andExpect(status().isOk());
		
		
		byte[] bytes1 = 
		this.mockMvc.perform(get("/view/spreadsheet/1.xls")
			)
			//.andDo(print())
			.andExpect(status().isOk())
			.andReturn().getResponse().getContentAsByteArray();
		ByteArrayInputStream input1 = new ByteArrayInputStream(bytes1);

		File file1 = new File("./target/output1.xls");
		FileOutputStream output1 = new FileOutputStream(file1);
		
		IOUtils.copy(input1, output1);
		logger.debug(file1.getAbsolutePath());

	
		
		
		byte[] bytes2 = 
		this.mockMvc.perform(get("/view/spreadsheet.xls")
			)
			//.andDo(print())
			.andExpect(status().isOk())
			.andReturn().getResponse().getContentAsByteArray();
		
		ByteArrayInputStream input2 = new ByteArrayInputStream(bytes2);
		File file2 = new File("./target/output2.xls");
		FileOutputStream output2 = new FileOutputStream(file2);
		IOUtils.copy(input2, output2);
		logger.debug(file2.getAbsolutePath());
	
	}
	
	
	
}
