package example.u2ware.springfield.part4.step3;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.fileUpload;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

@RunWith(SpringJUnit4ClassRunner.class)
@WebAppConfiguration
@ContextConfiguration(locations="../../application-context.xml")
public class AttachedFileControllerTest {

	protected final Log logger = LogFactory.getLog(getClass());

	@Autowired
	protected WebApplicationContext applicationContext;

	protected MockMvc mockMvc;

	@Before
	public void setup() throws Exception {
		this.mockMvc = MockMvcBuilders.webAppContextSetup(applicationContext).build();
	}
	
	
	@Test
	public void testUpload() throws Exception{

		MockMultipartFile file = new MockMultipartFile("multipartFile", "data.text", "text/html", "AAAAAA".getBytes());
		
		this.mockMvc.perform(fileUpload("/part4/step3/new")
				.file(file)
				//.param("id", "1")
				)
			.andExpect(model().hasNoErrors())
			.andExpect(status().isOk());
	}
	//////////////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////////////	
	@Test
	public void testDownload() throws Exception{
		
		this.mockMvc.perform(
				get("/part4/step3/{id}.download", "1"))
			.andDo(print())
			.andExpect(status().isOk());
	}

	//////////////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////////////	
	@Test
	public void testStream() throws Exception{
		
		this.mockMvc.perform(
				get("/part4/step3/{id}.stream", "1"))
			.andDo(print())
			.andExpect(status().isOk());
	}
	
	//////////////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////////////	
	@Test
	public void testDelete() throws Exception{
		
		this.mockMvc.perform(
				delete("/part4/step3/{id}/edit", "1"))
			//.andDo(print())
			.andExpect(status().isOk());
	}
	
}
