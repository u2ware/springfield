package example.u2ware.springfield.part4.step4;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.fileUpload;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
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
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

@RunWith(SpringJUnit4ClassRunner.class)
@WebAppConfiguration
@ContextConfiguration(locations="../../application-context.xml")
public class UploadControllerTest {

	protected final Log logger = LogFactory.getLog(getClass());

	@Autowired
	protected WebApplicationContext applicationContext;
	//@Autowired 
	//protected MockHttpSession session;
	protected MockMvc mockMvc;

	@Before
	public void setup() throws Exception {
		this.mockMvc = MockMvcBuilders.webAppContextSetup(applicationContext).build();
	}
	
	
	@Test
	public void testUploadAndRemove() throws Exception{
		MockMultipartFile file = new MockMultipartFile("multipartFile", "data.text", "text/html", "AAAAAA".getBytes());
		

		MvcResult r = this.mockMvc.perform(fileUpload("/part4/step4/upload")
				.file(file))
			.andExpect(status().isOk()).andDo(print()).andReturn();


		String json = r.getResponse().getContentAsString();
		ObjectMapper m = new ObjectMapper();
		JsonNode node = m.readTree(json);
		String contentFile = node.get("contentFile").asText();
		logger.debug(contentFile);
		
		
		this.mockMvc.perform(post("/part4/step4/delete")
				.param("multipartFile" , contentFile))
			.andExpect(status().isOk());
	}	
}
