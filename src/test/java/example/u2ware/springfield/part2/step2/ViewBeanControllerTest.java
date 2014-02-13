package example.u2ware.springfield.part2.step2;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
//import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.*;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

@RunWith(SpringJUnit4ClassRunner.class)
@WebAppConfiguration
@ContextConfiguration(locations="../../application-context.xml")
public class ViewBeanControllerTest {

	protected final Log logger = LogFactory.getLog(getClass());

	@Autowired
	protected WebApplicationContext applicationContext;

	protected MockMvc mockMvc;

	@Before
	public void setup() throws Exception {
		this.mockMvc = MockMvcBuilders.webAppContextSetup(applicationContext).build();
	}
	
	//////////////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////////////
	@Test
	public void testMappingJson() throws Exception{
		this.mockMvc.perform(
				post("/part2/step2"))
			//.andDo(print())
			.andExpect(status().isOk());
	}

	@Test
	public void testMappingDo() throws Exception{
		this.mockMvc.perform(
				post("/part2/step2.do"))
			//.andDo(print())
			.andExpect(status().isOk());
	}
	
	
	

	
}
