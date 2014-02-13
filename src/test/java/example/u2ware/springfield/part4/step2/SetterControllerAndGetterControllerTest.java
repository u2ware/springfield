package example.u2ware.springfield.part4.step2;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

@RunWith(SpringJUnit4ClassRunner.class)
@WebAppConfiguration
@ContextConfiguration(locations="../../application-context.xml")
public class SetterControllerAndGetterControllerTest {

	protected final Log logger = LogFactory.getLog(getClass());

	@Autowired
	protected WebApplicationContext applicationContext;

	protected MockMvc mockMvc;

	
	@Before
	public void setup() throws Exception {
		this.mockMvc = MockMvcBuilders.webAppContextSetup(applicationContext).build();
	}

	@Test
	public void testSetter() throws Exception{
		MockHttpSession httpSession = new MockHttpSession();
		
		this.mockMvc.perform(
				post("/part4/setter/new").session(httpSession)
				.param("code", "1")
				.param("name", "name1"))
			//.andDo(print())
			.andExpect(status().isOk());
		
		this.mockMvc.perform(
				get("/part4/getter/new").session(httpSession))
			//.andDo(print())
			.andExpect(status().isOk());
	}
	
}
