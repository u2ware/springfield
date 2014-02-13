package example.u2ware.springfield.part3.step3;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

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
public class FormControllerTest {

	protected final Log logger = LogFactory.getLog(getClass());

	@Autowired
	protected WebApplicationContext applicationContext;
	protected MockMvc mockMvc;

	@Before
	public void setup() throws Exception {
		this.mockMvc = MockMvcBuilders.webAppContextSetup(applicationContext).build();
	}
	
	@Test
	public void testCreate() throws Exception{
		this.mockMvc.perform(
				post("/part3/step3/new")
				.param("password", "a")
				.param("name", "korea")
				.param("address", "seoul"))
			.andExpect(status().isOk());
	}

	@Test
	public void testList() throws Exception{

		this.mockMvc.perform(get("/part3/step3"))
			.andExpect(status().isOk())
			.andExpect(model().attributeExists("model_query_result"));
	}	

}
