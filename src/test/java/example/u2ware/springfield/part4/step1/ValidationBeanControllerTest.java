package example.u2ware.springfield.part4.step1;

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
public class ValidationBeanControllerTest {

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
	public void testValidation() throws Exception{
		
		this.mockMvc.perform(post("/part4/step1/new")
				.param("tempValue", "aaaaaaaaaaaaaa")
				.param("foo", "bar")
				//.param("intValue", "1")
				.param("stringValue", "no")
				.param("floatValue", "22220020986")
				.param("enumValue", "RED")
				.param("dateTimeValue", "2012-09-09"))
			.andExpect(status().isOk())
			.andExpect(model().hasErrors());
		/*
		String key = "org.springframework.validation.BindingResult.model_entity";
		BindingResult bindingResult = (BindingResult)result.getModelAndView().getModel().get(key);
		
		for(ObjectError error : bindingResult.getAllErrors()){
			logger.debug("---------------------------------");
			for(String code : error.getCodes()){
				logger.debug("\t"+code);
			}
			logger.debug(error.getDefaultMessage());
		}
		*/
	
	}
}
