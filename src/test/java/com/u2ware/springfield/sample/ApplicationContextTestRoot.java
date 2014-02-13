package com.u2ware.springfield.sample;

/*
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;
*/
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
@ContextConfiguration
public class ApplicationContextTestRoot {

	protected final Log logger = LogFactory.getLog(getClass());
	
	
	protected @Autowired WebApplicationContext applicationContext;
	protected MockMvc mockMvc;
	
	@Before
	public void setup() throws Exception {
		this.mockMvc = MockMvcBuilders.webAppContextSetup(applicationContext).build();
		
	}

	@Test
	public void beans() throws Exception {
		logger.info("======================================================================ApplicationContext");
		if(applicationContext != null){
			for(String name : applicationContext.getBeanDefinitionNames()){
				//logger.info(name+"="+applicationContext.getType(name));
			}
		}
		logger.info("======================================================================ApplicationContext");
	}
}
