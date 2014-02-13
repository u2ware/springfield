package com.u2ware.springfield.sample;

/*
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;
*/
import javax.servlet.Filter;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

@RunWith(SpringJUnit4ClassRunner.class)
@WebAppConfiguration
@ContextConfiguration(locations="root-context.xml")
public class ApplicationContextTestRoot {

	protected static final Logger logger = LoggerFactory.getLogger(ApplicationContextTestRoot.class);
	
	@Autowired
	protected WebApplicationContext applicationContext;
	@Autowired @Qualifier("springSecurityFilterChain")
	protected Filter springSecurityFilterChain;
	protected MockMvc mockMvc;

	@Before
	public void setup() throws Exception {
		//this.mockMvc = MockMvcBuilders.webAppContextSetup(applicationContext).build();
		this.mockMvc = MockMvcBuilders.webAppContextSetup(applicationContext).addFilters(springSecurityFilterChain).build();
	}
	
	@Test
	public void beans() throws Exception{
		logger.info("======================================================================ApplicationContext");
		if(applicationContext != null){
			for(String name : applicationContext.getBeanDefinitionNames()){
				logger.info(name+"="+applicationContext.getType(name));
			}
		}
		logger.info("======================================================================ApplicationContext");
	}
	
}
