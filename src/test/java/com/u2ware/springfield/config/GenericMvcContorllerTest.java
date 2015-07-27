package com.u2ware.springfield.config;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;



@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes=GenericMvcBasicTestConfig.class)
@WebAppConfiguration
public class GenericMvcContorllerTest{
	
    protected Log logger = LogFactory.getLog(getClass());

    @Autowired
    private WebApplicationContext applicationContext;
    private MockMvc mockMvc;

    @Before
    public void setup() throws Exception {
        logger.warn("===================================================");
        String[] beanNames = applicationContext.getBeanDefinitionNames();
        Arrays.sort(beanNames, 0, beanNames.length);
        for(String name : beanNames){
            logger.warn(name+"="+applicationContext.getBean(name).getClass());
        }
        logger.warn("===================================================");
        this.mockMvc = MockMvcBuilders.webAppContextSetup(applicationContext).build();
    }

    @Test
    public void testAccept() throws Exception{
    	
    	String content = null;
        logger.warn("===================================================");
        content = this.mockMvc.perform(
        		get("/sample/application/consumer")
    			.contentType(MediaType.APPLICATION_JSON)
    			)
        		.andExpect(status().isOk())
        		.andReturn().getResponse().getContentAsString();
        logger.debug(content);
        logger.warn("===================================================");
        content = this.mockMvc.perform(
        		get("/a/b/c")
    			.contentType(MediaType.APPLICATION_JSON)
    			)
        		.andExpect(status().isOk())
        		.andReturn().getResponse().getContentAsString();
        logger.debug(content);
        logger.warn("===================================================");
        content = this.mockMvc.perform(
        		get("/sample/application/house")
    			.contentType(MediaType.APPLICATION_JSON)
    			)
        		.andExpect(status().isOk())
		.andReturn().getResponse().getContentAsString();
        logger.debug(content);
        logger.warn("===================================================");
        content = this.mockMvc.perform(
        		get("/sample/application/phonebook")
    			.contentType(MediaType.APPLICATION_JSON)
    			)
        		.andExpect(status().isOk())
        		.andReturn().getResponse().getContentAsString();
        logger.debug(content);
    }
}