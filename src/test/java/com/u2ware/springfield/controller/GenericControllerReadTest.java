package com.u2ware.springfield.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Assert;
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

import sample.application.consumer.Consumer;

import com.fasterxml.jackson.databind.ObjectMapper;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes=GenericControllerTestConfig.class)
@WebAppConfiguration
public class GenericControllerReadTest {
	
    protected Log logger = LogFactory.getLog(getClass());

    @Autowired
    protected WebApplicationContext applicationContext;
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
    public void testRead1() throws Exception{

    	this.mockMvc.perform(post("/consumer")
    			.contentType(MediaType.APPLICATION_JSON)
    			.content(new ObjectMapper().writeValueAsString(new Consumer("a","a")))
    			)
    			.andExpect(status().isOk());
    	
    	logger.warn("===================================================");
    	this.mockMvc.perform(get("/consumer/a")
    			.contentType(MediaType.APPLICATION_JSON)
    			)
    			.andDo(print())
    			.andExpect(status().isOk());


    	logger.warn("===================================================");
    	String url = this.mockMvc.perform(get("/consumer/a.xyz")
    			)
    			.andDo(print())
    			.andExpect(status().isOk())
    			.andExpect(model().hasNoErrors())
    			.andReturn().getResponse().getForwardedUrl();
    	Assert.assertEquals("/consumer/read.xyz", url);
    }
    
    @Test
    public void testRead2() throws Exception{
        logger.warn("===================================================");
    	this.mockMvc.perform(get("/consumer/a/")
    			.contentType(MediaType.APPLICATION_JSON)
    			)
    			.andExpect(status().isNotFound());	

    	logger.warn("===================================================");
    	this.mockMvc.perform(get("/consumer/a")
    			)
    			.andExpect(status().isUnsupportedMediaType()); //415

        logger.warn("===================================================");
		this.mockMvc.perform(get("/consumer/ooops")
    			.contentType(MediaType.APPLICATION_JSON)
				)
    			.andExpect( status().isBadRequest());
    	
        logger.warn("===================================================");
		this.mockMvc.perform(get("/consumer/ooops.xyz")
				)
    			.andExpect( status().isOk())
				.andExpect( model().errorCount(1));
    }
}


