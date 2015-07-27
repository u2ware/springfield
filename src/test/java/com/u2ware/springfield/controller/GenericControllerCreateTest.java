package com.u2ware.springfield.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
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
public class GenericControllerCreateTest {
	
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
    public void testCreate1() throws Exception{
        logger.warn("===================================================");
    	this.mockMvc.perform(post("/consumer")
    			.contentType(MediaType.APPLICATION_JSON)
    			.content(new ObjectMapper().writeValueAsString(new Consumer("id1","pass1")))
    			)
    			.andDo(print())
    			.andExpect(status().isOk());
    	
        logger.warn("===================================================");
        String url = this.mockMvc.perform(post("/consumer.xyz")
    			.param("id", "id2")
    			.param("password", "pass2")
    			)
    			.andDo(print())
    			.andExpect(status().isOk())
    			.andExpect(model().hasNoErrors())
    			.andReturn().getResponse().getForwardedUrl();
    	Assert.assertEquals("/consumer/create.xyz", url);
    }
    
    
    @Test
    public void testCreate2() throws Exception{

    	logger.warn("===================================================");
    	this.mockMvc.perform(post("/consumer/")
    			.contentType(MediaType.APPLICATION_JSON)
    			.content(new ObjectMapper().writeValueAsString(new Consumer("a","a")))
    			)
    			.andExpect(status().isNotFound());

        logger.warn("===================================================");
    	this.mockMvc.perform(post("/consumer")
    			)
    			.andExpect(status().isUnsupportedMediaType()); //405

    	logger.warn("===================================================");
    	this.mockMvc.perform(post("/consumer")
    			.contentType(MediaType.APPLICATION_JSON)
    			.content("{}")
    			)
    			.andExpect(status().isBadRequest());
    	
        logger.warn("===================================================");
    	this.mockMvc.perform(post("/consumer.xyz")
    			)
    			.andExpect(status().isOk())
    			.andExpect( model().errorCount(2));
    }
    
    
}


