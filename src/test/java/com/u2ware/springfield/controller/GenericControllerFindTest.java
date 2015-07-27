package com.u2ware.springfield.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
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
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import sample.application.consumer.Consumer;

import com.fasterxml.jackson.databind.ObjectMapper;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes=GenericControllerTestConfig.class)
@WebAppConfiguration
public class GenericControllerFindTest {
	
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
    public void testFind1() throws Exception{
        logger.warn("===================================================");
    	this.mockMvc.perform(get("/consumer")
    			.contentType(MediaType.APPLICATION_JSON)
    			)
    			.andExpect(status().isOk());

    	logger.warn("===================================================");
    	String url = this.mockMvc.perform(get("/consumer.xyz")
    			)
    			.andDo(print())
    			.andExpect(status().isOk())
    			.andExpect(model().hasNoErrors())
    			.andReturn().getResponse().getForwardedUrl();
    	Assert.assertEquals("/consumer/find.xyz", url);

    }
    
    //@Test
    public void testFind2() throws Exception{


    }

    //@Test
    public void testFind3() throws Exception{
    	
        logger.warn("===================================================");
        for(int i = 0 ; i < 100; i++){
        	this.mockMvc.perform(post("/consumer")
        			.contentType(MediaType.APPLICATION_JSON)
        			.content(new ObjectMapper().writeValueAsString(new Consumer("id"+i,"password"+i)))
        			)
        			.andExpect(status().isOk());
        }

        logger.warn("===================================================");
        MvcResult r = this.mockMvc.perform(get("/consumer/findByIdAndPassword.xyz")
    			)
    			.andExpect(status().isOk())
    			.andReturn();
        Page<?> page = (Page<?>)r.getModelAndView().getModel().get("responseDto");
        Assert.assertEquals(100, page.getTotalElements());

        logger.warn("===================================================");
        r = this.mockMvc.perform(get("/consumer/findByIdAndPassword.xyz")
        		.param("password", "password4")
    			)
    			.andExpect(status().isOk())
    			.andReturn();
        page = (Page<?>)r.getModelAndView().getModel().get("responseDto");
        Assert.assertEquals(1, page.getTotalElements());
    
    }
}


