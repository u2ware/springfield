package com.u2ware.springfield.config;

import java.util.Arrays;
import java.util.Locale;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.multipart.MultipartResolver;



@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(value="GenericMvcBasicTestConfig.xml")
//@ContextConfiguration(classes=GenericMvcBasicTestConfig.class)
@WebAppConfiguration
public class GenericMvcBasicTest{
	
    protected Log logger = LogFactory.getLog(getClass());

    @Autowired
    protected WebApplicationContext applicationContext;
    protected MockMvc mockMvc;


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
    
    @Autowired 
    protected MessageSource messageSource;
        
    @Autowired 
    protected MessageSource[] messageSources;

    @Autowired(required=false)
    protected MultipartResolver multipartResolver;
    
    
    @Test
    public void test(){

    	for(MessageSource s : messageSources){
        	logger.debug("1111 "+s);
        	logger.debug("2222 "+s.getClass());
        	logger.debug("3333 "+s.hashCode());
    	}
    	
    	logger.debug(messageSource);
    	logger.debug(messageSource.getClass());
    	logger.debug(messageSource.getMessage("springfield.aaa", null, Locale.getDefault()));
    	logger.debug(messageSource.getMessage("springfield.bbb", null, Locale.getDefault()));
    	logger.debug(multipartResolver);
    	//DelegatingMessageSource d;
    	//logger.debug(messageSource.getMessage("javax.validation.constraints.AssertFalse.message", null, Locale.getDefault()));
    }
}