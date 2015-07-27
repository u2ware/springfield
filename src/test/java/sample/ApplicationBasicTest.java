package sample;

import java.util.Arrays;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;

import javax.sql.DataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.WebIntegrationTest;
import org.springframework.context.MessageSource;
import org.springframework.core.env.Environment;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.validation.SmartValidator;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.multipart.MultipartResolver;
import org.springframework.web.servlet.HandlerMapping;

import com.fasterxml.jackson.databind.ObjectMapper;


@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(classes = Application.class)
//@WebAppConfiguration
@WebIntegrationTest
public class ApplicationBasicTest {
    protected Log logger = LogFactory.getLog(getClass());
	
	@Autowired
	private WebApplicationContext applicationContext;

    @Before
    public void setup() throws Exception {
        logger.warn("===================================================");
        String[] beanNames = applicationContext.getBeanDefinitionNames();
        Arrays.sort(beanNames, 0, beanNames.length);
        for(String name : beanNames){
            logger.warn(name+"="+applicationContext.getBean(name).getClass());
        }
        logger.warn("===================================================");
    }

	@Autowired(required=false)
	private Environment  env;
	
	@Autowired(required=false)
	private SmartValidator validator;
	
    @Autowired (required=false)
    protected MessageSource messageSource;
        
    protected @Autowired(required=false) Map<String, HandlerMapping> handlerMapping;
    protected @Autowired(required=false) Map<String, MessageSource> messageSources;
    protected @Autowired(required=false) Map<String, DataSource> dataSource;
    protected @Autowired(required=false) Map<String, ObjectMapper> objectMapper;
    protected @Autowired(required=false) Map<String, ModelMapper> modelMapper;
    protected @Autowired(required=false) Map<String, MultipartResolver> multipartResolver;

    @Test
	public void test1() {

		logger.warn("===================================================");
        logger.warn("Environment: "+env);
        logger.warn("SmartValidator: "+validator);
        
        logger.warn("===================================================");
    	for(Entry<String,MessageSource> s : messageSources.entrySet()){
        	logger.debug("MessageSource: "+s.getKey()+"="+s.getValue().getClass());
    	}
    	logger.warn("===================================================");
    	for(Entry<String,DataSource> s : dataSource.entrySet()){
        	logger.debug("DataSource: "+s.getKey()+"="+s.getValue().getClass());
    	}
    	logger.warn("===================================================");
    	for(Entry<String,HandlerMapping> s : handlerMapping.entrySet()){
        	logger.debug("HandlerMapping: "+s.getKey()+"="+s.getValue().getClass());
    	}
    	logger.warn("===================================================");
    	for(Entry<String,ObjectMapper> s : objectMapper.entrySet()){
        	logger.debug("ObjectMapper: "+s.getKey()+"="+s.getValue().getClass());
    	}
    	logger.warn("===================================================");
    	for(Entry<String,ModelMapper> s : modelMapper.entrySet()){
        	logger.debug("ModelMapper: "+s.getKey()+"="+s.getValue().getClass());
    	}
    	logger.warn("===================================================");
    	for(Entry<String,MultipartResolver> s : multipartResolver.entrySet()){
        	logger.debug("MultipartResolver: "+s.getKey()+"="+s.getValue().getClass());
    	}
    	
    	
        logger.warn("===================================================");
    	try{
        	logger.debug(messageSource.getClass());
        	logger.debug(messageSource.getMessage("springfield.aaa", null, Locale.getDefault()));
        	logger.debug(messageSource.getMessage("springfield.bbb", null, Locale.getDefault()));
    	}catch(Exception e){
    		e.printStackTrace();
    	}
    	logger.warn("===================================================");
    	
	}
}
