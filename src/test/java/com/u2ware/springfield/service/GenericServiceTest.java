package com.u2ware.springfield.service;

import java.util.Arrays;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import sample.application.consumer.Consumer;



@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes=GenericServiceTestConfig.class)
public class GenericServiceTest {
	
    protected Log logger = LogFactory.getLog(getClass());

    @Autowired
    protected ApplicationContext applicationContext;


    @Before
    public void setup() throws Exception {
        logger.warn("===================================================");
        String[] beanNames = applicationContext.getBeanDefinitionNames();
        Arrays.sort(beanNames, 0, beanNames.length);
        for(String name : beanNames){
            logger.warn(name+"="+applicationContext.getBean(name).getClass());
        }
        logger.warn("===================================================");
        
        //EntityManagerBeanDefinitionRegistrarPostProcessor g;
    }

    @Autowired
    private GenericService<Consumer> service;

    
    @Test
    public void test1() throws Exception{

        logger.warn(service);
        service.create(new Consumer("service","service"));
        logger.warn("===================================================");

        try{
            logger.warn(service);
            service.create(new Consumer("service","service"));
        }catch(Exception e){
        	Assert.assertEquals(e.getMessage(), e.getClass(), ValidationException.class);
        }
    
    }
    
    @Test
    public void test2() throws Exception{

    	for(int i = 0; i < 100; i++){
            service.create(new Consumer("serviceId"+i,"servicePassword"+i));
    	}
        logger.warn("===================================================");

        service.find("IdAndPassword", new Consumer(null,"servicePassword3"), null);
        
        logger.warn("===================================================");
    }
    
}
