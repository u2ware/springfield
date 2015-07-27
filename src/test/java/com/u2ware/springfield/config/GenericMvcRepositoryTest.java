package com.u2ware.springfield.config;

import java.util.Arrays;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;

import sample.application.consumer.Consumer;
import sample.application.desk.DeskRepository;
import sample.application.house.HouseRepository;
import sample.application.phonebook.PhoneBook;

import com.u2ware.springfield.repository.GenericRepository;



@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes=GenericMvcBasicTestConfig.class)
@WebAppConfiguration
public class GenericMvcRepositoryTest{
	
    protected Log logger = LogFactory.getLog(getClass());

    @Autowired
    protected ApplicationContext applicationContext;


    @Before
    public void setup() throws Exception {
        logger.warn("===================================================");
        String[] beanNames = applicationContext.getBeanDefinitionNames();
        Arrays.sort(beanNames, 0, beanNames.length);
        for(String name : beanNames){
            logger.warn(name+"="+applicationContext.getBean(name));
        }
        logger.warn("===================================================");
    }
    
    @Autowired(required=false) 
    @Qualifier("com_u2ware_springfield_repository_consumerRepository")
    protected GenericRepository<Consumer, String> consumerRepository;

    @Autowired(required=false) 
    protected DeskRepository deskRepository;
    
    @Autowired(required=false) 
    protected HouseRepository houseRepository;


    @Autowired(required=false) 
    @Qualifier("com_u2ware_springfield_repository_phoneBookRepository")
    protected GenericRepository<PhoneBook, String> phoneBookRepository;
    
        
    @Test
    public void test2(){
    	
        logger.warn("===================================================");
    	logger.debug("consumerRepository: "+consumerRepository);
    	if(consumerRepository != null) consumerRepository.findAll();
        logger.warn("===================================================");

    	logger.debug("deskRepository: "+deskRepository);
    	if(deskRepository != null) deskRepository.findAll();
        logger.warn("===================================================");

    	logger.debug("houseRepository: "+houseRepository);
    	if(houseRepository != null) houseRepository.findAll();
        logger.warn("===================================================");

    	logger.debug("phoneBookRepository: "+phoneBookRepository);
    	if(phoneBookRepository != null) phoneBookRepository.findAll();
        logger.warn("===================================================");

    }
}