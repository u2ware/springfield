package com.u2ware.springfield.repository.support;

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

import com.u2ware.springfield.repository.GenericRepository;

import sample.application.consumer.Consumer;
import sample.application.desk.DeskRepository;
import sample.application.house.HouseRepository;
import sample.application.phonebook.Phone;
import sample.application.phonebook.PhoneBook;


@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes=SpringJpaRepositoryFactoryBeanTestConfig.class)
public class SpringJpaRepositoryFactoryBeanTest {
	
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
        
        //EntityManagerBeanDefinitionRegistrarPostProcessor g;
    }

    @Autowired(required=false) 
    @Qualifier("consumerRepository")
    protected GenericRepository<Consumer, String> consumerRepository;

    @Autowired(required=false) 
    protected DeskRepository deskRepository;
    
    @Autowired(required=false) 
    protected HouseRepository houseRepository;


    @Autowired(required=false) 
    @Qualifier("phoneBookRepository")
    protected GenericRepository<PhoneBook, String> phoneBookRepository;
    
    
    @Autowired(required=false) 
    @Qualifier("roomRepository")
    protected GenericRepository<Phone, String> roomRepository;
    
        
    @Test
    public void test(){
    	
    	logger.debug("consumerRepository: "+consumerRepository);
    	if(consumerRepository != null) consumerRepository.findAll();
    	if(consumerRepository != null) consumerRepository.getClass();

    	logger.debug("deskRepository: "+deskRepository);
    	if(deskRepository != null) deskRepository.findAll();
    	if(deskRepository != null) deskRepository.getClass();

    	logger.debug("houseRepository: "+houseRepository);
    	if(houseRepository != null) houseRepository.findAll();
    	if(houseRepository != null) houseRepository.getClass();

    	logger.debug("phoneBookRepository: "+phoneBookRepository);
    	if(phoneBookRepository != null) phoneBookRepository.findAll();
    	if(phoneBookRepository != null) phoneBookRepository.getClass();

    	logger.debug("roomRepository: "+roomRepository);
    	if(roomRepository != null) roomRepository.findAll();
    	if(roomRepository != null) roomRepository.getClass();
    }
    
}
