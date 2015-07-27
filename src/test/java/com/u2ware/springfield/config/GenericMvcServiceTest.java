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
import sample.application.desk.Desk;
import sample.application.house.HouseDto;
import sample.application.phonebook.PhoneBook;

import com.u2ware.springfield.service.GenericService;



@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes=GenericMvcBasicTestConfig.class)
@WebAppConfiguration
public class GenericMvcServiceTest{
	
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
    @Qualifier("com_u2ware_springfield_service_consumerService")
    protected GenericService<Consumer> consumerService;

    @Autowired(required=false) 
    @Qualifier("com_u2ware_springfield_service_deskService")
    protected GenericService<Desk> deskService;
    
    @Autowired(required=false) 
    @Qualifier("com_u2ware_springfield_service_houseService")
    protected GenericService<HouseDto> houseDtoService;

    @Autowired(required=false) 
    @Qualifier("com_u2ware_springfield_service_phoneBookService")
    protected GenericService<PhoneBook> phoneBookService;
            
    @Test
    public void test2() throws Exception{
    	
    	logger.debug("consumerService: "+consumerService);
    	if(consumerService != null) consumerService.find("findBy", null, null);
        logger.warn("===================================================");

    	logger.debug("deskService: "+deskService);
    	if(deskService != null) deskService.find("findBy", null, null);
        logger.warn("===================================================");

    	logger.debug("houseDtoService: "+houseDtoService);
    	if(houseDtoService != null) houseDtoService.find("findBy", null, null);
        logger.warn("===================================================");

    	logger.debug("phoneBookService: "+phoneBookService);
    	if(phoneBookService != null) phoneBookService.find("findBy", null, null);
        logger.warn("===================================================");

    }
}