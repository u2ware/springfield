package com.u2ware.springfield.repository.support;

import java.util.Arrays;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.PersistenceContext;
import javax.persistence.PersistenceUnit;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import sample.application.consumer.Consumer;


/**
 * 
 * JPA 설정 확
 * 
 * 
 * @author U2ware
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes=SpringJpaBaseConfigurationTestConfig.class)
public class SpringJpaBaseConfigurationTest {
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
    }

    @PersistenceUnit
    private EntityManagerFactory emf;
    

    @PersistenceContext
    private EntityManager em;
    
    @Test
    @Transactional
    public void test(){

        logger.warn(emf);
        logger.warn(em);
        Consumer e = new Consumer("a", "a");
        em.merge(e);
        em.flush();
    }
	
	
}
