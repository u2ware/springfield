package com.u2ware.springfield.repository;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.domain.Sort.Order;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import sample.application.consumer.Consumer;



@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes=GenericRepositoryTestConfig.class)
public class GenericRepositoryTest {
	
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
    private GenericRepository<Consumer,String> repository;

    

    
    @Test
    public void test() throws Exception{

        logger.warn(repository);
        logger.warn(repository);
    	
        Set<Consumer> userList = new HashSet<Consumer>();
        for(int i=0; i < 101; i++){
            userList.add(new Consumer("a"+i,"bbbbb"));
        }

        repository.save(userList);

        Order o1 = new Order(Direction.DESC, "id");
        Order o2 = new Order(Direction.DESC, "password");
        
        Sort sort = new Sort(o1,o2);
        
        PageRequest p = new PageRequest(10, 10, sort);
        
        Page<Consumer> r = repository.findAll(p);
        logger.warn(r.getSize());
        logger.warn(r.getNumber());
        logger.warn(r.getNumberOfElements());
        logger.warn(r.getTotalElements());
        logger.warn(r.getTotalPages());
        
        
        Page<Consumer> r2 = repository.findAll(new FindById("a"), p);
        logger.warn(r2.getSize());
        logger.warn(r2.getNumber());
        logger.warn(r2.getNumberOfElements());
        logger.warn(r2.getTotalElements());
        logger.warn(r2.getTotalPages());
        
        Page<Consumer> r3 = repository.findAll(new MyQuery("a"), p);
        logger.warn(r3.getSize());
        logger.warn(r3.getNumber());
        logger.warn(r3.getNumberOfElements());
        logger.warn(r3.getTotalElements());
        logger.warn(r3.getTotalPages());
    }
    
    public static class FindById{
    	private String id;

		public FindById(String id) {
			super();
			this.id = id;
		}

		public String getId() {
			return id;
		}

		public void setId(String id) {
			this.id = id;
		}
    }

    @QueryMethod("findByPassword")
    public static class MyQuery{
    	private String password;

		public MyQuery(String password) {
			super();
			this.password = password;
		}
		public String getPassword() {
			return password;
		}

		public void setPassword(String password) {
			this.password = password;
		}
    }
       
}

/*
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [38] - ===================================================
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - dataSource=org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseFactory$EmbeddedDataSourceProxy@447ef43d
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - entityManagerFactory=org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean@6ad4f3ec
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - foo=org.springframework.data.jpa.repository.support.EntityManagerBeanDefinitionRegistrarPostProcessor@42da41bf
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - jpaMappingContext=org.springframework.data.jpa.mapping.JpaMetamodelMappingContext@71284e3
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.aop.config.internalAutoProxyCreator=proxyTargetClass=false; optimize=false; opaque=false; exposeProxy=false; frozen=false
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.context.annotation.ConfigurationClassPostProcessor.enhancedConfigurationProcessor=org.springframework.context.annotation.ConfigurationClassPostProcessor$EnhancedConfigurationBeanPostProcessor@50d256b5
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.context.annotation.ConfigurationClassPostProcessor.importAwareProcessor=org.springframework.context.annotation.ConfigurationClassPostProcessor$ImportAwareBeanPostProcessor@213aae87
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.context.annotation.internalAutowiredAnnotationProcessor=org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor@1398321c
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.context.annotation.internalCommonAnnotationProcessor=org.springframework.context.annotation.CommonAnnotationBeanPostProcessor@6d01d650
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.context.annotation.internalConfigurationAnnotationProcessor=org.springframework.context.annotation.ConfigurationClassPostProcessor@139d5619
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.context.annotation.internalPersistenceAnnotationProcessor=org.springframework.orm.jpa.support.PersistenceAnnotationBeanPostProcessor@3e7f8d5e
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.context.annotation.internalRequiredAnnotationProcessor=org.springframework.beans.factory.annotation.RequiredAnnotationBeanPostProcessor@281a3660
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.data.jpa.repository.config.JpaRepositoryConfigExtension#0=org.springframework.data.jpa.repository.config.JpaRepositoryConfigExtension@17ebbd2a
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.data.repository.core.support.RepositoryInterfaceAwareBeanPostProcessor=org.springframework.data.repository.core.support.RepositoryInterfaceAwareBeanPostProcessor@75d01caa
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.orm.jpa.SharedEntityManagerCreator#0=Shared EntityManager proxy for target factory [org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean@6ad4f3ec]
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.transaction.annotation.ProxyTransactionManagementConfiguration=org.springframework.transaction.annotation.ProxyTransactionManagementConfiguration$$EnhancerBySpringCGLIB$$96ed79d0@320a1953
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.transaction.config.internalTransactionAdvisor=org.springframework.transaction.interceptor.BeanFactoryTransactionAttributeSourceAdvisor: advice bean 'null'
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - persistenceUnitManager=org.springframework.orm.jpa.persistenceunit.DefaultPersistenceUnitManager@3504b1cf
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - producerRepository=org.springframework.data.jpa.repository.support.SimpleJpaRepository@74b3a2ae
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - restRepositoryTestConfig=com.u2ware.springfield.repository.RestRepositoryTestConfig$$EnhancerBySpringCGLIB$$c5fe0543@37a54f5e
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - transactionAttributeSource=org.springframework.transaction.annotation.AnnotationTransactionAttributeSource@32c70d01
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - transactionInterceptor=org.springframework.transaction.interceptor.TransactionInterceptor@45fdc0dc
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - transactionManager=org.springframework.orm.jpa.JpaTransactionManager@1680cf6a
23:47:00 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [44] - ===================================================

*/