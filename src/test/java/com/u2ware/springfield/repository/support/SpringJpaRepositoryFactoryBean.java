package com.u2ware.springfield.repository.support;

import java.io.Serializable;

import javax.persistence.EntityManager;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.data.jpa.repository.support.JpaRepositoryFactory;
import org.springframework.data.jpa.repository.support.JpaRepositoryFactoryBean;
import org.springframework.data.repository.Repository;
import org.springframework.data.repository.core.RepositoryMetadata;
import org.springframework.data.repository.core.support.RepositoryFactorySupport;

import com.u2ware.springfield.repository.GenericRepositoryImpl;

public class SpringJpaRepositoryFactoryBean<R extends Repository<T, ID>, T, ID extends Serializable> 
extends JpaRepositoryFactoryBean<R, T, ID>{
	
    protected final static Log logger = LogFactory.getLog(SpringJpaRepositoryFactoryBean.class);

    private Class<T> entityClass;
    
	public SpringJpaRepositoryFactoryBean(){
		//super.setLazyInit(true);
    	//super.setTransactionManager(null);
    	//super.setRepositoryInterface((Class<? extends R>) RestRepository.class);
    }

	public void setEntityClass(Class<T> entityClass) {
		this.entityClass = entityClass;
	}
	
    @Override
	protected RepositoryFactorySupport createRepositoryFactory(EntityManager entityManager) {
    	//logger.debug("createRepositoryFactory");
    	EntityRepositoryFactory r = new EntityRepositoryFactory(entityManager);
    	r.setEntityClass(entityClass);
    	return r;
	}

    private static class EntityRepositoryFactory extends JpaRepositoryFactory {

        private final EntityManager entityManager;
        private Class<?> entityClass;
        
    	public void setEntityClass(Class<?> entityClass) {
    		this.entityClass = entityClass;
    	}
    	
    	public EntityRepositoryFactory(EntityManager entityManager) {
    		super(entityManager);
    		this.entityManager = entityManager;
    	}

    	
    	@Override
    	@SuppressWarnings({ "rawtypes", "unchecked" })
    	protected Object getTargetRepository(RepositoryMetadata metadata) {
        	Class<?> entityType = metadata.getDomainType();
        	if(entityType.equals(Object.class)){
        		entityType = entityClass;
    		}
        	/*
    		Class<?> repositoryInterface = metadata.getRepositoryInterface();
    		JpaEntityInformation<?, Serializable> entityInformation = getEntityInformation(metadata.getDomainType());
        	logger.debug("entityInformation "+entityInformation);

        	Object info2 = JpaEntityInformationSupport.getEntityInformation(domainType, entityManager);
        	logger.debug("entityInformation "+info2);
        	*/
        	
    		GenericRepositoryImpl repository = new GenericRepositoryImpl(entityType, entityManager);
    		return repository;
    	}
    	
    	@Override
    	protected Class<?> getRepositoryBaseClass(RepositoryMetadata metadata) {
    		return GenericRepositoryImpl.class;
    	}
    }

}
/*23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [38] - ===================================================
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - consumerRepository=com.u2ware.springfield.repository.RestRepositoryImpl@4690d4d1
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - dataSource=org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseFactory$EmbeddedDataSourceProxy@4fdf263a
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - entityManagerFactory=org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean@4346d2cd
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.aop.config.internalAutoProxyCreator=proxyTargetClass=false; optimize=false; opaque=false; exposeProxy=false; frozen=false
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.context.annotation.ConfigurationClassPostProcessor.enhancedConfigurationProcessor=org.springframework.context.annotation.ConfigurationClassPostProcessor$EnhancedConfigurationBeanPostProcessor@5eb9b33c
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.context.annotation.ConfigurationClassPostProcessor.importAwareProcessor=org.springframework.context.annotation.ConfigurationClassPostProcessor$ImportAwareBeanPostProcessor@71a658b9
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.context.annotation.internalAutowiredAnnotationProcessor=org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor@600b13f9
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.context.annotation.internalCommonAnnotationProcessor=org.springframework.context.annotation.CommonAnnotationBeanPostProcessor@774c71b1
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.context.annotation.internalConfigurationAnnotationProcessor=org.springframework.context.annotation.ConfigurationClassPostProcessor@3bbc58a7
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.context.annotation.internalPersistenceAnnotationProcessor=org.springframework.orm.jpa.support.PersistenceAnnotationBeanPostProcessor@4a605a94
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.context.annotation.internalRequiredAnnotationProcessor=org.springframework.beans.factory.annotation.RequiredAnnotationBeanPostProcessor@7bdad0b1
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.transaction.annotation.ProxyTransactionManagementConfiguration=org.springframework.transaction.annotation.ProxyTransactionManagementConfiguration$$EnhancerBySpringCGLIB$$bd3bab73@5abb53fd
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - org.springframework.transaction.config.internalTransactionAdvisor=org.springframework.transaction.interceptor.BeanFactoryTransactionAttributeSourceAdvisor: advice bean 'null'
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - persistenceUnitManager=org.springframework.orm.jpa.persistenceunit.DefaultPersistenceUnitManager@40873594
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - restRepositoryTestConfig=com.u2ware.springfield.repository.RestRepositoryTestConfig$$EnhancerBySpringCGLIB$$ec4c36e6@6ccea8a4
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - transactionAttributeSource=org.springframework.transaction.annotation.AnnotationTransactionAttributeSource@aa26d6b1
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - transactionInterceptor=org.springframework.transaction.interceptor.TransactionInterceptor@73a1deca
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [42] - transactionManager=org.springframework.orm.jpa.JpaTransactionManager@829db15
23:48:14 [main] WARN  c.u.s.repository.RestRepositoryTest # setup [44] - ===================================================
*/