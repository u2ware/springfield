package com.u2ware.springfield.repository;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.TransactionManagementConfigurer;

import sample.application.consumer.Consumer;

import com.u2ware.springfield.repository.support.EnableSpringJpaBaseConfiguration;



@Configuration
@EnableSpringJpaBaseConfiguration("sample.application.consumer")
@EnableJpaRepositories("sample.application.consumer")
public class GenericRepositoryTestConfig implements TransactionManagementConfigurer, BeanFactoryAware{

	private PlatformTransactionManager txManager;
	
	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		txManager = beanFactory.getBean("transactionManager", PlatformTransactionManager.class);
	}
	@Override
	public PlatformTransactionManager annotationDrivenTransactionManager() {
		return txManager;
	}

	@PersistenceContext
	protected EntityManager entityManager;
	
	@Bean
	public GenericRepository<Consumer,String> consumerRepository1(){

        GenericRepositoryImpl<Consumer, String> repository 
        = new GenericRepositoryImpl<Consumer, String>(Consumer.class, entityManager);

        return repository;
	}

	
	/*
	//@Bean
	@SuppressWarnings("unchecked")
	public GenericRepositoryFactoryBean<GenericRepository<Consumer,String>, Consumer,String> consumerRepository2(){

        GenericRepositoryFactoryBean<GenericRepository<Consumer,String>, Consumer,String> repository 
        = new GenericRepositoryFactoryBean<>();
        repository.setEntityClass(Consumer.class);
        //repository.setEntityManager(entityManager);
        //repository.setTransactionManager(null);
        repository.setRepositoryInterface((Class<? extends GenericRepository<Consumer, String>>) GenericRepository.class);
        return repository;
	}	
	
	//GenericRepositoryFactoryBean 을 Bean Definition 으로 등록 하지 않고 Java config 로 등록시에 발생함.
	//@Bean(name=SpringJpaRepositoryInterfaceAwareBeanPostProcessor.REPOSITORY_INTERFACE_POST_PROCESSOR)
	public InstantiationAwareBeanPostProcessorAdapter my(){
		return new SpringJpaRepositoryInterfaceAwareBeanPostProcessor();
	}
	*/
}
