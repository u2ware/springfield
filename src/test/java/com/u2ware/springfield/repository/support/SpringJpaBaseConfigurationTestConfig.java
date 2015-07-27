package com.u2ware.springfield.repository.support;

import javax.sql.DataSource;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.TransactionManagementConfigurer;

@Configuration
@EnableSpringJpaBaseConfiguration(basePackages="sample.application")
public class SpringJpaBaseConfigurationTestConfig implements TransactionManagementConfigurer, BeanFactoryAware{

	private PlatformTransactionManager txManager;
	
	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		txManager = beanFactory.getBean("transactionManager", PlatformTransactionManager.class);
	}
	@Override
	public PlatformTransactionManager annotationDrivenTransactionManager() {
		return txManager;
	}

	@Bean
	public DataSource otherDataSource(){
		DataSource ds = new EmbeddedDatabaseBuilder().setType(EmbeddedDatabaseType.H2).build();
		return ds;
	}
	@Bean
	public DataSourceTransactionManager otherTransactionManager(){
		DataSourceTransactionManager ds = new DataSourceTransactionManager(otherDataSource());
		return ds;
	}

}
