package com.u2ware.springfield.repository.support;

import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;



@Configuration
@EnableSpringJpaBaseConfiguration("sample.application")
@EnableJpaRepositories(
		basePackages="sample.application"
		,repositoryFactoryBeanClass=SpringJpaRepositoryFactoryBean.class
		//,transactionManagerRef="transactionManager"
)
public class SpringJpaRepositoryFactoryBeanTestConfig {


}
