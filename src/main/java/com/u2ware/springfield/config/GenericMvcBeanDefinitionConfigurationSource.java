package com.u2ware.springfield.config;

import org.springframework.core.env.Environment;
import org.springframework.core.io.ResourceLoader;


public class GenericMvcBeanDefinitionConfigurationSource {

	private Iterable<String> basePackages;
	private ResourceLoader resourceLoader;
	private Environment environment;
	private Object source;

	private String entityManagerFactoryRef;
	private String transactionManagerRef;
	
	public Iterable<String> getBasePackages() {
		return basePackages;
	}
	public void setBasePackages(Iterable<String> basePackages) {
		this.basePackages = basePackages;
	}
	public ResourceLoader getResourceLoader() {
		return resourceLoader;
	}
	public void setResourceLoader(ResourceLoader resourceLoader) {
		this.resourceLoader = resourceLoader;
	}
	public Environment getEnvironment() {
		return environment;
	}
	public void setEnvironment(Environment environment) {
		this.environment = environment;
	}
	public Object getSource() {
		return source;
	}
	public void setSource(Object source) {
		this.source = source;
	}
	public String getEntityManagerFactoryRef() {
		return entityManagerFactoryRef;
	}
	public void setEntityManagerFactoryRef(String entityManagerFactoryRef) {
		this.entityManagerFactoryRef = entityManagerFactoryRef;
	}
	public String getTransactionManagerRef() {
		return transactionManagerRef;
	}
	public void setTransactionManagerRef(String transactionManagerRef) {
		this.transactionManagerRef = transactionManagerRef;
	}
}
