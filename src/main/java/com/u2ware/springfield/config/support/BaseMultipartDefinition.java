package com.u2ware.springfield.config.support;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;

import com.u2ware.springfield.config.ModulesConfig;

/**
 * 
 * @deprecated
 *
 */
public class BaseMultipartDefinition {

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	protected void registerBeanDefinition(BeanDefinitionRegistry registry, String beanName, BeanDefinition beanDefinition){
		registry.registerBeanDefinition(beanName, beanDefinition);
		logger.debug(beanName+" = "+beanDefinition.getBeanClassName());
	}
	
	protected void registerBeanDefinition(BeanDefinitionRegistry registry, String beanName){
		logger.debug(beanName+" = <<bean name used in registry >>");
	}

	/////////////////////////////////////
	//
	/////////////////////////////////////
	public void addSpringfieldMultipartResolver(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {
		
		String beanName = "springfieldMultipartResolver";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return;}

		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("org.springframework.web.multipart.commons.CommonsMultipartResolver")
				.addPropertyValue("maxUploadSize", "#{springfieldEnvironment['springfield.multipart.size']}")
				.getRawBeanDefinition();
				
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	
	public void addSpringfieldMultipartFileHandler(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {
		
		String beanName = "springfieldMultipartFileHandler";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return;}

		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.support.multipart.MultipartFileHandlerImpl")
				.addPropertyValue("directory", "#{springfieldEnvironment['springfield.multipart.location']}")
				.getRawBeanDefinition();
				
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	
}
