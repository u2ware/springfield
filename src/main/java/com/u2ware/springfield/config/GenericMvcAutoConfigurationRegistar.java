package com.u2ware.springfield.config;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.boot.autoconfigure.AutoConfigurationPackages;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.core.type.StandardAnnotationMetadata;

public class GenericMvcAutoConfigurationRegistar extends GenericMvcBeanDefinitionRegistrar implements BeanFactoryAware{

	private BeanFactory beanFactory;

	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

	protected AnnotationMetadata resolveAnnotationMetadata(AnnotationMetadata importingClassMetadata){
		return new StandardAnnotationMetadata(EnableGenericMvcConfiguration.class, true);
	}

	protected Iterable<String> resolveBasePackages(AnnotationMetadata metadata, AnnotationAttributes attributes){
		return AutoConfigurationPackages.get(beanFactory);
	}
	
	@EnableGenericMvc
	private static class EnableGenericMvcConfiguration {
	}
	
	
}
