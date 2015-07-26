package com.u2ware.springfield.config;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.EnvironmentAware;
import org.springframework.context.ResourceLoaderAware;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.env.Environment;
import org.springframework.core.io.ResourceLoader;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.util.ClassUtils;

public class GenericMvcBeanDefinitionRegistrar implements ImportBeanDefinitionRegistrar, ResourceLoaderAware, EnvironmentAware{

	protected Log logger = LogFactory.getLog(getClass());
	
	private Iterable<String> basePackages;
	private ResourceLoader resourceLoader;
	private Environment environment;
	private Object source;
	private String entityManagerFactoryRef;
	private String transactionManagerRef;
	
	@Override
	public void setResourceLoader(ResourceLoader resourceLoader) {
		this.resourceLoader = resourceLoader;
	}
	@Override
	public void setEnvironment(Environment environment) {
		this.environment = environment;
	}
	
	@Override
	public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {
		
		try {
			AnnotationMetadata metadata = resolveAnnotationMetadata(importingClassMetadata);
			Map<String, Object> attributeMap = metadata.getAnnotationAttributes(EnableGenericMvc.class.getName());
			AnnotationAttributes attributes = new AnnotationAttributes(attributeMap);
			
			this.basePackages = resolveBasePackages(metadata, attributes);
			this.source = resolveSource(metadata, attributes);
			this.entityManagerFactoryRef = resolveEntityManagerFactoryRef(metadata, attributes);
			this.transactionManagerRef = resolveTransactionManagerRef(metadata, attributes);
			
			logger.debug("source : "+source);
			logger.debug("basePackages : "+basePackages);
			logger.debug("entityManagerFactoryRef : "+entityManagerFactoryRef);
			logger.debug("transactionManagerRef : "+transactionManagerRef);
			logger.debug("resourceLoader : "+resourceLoader);
			logger.debug("environment : "+environment);
			

			GenericMvcBeanDefinitionConfigurationSource configSource = new GenericMvcBeanDefinitionConfigurationSource();
			configSource.setBasePackages(this.basePackages);
			configSource.setResourceLoader(this.resourceLoader);
			configSource.setSource(this.source);
			configSource.setEnvironment(this.environment);
			configSource.setEntityManagerFactoryRef(this.entityManagerFactoryRef);
			configSource.setTransactionManagerRef(this.transactionManagerRef);
			
			new GenericMvcBeanDefinitionConfiguration().registerBeanDefinitions(registry, configSource);

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	///////////////////////
	//
	//////////////////////
	protected AnnotationMetadata resolveAnnotationMetadata(AnnotationMetadata importingClassMetadata){
		return importingClassMetadata;
	}
	
	protected Iterable<String> resolveBasePackages(AnnotationMetadata metadata, AnnotationAttributes attributes){

		Set<String> packages = null;
		String[] value = attributes.getStringArray("value");
		String[] basePackages = attributes.getStringArray("basePackages");
		Class<?>[] basePackageClasses = attributes.getClassArray("basePackageClasses");

		// Default configuration - return package of annotated class
		if (value.length == 0 && basePackages.length == 0 && basePackageClasses.length == 0) {
			String className = metadata.getClassName();
			packages = Collections.singleton(ClassUtils.getPackageName(className));
		}else{
			packages = new HashSet<String>();
			packages.addAll(Arrays.asList(value));
			packages.addAll(Arrays.asList(basePackages));
			for (Class<?> typeName : basePackageClasses) {
				packages.add(ClassUtils.getPackageName(typeName));
			}
		}
		return packages;
	}
	protected Object resolveSource(AnnotationMetadata metadata, AnnotationAttributes attributes){
		return metadata;
	}
	protected String resolveEntityManagerFactoryRef(AnnotationMetadata metadata, AnnotationAttributes attributes){
		return attributes.getString("entityManagerFactoryRef");
	}
	protected String resolveTransactionManagerRef(AnnotationMetadata metadata, AnnotationAttributes attributes){
		return attributes.getString("transactionManagerRef");
	}
}
