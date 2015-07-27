package com.u2ware.springfield.repository.support;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.util.ClassUtils;


public class SpringJpaBaseConfigurationRegistrar implements ImportBeanDefinitionRegistrar{


	@Override
	public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {

		Map<String, Object> attributeMap = importingClassMetadata.getAnnotationAttributes(EnableSpringJpaBaseConfiguration.class.getName());
		AnnotationAttributes attributes = new AnnotationAttributes(attributeMap);
		
		String[] value = attributes.getStringArray("value");
		String[] basePackages = attributes.getStringArray("basePackages");
		Class<?>[] basePackageClasses = attributes.getClassArray("basePackageClasses");

		List<String> packages = null;		
		
		// Default configuration - return package of annotated class
		if (value.length == 0 && basePackages.length == 0 && basePackageClasses.length == 0) {
			String className = importingClassMetadata.getClassName();
			packages = Collections.singletonList(ClassUtils.getPackageName(className));
		}else{
			packages = new ArrayList<String>();
			packages.addAll(Arrays.asList(value));
			packages.addAll(Arrays.asList(basePackages));
			for (Class<?> typeName : basePackageClasses) {
				packages.add(ClassUtils.getPackageName(typeName));
			}
		}
		
		String name = SpringJpaBaseConfiguration.class.getName();
		BeanDefinitionBuilder bean = BeanDefinitionBuilder
				.rootBeanDefinition(SpringJpaBaseConfiguration.class)
				.addConstructorArgValue(packages);
		registry.registerBeanDefinition(name, bean.getBeanDefinition());
		
	}
}
