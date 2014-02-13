package com.u2ware.springfield.config.test2;

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;

import com.u2ware.springfield.config.ModulesConfig;

public class ContextWebmvcBaseDifinitionParser {
	
	private void registerBeanDefinition(BeanDefinitionRegistry registry, String beanName, BeanDefinition beanDefinition) {
		
	}
	private void registerBeanDefinition(BeanDefinitionRegistry registry, String beanName) {
		
	}
	private String getSpringfieldConfigurerValue(String key){
		return "#{"+Configurer.BEAN_NAME+"['"+key+"']}";
	}


	///////////////////////////////////////
	//
	///////////////////////////////////////
	public void addSmartValidatorConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {
		
		String beanName = "mvcValidator";
		if(registry.isBeanNameInUse(beanName)) {registry.removeBeanDefinition(beanName);}

		
		BeanDefinition messageInterpolator = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.validation.ValidationMessageInterpolator")
				.getRawBeanDefinition();
		//this.registerBeanDefinition(registry, "messageInterpolator", messageInterpolator);
		
		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("org.springframework.validation.beanvalidation.LocalValidatorFactoryBean")
				.addPropertyValue("messageInterpolator", messageInterpolator)
				.getRawBeanDefinition();
				
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	public void addSpringfieldMessageSourceBasenamesConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {
		
		String beanName = "springfieldMessageSourceBasenames";

		String[] packages = new String[]{modulesConfig.getBasePackage()};
		if(registry.isBeanNameInUse(beanName)) {
		
			BeanDefinition savedBean = registry.getBeanDefinition(beanName);
			
			String[] savedPackages = (String[])
					(savedBean.getPropertyValues().getPropertyValue("packagesToScan").getValue());
		
			if(savedPackages != null){
				String[] newPackage = new String[packages.length+savedPackages.length];
				
				for(int i = 0; i < savedPackages.length; i++){
					newPackage[i] = savedPackages[i];
				}
				for(int i = 0; i < packages.length; i++){
					newPackage[i + savedPackages.length] = packages[i];
				}
				packages = newPackage;
			}
			registry.removeBeanDefinition(beanName);
		}

		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.support.resource.ResourcePatternResolverBean")
				.addPropertyValue("packagesToScan", packages)
				.addPropertyValue("ResourcePatterns", "/**/messages.xml")
				.getRawBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	
	public void addSpringfieldMessageSourceConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {
	
		
		String beanName = "springfieldMessageSource";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}

		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("org.springframework.context.support.ReloadableResourceBundleMessageSource")
				.addPropertyValue("defaultEncoding", "UTF-8")
				.addPropertyValue("basenames", "#{springfieldMessageSourceBasenames.filenames}")
				.getRawBeanDefinition();
				
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	
	public void addSpringfieldMessageSourceAccessorConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {
		
		String beanName = "springfieldMessageSourceAccessor";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}

		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("org.springframework.context.support.MessageSourceAccessor")
				.addConstructorArgReference("springfieldMessageSource")
				.addConstructorArgValue( getSpringfieldConfigurerValue(Configurer.BASE_LOCALE))
				.getRawBeanDefinition();
				
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}

	
	
	
}
