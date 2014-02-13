package com.u2ware.springfield.config.support;

import java.util.Locale;

import org.springframework.context.EnvironmentAware;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.MessageSourceAccessor;
import org.springframework.context.support.ReloadableResourceBundleMessageSource;
import org.springframework.core.env.Environment;

import com.u2ware.springfield.support.resource.ResourcePatternResolverBean;

@Configuration
public class BaseMessageConfiguration implements EnvironmentAware{

	public Environment environment;
	public String[] packagesToScan;

	@Override
	public void setEnvironment(Environment environment) {
		this.environment = environment;
	}
	public void setPackagesToScan(String[] packagesToScan) {
		this.packagesToScan = packagesToScan;
	}

	@Bean
	public ResourcePatternResolverBean messageSourceBasenames()  throws Exception{
		ResourcePatternResolverBean bean = new ResourcePatternResolverBean();
		bean.setPackagesToScan(packagesToScan);
		bean.setResourcePatterns("/**/messages.xml");
		return bean;
	}
	@Bean
	public ReloadableResourceBundleMessageSource messageSource()  throws Exception{
		ReloadableResourceBundleMessageSource bean = new ReloadableResourceBundleMessageSource();
		bean.setDefaultEncoding("UTF-8");
		String[] basenames = messageSourceBasenames().getFilenames();
		if(basenames != null){
			bean.setBasenames(basenames);
		}else{
			String path = "classpath:com/u2ware/springfield/view/thymeleaf/messages";
			bean.setBasenames(path);
		}
		return bean;
	}
	@Bean
	public MessageSourceAccessor messageSourceAccessor()  throws Exception{
		MessageSourceAccessor bean = new MessageSourceAccessor(messageSource(), 
				environment.getProperty(
						"springfield.baseLocale", 
						Locale.class, 
						Locale.KOREA
					)
		);
		return bean;
	}



}
