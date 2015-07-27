package com.u2ware.springfield.controller;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.data.web.config.EnableSpringDataWebSupport;
import org.springframework.validation.SmartValidator;
import org.springframework.web.multipart.MultipartResolver;
import org.springframework.web.multipart.commons.CommonsMultipartResolver;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;

import sample.application.consumer.Consumer;

import com.u2ware.springfield.service.GenericService;
import com.u2ware.springfield.service.GenericServiceTestConfig;


@Configuration
@Import(GenericServiceTestConfig.class)
@EnableWebMvc
@EnableSpringDataWebSupport
public class GenericControllerTestConfig extends WebMvcConfigurerAdapter{

	//WebMvcConfigurationSupport d;
	
    protected Log logger = LogFactory.getLog(getClass());
	
	@Bean
	public GenericController<Consumer> consumerController1(GenericService<Consumer> service, SmartValidator smartValidator){
		GenericControllerImpl<Consumer> controller = new GenericControllerImpl<Consumer>();
		controller.setDomainClass(Consumer.class);
		controller.setService(service);
		controller.setSmartValidator(smartValidator);
		controller.setRequestMappingRootPatternValue("consumer");
		controller.setRequestMappingUniquePatternValue("{id}");
		return controller;
	}

	@Bean
	public GenericHandlerMapping genericControllerMapping(){
		return new GenericHandlerMapping();
	}
	
	@Bean
	public MultipartResolver multipartResolver(){
		return new CommonsMultipartResolver();
	}
	
}
