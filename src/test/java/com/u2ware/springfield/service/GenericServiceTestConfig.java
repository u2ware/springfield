package com.u2ware.springfield.service;

import javax.validation.MessageInterpolator;

import org.modelmapper.ModelMapper;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

import sample.application.consumer.Consumer;

import com.u2ware.springfield.repository.GenericRepository;
import com.u2ware.springfield.repository.GenericRepositoryTestConfig;



@Configuration
@Import(GenericRepositoryTestConfig.class)
public class GenericServiceTestConfig {

	
	@Bean
	public GenericService<Consumer> consumerService1(GenericRepository<Consumer, String> consumerRepository){
		GenericServiceImpl<Consumer, Consumer, String> deskService = new GenericServiceImpl<Consumer, Consumer, String>();
		deskService.setRepository(consumerRepository);
		deskService.setDomainClass(Consumer.class);
		deskService.setModelMapper(modelMapper());
		return deskService;
	}

	@Bean
	public ModelMapper modelMapper(){
		return new ModelMapper();
	}
	
	@Bean
	public MessageInterpolator messageInterpolator(){
		return new ValidationMessageInterpolator();
	}
}
