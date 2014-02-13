package com.u2ware.springfield.config.support;

import java.io.File;

import org.springframework.context.EnvironmentAware;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import org.springframework.web.multipart.commons.CommonsMultipartResolver;

import com.u2ware.springfield.support.multipart.MultipartFileHandler;
import com.u2ware.springfield.support.multipart.MultipartFileHandlerImpl;

@Configuration
public class BaseMultipartConfiguration implements EnvironmentAware{

	public Environment environment;

	
	@Override
	public void setEnvironment(Environment environment) {
		this.environment = environment;
	}

	@Bean
	public CommonsMultipartResolver filterMultipartResolver(){
		CommonsMultipartResolver b = new CommonsMultipartResolver();
		b.setMaxUploadSize(
			environment.getProperty(
					"springfield.multipart.size", 
					Long.class, 
					new Long(-1)
					)
		);
		return b;
	}
	
	@Bean
	public MultipartFileHandler springfieldBaseMultipartFileHandler(){
		MultipartFileHandlerImpl b = new MultipartFileHandlerImpl();
		b.setDirectory(
			environment.getProperty(
					"springfield.multipart.location",
					File.class, 
					new File( System.getProperty("java.io.tmpdir"), "/springfield")  
			)
		);
		return b;
	}
}
