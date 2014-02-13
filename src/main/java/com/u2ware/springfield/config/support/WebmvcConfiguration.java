package com.u2ware.springfield.config.support;

import java.util.List;
import java.util.Locale;

import javax.validation.MessageInterpolator;

import org.springframework.context.EnvironmentAware;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import org.springframework.data.web.PageableHandlerMethodArgumentResolver;
import org.springframework.data.web.SortHandlerMethodArgumentResolver;
import org.springframework.format.FormatterRegistry;
import org.springframework.validation.Validator;
import org.springframework.validation.beanvalidation.LocalValidatorFactoryBean;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.servlet.config.annotation.ContentNegotiationConfigurer;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;
import org.springframework.web.servlet.handler.MappedInterceptor;
import org.springframework.web.servlet.i18n.CookieLocaleResolver;
import org.springframework.web.servlet.i18n.LocaleChangeInterceptor;

import com.u2ware.springfield.controller.HandlerInterceptor;
import com.u2ware.springfield.controller.HandlerMapping;
import com.u2ware.springfield.security.authorization.NavigationInterceptor;
import com.u2ware.springfield.support.conversion.StringTrimFormatter;
import com.u2ware.springfield.validation.ValidationMessageInterpolator;

@Configuration
@EnableWebMvc
public class WebmvcConfiguration extends WebMvcConfigurerAdapter implements EnvironmentAware{

	public Environment environment;

	@Override
	public void setEnvironment(Environment environment) {
		this.environment = environment;
	}

	@Override
	public void addArgumentResolvers(List<HandlerMethodArgumentResolver> argumentResolvers) {
		
		SortHandlerMethodArgumentResolver r1 = new SortHandlerMethodArgumentResolver();
		r1.setSortParameter("model_query_pageable.pageSort");

		PageableHandlerMethodArgumentResolver r2 = new PageableHandlerMethodArgumentResolver(r1);
		r2.setPageParameterName("model_query_pageable.pageNumber");
		r2.setSizeParameterName("model_query_pageable.pageSize");
		
		argumentResolvers.add(r2);
	}
		
	@Override
	public void addResourceHandlers(ResourceHandlerRegistry registry) {
		 registry.addResourceHandler("/resources/**").addResourceLocations("/resources/");
	}

	@Override
	public void addFormatters(FormatterRegistry registry) {
		registry.addFormatter(new StringTrimFormatter());
	}

	@Override
	public void configureContentNegotiation(ContentNegotiationConfigurer configurer) {

	}

	
	@Override
	public Validator getValidator() {
		return springfieldWebmvcValidator();
	}

	@Bean
	public LocalValidatorFactoryBean springfieldWebmvcValidator() {
		LocalValidatorFactoryBean v = new LocalValidatorFactoryBean();
		v.setMessageInterpolator(springfieldWebmvcValidatorMessageInterpolator());
		return v;
	}
	@Bean
	public MessageInterpolator springfieldWebmvcValidatorMessageInterpolator() {
		return new ValidationMessageInterpolator();
	}
	
	
	
	///////////////////////////////////
	//
	///////////////////////////////////
	@Bean
	public HandlerMapping springfieldWebmvcHandlerMapping() {
		HandlerMapping b = new HandlerMapping();
		return b;
	}

	
	///////////////////////////////////
	//
	///////////////////////////////////
	@Bean
	public MappedInterceptor springfieldWebmvcMappedLocaleChangeInterceptor(){
		MappedInterceptor b = new MappedInterceptor(null, springfieldWebmvcMappedLocaleChangeInterceptorBean());
		return b;
	}
	@Bean
	public LocaleChangeInterceptor springfieldWebmvcMappedLocaleChangeInterceptorBean(){
		LocaleChangeInterceptor b = new LocaleChangeInterceptor();
		return b;
	}

	
	///////////////////////////////////
	//
	///////////////////////////////////
	@Bean
	public MappedInterceptor springfieldWebmvcMappedLoggingInterceptor(){
		MappedInterceptor b = new MappedInterceptor(null, springfieldWebmvcMappedLoggingInterceptorBean());
		return b;
	}
	@Bean
	public HandlerInterceptor springfieldWebmvcMappedLoggingInterceptorBean(){
		HandlerInterceptor b = new HandlerInterceptor();
		return b;
	}

	///////////////////////////////////
	//
	///////////////////////////////////
	@Bean
	public MappedInterceptor springfieldWebmvcMappedNavigationInterceptor(){
		MappedInterceptor b = new MappedInterceptor(null, springfieldWebmvcMappedNavigationInterceptorBean());
		return b;
	}
	
	@Bean
	public NavigationInterceptor springfieldWebmvcMappedNavigationInterceptorBean(){
		NavigationInterceptor b = new NavigationInterceptor();
		return b;
	}
	
	
	
	///////////////////////////////////////////////////////////////////////////////
	// Bean Name 을 localeResolver 로 하지 않으면 AcceptHeaderLocaleResolver 가 발동한다.
	///////////////////////////////////////////////////////////////////////////////
	@Bean
	public CookieLocaleResolver localeResolver(){
		CookieLocaleResolver b = new CookieLocaleResolver();
		b.setDefaultLocale(
				environment.getProperty(
					"springfield.defaultLocale", 
					Locale.class, 
					Locale.KOREA
				)
		);
		return b;
	}
	
}
