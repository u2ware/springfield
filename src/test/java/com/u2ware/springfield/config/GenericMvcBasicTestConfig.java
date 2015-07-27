package com.u2ware.springfield.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.data.web.config.EnableSpringDataWebSupport;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import com.u2ware.springfield.repository.support.EnableSpringJpaBaseConfiguration;

@Configuration
@EnableSpringJpaBaseConfiguration("sample.application")
@EnableJpaRepositories("sample.application")
@EnableWebMvc
@EnableSpringDataWebSupport
@ComponentScan("sample.application")
@EnableGenericMvc("sample.application")
public class GenericMvcBasicTestConfig  {


}
