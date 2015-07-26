package com.u2ware.springfield.config;

import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.boot.autoconfigure.data.rest.RepositoryRestMvcAutoConfiguration;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

@Configuration
@ConditionalOnWebApplication
@AutoConfigureAfter(RepositoryRestMvcAutoConfiguration.class)
@Import(GenericMvcAutoConfigurationRegistar.class)
public class GenericMvcAutoConfiguration {

}
