package com.u2ware.springfield.config.support;

import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.EnvironmentAware;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.Environment;
import org.springframework.core.env.PropertiesPropertySource;
import org.springframework.util.ClassUtils;

@Configuration
public class BaseConfiguration implements EnvironmentAware {

	protected final Logger logger = LoggerFactory.getLogger(getClass());
	
	public Properties properties;

	public void setProperties(Properties properties) {
		this.properties = properties;
	}
	
	@Override
	public void setEnvironment(Environment environment) {
		if(properties != null && ClassUtils.isAssignableValue(ConfigurableEnvironment.class, environment)){
			ConfigurableEnvironment e = (ConfigurableEnvironment)environment;
			e.getPropertySources().addFirst(new PropertiesPropertySource("springfieldEnvironment", properties));
		}
	}
}