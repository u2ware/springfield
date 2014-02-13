package com.u2ware.springfield.config.support;

import java.util.Properties;

import org.springframework.context.EnvironmentAware;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;
import org.springframework.core.env.Environment;
import org.springframework.core.io.Resource;

import com.u2ware.springfield.security.authorization.NavigationFactory;
import com.u2ware.springfield.support.resource.ResourcePatternResolverBean;

@Configuration
@ImportResource("classpath:com/u2ware/springfield/config/support/WebmvcSecurityConfiguration.xml")
public class WebmvcSecurityConfiguration implements EnvironmentAware{

	private Environment env;
	
	public String basePackage;

	public void setBasePackage(String basePackage) {
		this.basePackage = basePackage;
	}

	@Override
	public void setEnvironment(Environment env) {
		this.env = env;
	}

	@Bean
	public Properties springfieldWebmvcSecurityProperties(){
		Properties p = new Properties();
		
		p.put("springfield.security.formPage",       env.getProperty("springfield.security.formPage", "/security/user/loginForm.html"));
		p.put("springfield.security.formUsername",   env.getProperty("springfield.security.formUsername", "j_username"));
		p.put("springfield.security.formPassword",   env.getProperty("springfield.security.formPassword", "j_password"));
		p.put("springfield.security.formRememberme", env.getProperty("springfield.security.formRememberme", "_spring_security_remember_me"));
		p.put("springfield.security.loginUrl",       env.getProperty("springfield.security.loginUrl", "/j_spring_security_check"));
		p.put("springfield.security.logoutUrl",      env.getProperty("springfield.security.logoutUrl", "/j_spring_security_logout"));
		
		return p;
	}
	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	@Bean
	public ResourcePatternResolverBean springfieldWebmvcSecurityNavigationFactoryConfigLocations(){
		ResourcePatternResolverBean b = new ResourcePatternResolverBean();
		b.setPackagesToScan(basePackage);
		b.setResourcePatterns("/**/navigation.xml");
		return b;
	}

	@Bean
	public NavigationFactory springfieldWebmvcSecurityNavigationFactory(){
		NavigationFactory b = new NavigationFactory();
		Resource[] r = springfieldWebmvcSecurityNavigationFactoryConfigLocations().getResources();
		if(r != null && r.length > 0){
			b.setConfigLocation(r[0]);
		}
		return b;
	}

	
	
}
