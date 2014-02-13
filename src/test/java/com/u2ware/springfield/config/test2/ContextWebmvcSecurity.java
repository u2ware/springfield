package com.u2ware.springfield.config.test2;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;



@Configuration
@ImportResource("classpath:com/u2ware/springfield/config/support/context-webmvc-security.xml")
public class ContextWebmvcSecurity{

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	//private SpringfieldConfigurer configurer;

	public void setConfigurer(Configurer configurer) {
		//this.configurer = configurer;
	}
	
	

	/*
	
	
	
	@Bean @SuppressWarnings("rawtypes")
	public AccessDecisionManager accessDecisionManager(){
		List<AccessDecisionVoter> decisionVoters = new ArrayList<AccessDecisionVoter>();
		decisionVoters.add(new AuthorityAttributeVoter());
		
		AccessDecisionManager b = new AccessDecisionManager(decisionVoters);
		b.setNavigationFactory(springfieldNavigationFactory());
		return b;
	}
	
	@Bean
	public LogonServiceDelegator userService(){
		LogonServiceDelegator b = new LogonServiceDelegator();
		b.setDefaultTargetUrl("/security/login/loginForm.html");
		b.setDefaultFailureUrl("/security/login/loginForm.html?errorCode=authentication-failure");
		return b;
	}
	
	@Bean
	public RemembermeServiceDelegator rememberMeService(){
		RemembermeServiceDelegator b = new RemembermeServiceDelegator("springfieldRememberMe", userService());
		b.setParameter("_spring_security_remember_me");
		b.setTokenValiditySeconds(3600);
		return b;
	}
	
	@Bean
	public SaltSource saltSource(){
		ReflectionSaltSource b = new ReflectionSaltSource();
		b.setUserPropertyToUse("salt");
		return b;
	}
	
	@Bean
	public ShaPasswordEncoder passwordEncoder(){
		ShaPasswordEncoder b = new ShaPasswordEncoder(512);
		return b;
	}
	
	*/
	
	
}
