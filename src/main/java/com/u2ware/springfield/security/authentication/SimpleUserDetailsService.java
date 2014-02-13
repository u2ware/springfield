package com.u2ware.springfield.security.authentication;

import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ApplicationContextException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;

public class SimpleUserDetailsService implements UserDetailsService , ApplicationContextAware, InitializingBean{

	protected final Log logger = LogFactory.getLog(getClass());

	private ApplicationContext applicationContext;
	private UserDetailsRepository simpleUserDetailsRepository;
	
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		this.applicationContext = applicationContext;
	}
	public void setSimpleUserDetailsRepository(UserDetailsRepository simpleUserDetailsRepository) {
		this.simpleUserDetailsRepository = simpleUserDetailsRepository;
	}
	
	/////////////////////////////////////////////////
	//
	/////////////////////////////////////////////////
	@Override
	public void afterPropertiesSet() throws Exception {
		if(simpleUserDetailsRepository != null) return;
		simpleUserDetailsRepository = autoDetectSimpleUserDetailsRepository();
	}
	private UserDetailsRepository autoDetectSimpleUserDetailsRepository() {
        Map<String,?> beans = applicationContext.getBeansOfType(UserDetailsRepository.class);

        if (beans.size() == 0) {
            throw new ApplicationContextException("No UserDetailsRepository registered.");
        } else if (beans.size() > 1) {
            throw new ApplicationContextException("More than one UserDetailsRepository registered.");
        }
        return (UserDetailsRepository) beans.values().toArray()[0];
	}

	@Override
	public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
		try{
			return simpleUserDetailsRepository.loadUserByUsername(username);
		}catch(Exception e){
            throw new UsernameNotFoundException("Username "+username+" not found", e);
		}
	}
}
