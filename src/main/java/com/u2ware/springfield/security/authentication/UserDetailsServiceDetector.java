package com.u2ware.springfield.security.authentication;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.util.ClassUtils;

public class UserDetailsServiceDetector implements UserDetailsService, ApplicationContextAware, InitializingBean{

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	private ApplicationContext applicationContext;
	private UserDetailsService userService;
	
	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		this.applicationContext = applicationContext;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
        try{
    		Map<String,UserDetailsService> beans = applicationContext.getBeansOfType(UserDetailsService.class);
    		
    		for(UserDetailsService s : beans.values()){
    			if(! ClassUtils.isAssignableValue(getClass(), s)){
    				this.userService = s;
    				break;
    			}
    		}

        }catch(Exception e){
    		logger.debug("", e);
        }
	}

	@Override
	public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
		
		logger.debug("loadUserByUsername");
		
		if(userService == null){
			Collection<GrantedAuthority> authorities = new HashSet<GrantedAuthority>();
			authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
			//authorities.add(new SimpleGrantedAuthority("ROLE_ANONYMOUS"));
			//authorities.add(new SimpleGrantedAuthority("ROLE_ADMIN"));
			return new User(username, "password", authorities);
		}
		return userService.loadUserByUsername(username);
	}
}
