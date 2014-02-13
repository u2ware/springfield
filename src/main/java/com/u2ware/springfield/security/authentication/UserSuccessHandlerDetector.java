package com.u2ware.springfield.security.authentication;

import java.io.IOException;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.security.web.authentication.SavedRequestAwareAuthenticationSuccessHandler;
import org.springframework.util.ClassUtils;

public class UserSuccessHandlerDetector extends SavedRequestAwareAuthenticationSuccessHandler implements ApplicationContextAware, InitializingBean{
	
	protected final Logger logger = LoggerFactory.getLogger(getClass());

	private ApplicationContext applicationContext;
	private AuthenticationSuccessHandler handler;
	
	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		this.applicationContext = applicationContext;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
        try{
    		Map<String,AuthenticationSuccessHandler> beans = applicationContext.getBeansOfType(AuthenticationSuccessHandler.class);

    		for(AuthenticationSuccessHandler h : beans.values()){
    			if(! ClassUtils.isAssignableValue(getClass(), h)){
    				this.handler = h;
    				break;
    			}
    		}
        }catch(Exception e){
        	logger.debug("", e);
        }
	}

	@Override
	public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response, Authentication authentication) throws IOException, ServletException {
    	logger.debug("onAuthenticationSuccess");
		super.onAuthenticationSuccess(request, response, authentication);
		if(handler == null) return ;
		handler.onAuthenticationSuccess(request, response, authentication);
	}
}
