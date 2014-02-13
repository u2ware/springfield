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
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.AuthenticationFailureHandler;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationFailureHandler;
import org.springframework.util.ClassUtils;

public class UserFailureHandlerDetector extends SimpleUrlAuthenticationFailureHandler implements ApplicationContextAware, InitializingBean{
	
	protected final Logger logger = LoggerFactory.getLogger(getClass());

	private ApplicationContext applicationContext;
	private AuthenticationFailureHandler handler;
	
	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		this.applicationContext = applicationContext;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
        try{
    		Map<String,AuthenticationFailureHandler> beans = applicationContext.getBeansOfType(AuthenticationFailureHandler.class);

    		for(AuthenticationFailureHandler h : beans.values()){
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
	public void onAuthenticationFailure(HttpServletRequest request, HttpServletResponse response, AuthenticationException exception)throws IOException, ServletException {
    	logger.debug("onAuthenticationFailure");
		super.onAuthenticationFailure(request, response, exception);
		if(handler == null) return ;
		handler.onAuthenticationFailure(request, response, exception);
	}
}
