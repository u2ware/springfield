package com.u2ware.springfield.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;

public class HandlerLoggingInterceptor implements HandlerInterceptor{

	protected final Log logger = LogFactory.getLog(getClass());
	
	public boolean preHandle(HttpServletRequest request,HttpServletResponse response, Object handler) throws Exception {
		logger.info("preHandle : "+request.getRequestURI()+" "+handler);
		return true;
	}

	public void postHandle(HttpServletRequest request,HttpServletResponse response, Object handler,ModelAndView modelAndView) throws Exception {
		logger.info("postHandle : "+request.getRequestURI()+" "+handler);
	}

	public void afterCompletion(HttpServletRequest request,HttpServletResponse response, Object handler, Exception ex)throws Exception {
		logger.info("afterCompletion : "+request.getRequestURI()+" "+handler);
	}


}
