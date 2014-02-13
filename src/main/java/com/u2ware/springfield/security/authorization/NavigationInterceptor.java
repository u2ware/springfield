package com.u2ware.springfield.security.authorization;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.servlet.ModelAndView;

public class NavigationInterceptor implements org.springframework.web.servlet.HandlerInterceptor{

	@Autowired
    private NavigationFactory navigationFactory;

	public void setNavigationFactory(NavigationFactory navigationFactory) {
		this.navigationFactory = navigationFactory;
	}
	
	public boolean preHandle(HttpServletRequest request,HttpServletResponse response, Object handler) throws Exception {
		
		if(request.getAttribute(Navigation.OBJECT_NAME) != null) return true;
		Navigation navigation = navigationFactory.resolveNavigation(request);
		request.setAttribute(Navigation.OBJECT_NAME, navigation);
		
		return true;
	}

	public void postHandle(HttpServletRequest request,HttpServletResponse response, Object handler,ModelAndView modelAndView) throws Exception {
	}

	public void afterCompletion(HttpServletRequest request,HttpServletResponse response, Object handler, Exception ex)throws Exception {
	}
}
