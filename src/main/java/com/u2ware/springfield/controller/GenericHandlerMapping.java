package com.u2ware.springfield.controller;

import java.lang.reflect.Method;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeansException;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.servlet.mvc.condition.PatternsRequestCondition;
import org.springframework.web.servlet.mvc.method.RequestMappingInfo;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

public class GenericHandlerMapping extends RequestMappingHandlerMapping {
	
    protected Log logger = LogFactory.getLog(getClass());

    public GenericHandlerMapping(){
    	super.setOrder(Integer.MIN_VALUE);
    }
    
	@Override
	protected boolean isHandler(Class<?> beanType) {
		return ClassUtils.isAssignable(GenericController.class, beanType);
	}

	private GenericController<?> resolveHandler(Object handler) throws BeansException {
		GenericController<?> controller = null;
		if (handler instanceof String) {
			String beanName = (String) handler;
			controller = getApplicationContext().getBean(beanName, GenericController.class);
		}else {
			controller = (GenericController<?>)handler;
		}
		return controller;
	}
	
	@Override
	protected void registerHandlerMethod(Object handler, Method method, RequestMappingInfo mapping) {

		GenericController<?> controller = resolveHandler(handler);

    	//logger.debug("controller : "+controller);
    	PatternsRequestCondition patterns = mapping.getPatternsCondition();

    	Set<String> oldPath = patterns.getPatterns();
    	String[] newPath = new String[oldPath.size()];
    	int i = 0;
    	for(String path : oldPath){
    		
    		String r = path;
    		r = StringUtils.replace(r, GenericController.ROOT_PATTERN, controller.getRequestMappingRootPatternValue());
    		r = StringUtils.replace(r, GenericController.UNIQUE_PATTERN, controller.getRequestMappingUniquePatternValue());

    		newPath[i] = r;
    		
    		//logger.debug(newPath[i]);
    		i++;
    	}

    	PatternsRequestCondition newPatterns = null;
    	newPatterns = new PatternsRequestCondition(newPath, getUrlPathHelper(), getPathMatcher(), false, false);

    	
    	
    	
    	RequestMappingInfo newMapping = new RequestMappingInfo(
					newPatterns 
			   		,mapping.getMethodsCondition()
					,mapping.getParamsCondition()
					,mapping.getHeadersCondition()
					,mapping.getConsumesCondition()
					,mapping.getProducesCondition()
					,mapping.getCustomCondition());
		
    	try{
        	super.registerHandlerMethod(handler, method, newMapping);
        	//logger.debug("mapping success : "+newMapping);
    	}catch(Exception e){
        	logger.debug("oops : "+newMapping);
    	}
	}
}