package com.u2ware.springfield.domain;

import javax.servlet.ServletRequest;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.PropertyValues;
import org.springframework.core.MethodParameter;
import org.springframework.validation.DataBinder;
import org.springframework.web.bind.ServletRequestDataBinder;
import org.springframework.web.bind.ServletRequestParameterPropertyValues;
import org.springframework.web.bind.support.WebArgumentResolver;
import org.springframework.web.context.request.NativeWebRequest;

import com.fasterxml.jackson.databind.ObjectMapper;


public class EntityPageableArgumentResolver implements WebArgumentResolver{

	protected final Log logger = LogFactory.getLog(getClass());

	private static final String DEFAULT_PREFIX = "model_query_pageable";
	private static final String DEFAULT_SEPARATOR = ".";

	private String prefix = DEFAULT_PREFIX;
	private String separator = DEFAULT_SEPARATOR;
	private ObjectMapper objectMapper = new ObjectMapper();

	public void setPrefix(String prefix) {
		this.prefix = null == prefix ? DEFAULT_PREFIX : prefix;
	}
	public void setSeparator(String separator) {
		this.separator = null == separator ? DEFAULT_SEPARATOR : separator;
	}
	

	public Object resolveArgument(MethodParameter methodParameter, NativeWebRequest webRequest) {

		//logger.debug("11");

		if (methodParameter.getParameterType().equals(EntityPageable.class)) {

			//logger.debug("22");
			//assertPageableUniqueness(methodParameter);
			ServletRequest servletRequest = (ServletRequest) webRequest.getNativeRequest();
			EntityPageRequest request = createEntityPageRequest(servletRequest);
			if(request == null){
				request = createEntityPageRequest(servletRequest, methodParameter);
			}
			return request;
		}

		return UNRESOLVED;
	}

	private EntityPageRequest getDefaultFromAnnotationOrFallback(MethodParameter methodParameter) {
		return new EntityPageRequest();
	}
	private String getPrefix(MethodParameter parameter) {
		return prefix;
	}

	private EntityPageRequest createEntityPageRequest(ServletRequest servletRequest) {

		String[] values = servletRequest.getParameterValues(DEFAULT_PREFIX);
		//logger.debug(values);
		if(values == null || values.length != 1) return null;
		logger.debug(values[0]);
		
		try {
			EntityPageRequest request = objectMapper.readValue(values[0], EntityPageRequest.class);
			//logger.debug(request);
			return request;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
	
	private EntityPageRequest createEntityPageRequest(ServletRequest servletRequest, MethodParameter methodParameter) {
		EntityPageRequest request = getDefaultFromAnnotationOrFallback(methodParameter);
		PropertyValues propertyValues = new ServletRequestParameterPropertyValues(servletRequest,
				getPrefix(methodParameter), separator);

		logger.debug(propertyValues);

		DataBinder binder = new ServletRequestDataBinder(request);
		binder.bind(propertyValues);

		//logger.debug(request);
		if(request.getPageNumber() < 0) request.setPageNumber(0);
		if(request.getPageSize() < 1) request.setPageSize(10);
		//logger.debug(request);
		
		return request;
	}
	
}
