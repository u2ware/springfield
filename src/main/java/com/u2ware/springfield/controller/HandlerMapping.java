package com.u2ware.springfield.controller;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.util.ClassUtils;
import org.springframework.util.PatternMatchUtils;
import org.springframework.util.ReflectionUtils.MethodFilter;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.method.HandlerMethodSelector;
import org.springframework.web.servlet.mvc.condition.ConsumesRequestCondition;
import org.springframework.web.servlet.mvc.condition.HeadersRequestCondition;
import org.springframework.web.servlet.mvc.condition.ParamsRequestCondition;
import org.springframework.web.servlet.mvc.condition.PatternsRequestCondition;
import org.springframework.web.servlet.mvc.condition.ProducesRequestCondition;
import org.springframework.web.servlet.mvc.condition.RequestCondition;
import org.springframework.web.servlet.mvc.condition.RequestMethodsRequestCondition;
import org.springframework.web.servlet.mvc.method.RequestMappingInfo;
import org.springframework.web.servlet.mvc.method.RequestMappingInfoHandlerMapping;




public class HandlerMapping extends RequestMappingInfoHandlerMapping{
	
	private static final Logger logger = LoggerFactory.getLogger(HandlerMapping.class);

	private final String REQUEST_MAPPING_INFO_MAP_KEY = "com.u2ware.springfield.domain.EntityInformation";
	
	
	protected Map<String,String> m = new LinkedHashMap<String,String>(new HashMap<String,String>());

	public Map<String, String> getRequestMappingInfoMap() {
		return m;
	}

	protected void initHandlerMethods() {
		super.initHandlerMethods();
		getServletContext().setAttribute(REQUEST_MAPPING_INFO_MAP_KEY, m);			
	}
	
	
	
	
	@Override
	protected void detectHandlerMethods(final Object handler) {

		//super.set(interceptors)
		Class<?> handlerType = (handler instanceof String) ? 
				getApplicationContext().getType((String) handler) : handler.getClass();

		final Class<?> userType = ClassUtils.getUserClass(handlerType);
				
		Set<Method> methods = HandlerMethodSelector.selectMethods(userType, new MethodFilter() {
			public boolean matches(Method method) {
				return getMappingForMethod(handler, method, userType) != null;
			}
		});
		
		for (Method method : methods) {
			RequestMappingInfo mapping = getMappingForMethod(handler, method, userType);
			registerHandlerMethod(handler, method, mapping);
		}
	}

	@Override
	protected boolean isHandler(Class<?> beanType) {
		return  ClassUtils.isAssignable(EntityControllerImpl.class, beanType);
	}

	@Override
	protected RequestMappingInfo getMappingForMethod(Method method, Class<?> handlerType) {
		return null;
	}
	
	private RequestMappingInfo getMappingForMethod(Object handler, Method method, Class<?> handlerType) {
		
		Object handlerObj = (handler instanceof String) ? getApplicationContext().getBean((String) handler) : handler;
		EntityControllerImpl<?,?> controller = (EntityControllerImpl<?,?>)handlerObj;
		
		
		RequestMappingInfo info = null;
		RequestMapping methodAnnotation = createMethodLevelRequestMapping(controller, method);
		//logger.debug(methodAnnotation+" "+handler+" "+controller+" "+method.getName()+" "+controller.getMetamodel().getTopLevelMapping());
		
		if (methodAnnotation != null) {
			info = createRequestMappingInfo(methodAnnotation, null);
			
			//RequestMapping typeAnnotation = createTypeLevelRequestMapping(controller, handlerType);
			//info = createRequestMappingInfo(typeAnnotation, null).combine(info);
			
			//logger.info("****1 "+controller);
			//logger.info("****2 "+method);
			//logger.info("****3 "+typeAnnotation.value());
			//logger.info("****4 "+methodAnnotation.value());
		}
		return info;
	}

	protected RequestMappingInfo createRequestMappingInfo(RequestMapping annotation, RequestCondition<?> customCondition) {
		return new RequestMappingInfo(
				new PatternsRequestCondition(annotation.value(), getUrlPathHelper(), getPathMatcher(), false, true),
				new RequestMethodsRequestCondition(annotation.method()),
				new ParamsRequestCondition(annotation.params()),
				new HeadersRequestCondition(annotation.headers()),
				new ConsumesRequestCondition(annotation.consumes(), annotation.headers()),
				new ProducesRequestCondition(annotation.produces(), annotation.headers()), 
				customCondition);
	}
	
	
	///////////////////////////////////////////////////////////////////////
	//
	///////////////////////////////////////////////////////////////////////
	/*
	protected RequestMapping createTypeLevelRequestMapping(EntityHandler<?,?,?> handler, Class<?> handlerType){
		
		final RequestMapping requestMapping = AnnotationUtils.findAnnotation(handlerType, RequestMapping.class);
		if(requestMapping == null) return null;
		final String[] newValues = convertTopLevelRequestMappingValues(handler, requestMapping.value());
		
		return new RequestMapping(){
			public Class<? extends Annotation> annotationType() {return requestMapping.annotationType();}
			public String[] value() {return newValues;}
			public RequestMethod[] method() {return requestMapping.method();}
			public String[] params() {return requestMapping.params();}
			public String[] headers() {return requestMapping.headers();}
			public String[] consumes() {return requestMapping.consumes();}
			public String[] produces() {return requestMapping.produces();}
		};
	}
	protected String[] convertTopLevelRequestMappingValues(AbstractHandler<?,?> handler, String[] requestMappingValues){
		if(requestMappingValues == null || requestMappingValues.length < 1){
			return new String[]{handler.getRequestMapping()};
		}else{
			return requestMappingValues;
		}
	}
	*/

	///////////////////////////////////////////////////////////////////////
	//
	///////////////////////////////////////////////////////////////////////
	protected RequestMapping createMethodLevelRequestMapping(EntityControllerImpl<?,?> handler, Method method) {
		final RequestMapping requestMapping = AnnotationUtils.findAnnotation(method, RequestMapping.class);
		if(requestMapping == null) return null;

		
		final String[] newValues = createMethodLevelRequestMappingValues(handler, method, requestMapping.value());
		if(newValues == null || newValues.length == 0) return null;

		return new RequestMapping(){
			public Class<? extends Annotation> annotationType() {return requestMapping.annotationType();}
			public String[] value() {return newValues;}
			public RequestMethod[] method() {return requestMapping.method();}
			public String[] params() {return requestMapping.params();}
			public String[] headers() {return requestMapping.headers();}
			public String[] consumes() {return requestMapping.consumes();}
			public String[] produces() {return requestMapping.produces();}
		};
	}
	
	
	protected String[] createMethodLevelRequestMappingValues(EntityControllerImpl<?,?> handler, Method method, String[] requestMappingValues){
		
		try{
			String topLevelMapping = handler.getInformation().getTopLevelMapping();
			String[] methodLevelMappings = handler.getInformation().getMethodLevelMapping();
			String identityPath = handler.getInformation().getEntityPath();

			
			List<String> newValuesList = new ArrayList<String>();
			for(String requestMappingValue : requestMappingValues){

				String requestMapping = StringUtils.replace(requestMappingValue, EntityControllerImpl.COMMAND_ID_PATH, identityPath);

				if( ! StringUtils.hasLength(identityPath) ){
					throw new Exception("identity requried");
				}
				
				for(int i = 0 ; i < methodLevelMappings.length; i++){
					
					String file = StringUtils.stripFilenameExtension(methodLevelMappings[i]);
					String extension = StringUtils.getFilenameExtension(methodLevelMappings[i]);

					//logger.debug(file+"."+extension);
				
					
					if(PatternMatchUtils.simpleMatch(file, method.getName())){
						
						if(StringUtils.hasText(extension)){
							newValuesList.add(topLevelMapping + requestMapping+"."+extension);
							
						}else{
							newValuesList.add( topLevelMapping + requestMapping);
						}

						if(! m.containsKey(topLevelMapping+"/")){
							m.put(topLevelMapping+"/" , ClassUtils.getShortName(handler.getInformation().getQueryClass()));
						}

					}

				}
			}
			//logger.debug(newValuesList.size());
			
			String[] newValues = new String[newValuesList.size()];
			newValuesList.toArray(newValues);
			return newValues;

		}catch(Exception e){
			throw new RuntimeException(e);
		}
		
	}
	
	
	
}
