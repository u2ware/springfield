package com.u2ware.springfield.controller;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.collections.map.LinkedMap;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
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
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;


public class HandlerMapping extends RequestMappingInfoHandlerMapping{
	RequestMappingHandlerMapping f;

	protected final Log logger = LogFactory.getLog(getClass());

	
	@SuppressWarnings("unchecked")
	protected Map<String,String> mappingTitle = new LinkedMap(new HashMap<String,String>());

	@SuppressWarnings("unchecked")
	protected Map<String,Integer> mappingSeq = new LinkedMap(new HashMap<String,Integer>());
	
	
	protected void initHandlerMethods() {
		super.initHandlerMethods();
		this.initNavigation();
	}
	
	protected void initNavigation(){
		
		
		
		HandlerMappingNavigation root = new HandlerMappingNavigation();
		root.setLink("/");
		root.setPath("/");
		root.setTitle("root.title");

		for(String mapping : mappingTitle.keySet()){
			
			//logger.debug(mapping+"......");
			
			String[] paths = StringUtils.delimitedListToStringArray(mapping, "/");
			
			HandlerMappingNavigation parent = root;
			HandlerMappingNavigation child = null;
			for(int i = 0 ; i < paths.length; i++){

				
				String[] sub = Arrays.copyOfRange(paths, 0, i+1);
				String p = StringUtils.arrayToDelimitedString(sub , "/");
						
				if(StringUtils.hasText(p)){
					child = new HandlerMappingNavigation();
					child.setPath(p);
					child.setLink(p);
					
					String title = sub[sub.length-1];
					if(! StringUtils.hasText(title)){
						title = mappingTitle.get(mapping);
					}
					if(title.startsWith(".")){
						title = mappingTitle.get(mapping)+title;
					}
					
					
					child.setTitle(title);
	
					parent = (HandlerMappingNavigation) parent.addChild(child);
				}
			}
		}
		
		printNavigation(root, 0);

		//logger.debug("springfield_navigation : "+ getClass().getName());
		getServletContext().setAttribute(root.getClass().getName(), root);			
	}

	private void printNavigation(HandlerMappingNavigation n, int i){
		//logger.debug(i + " "+n.getPath()+" "+n.getLink()+" "+n.getTitle());
		if(n.getChilds() != null){
			int j = i+1;
			for(HandlerMappingNavigation c : n.getChilds()){
				printNavigation(c, j);
			}
		}
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
		return  ClassUtils.isAssignable(EntityHandler.class, beanType);
	}

	@Override
	protected RequestMappingInfo getMappingForMethod(Method method, Class<?> handlerType) {
		return null;
	}
	
	private RequestMappingInfo getMappingForMethod(Object handler, Method method, Class<?> handlerType) {
		
		Object handlerObj = (handler instanceof String) ? getApplicationContext().getBean((String) handler) : handler;
		EntityHandler<?,?> controller = (EntityHandler<?,?>)handlerObj;
		
		//logger.debug(handler+" "+controller+" "+method.getName()+" "+controller.getQueryMetamodel().getTargetMapping());
		
		RequestMappingInfo info = null;
		RequestMapping methodAnnotation = createMethodLevelRequestMapping(controller, method);
		
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
	protected RequestMapping createMethodLevelRequestMapping(EntityHandler<?,?> handler, Method method) {
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
	
	
	protected String[] createMethodLevelRequestMappingValues(EntityHandler<?,?> handler, Method method, String[] requestMappingValues){
		
		try{
			String topLevelMapping = handler.getMetamodel().getTopLevelMapping();
			String[] methodLevelMappings = handler.getMetamodel().getMethodLevelMapping();
			String commandId = handler.getMetamodel().getIdentityUri();

			List<String> newValuesList = new ArrayList<String>();
			for(String requestMappingValue : requestMappingValues){

				String requestMapping = StringUtils.replace(requestMappingValue, EntityHandler.COMMAND_ID, commandId);

				for(int i = 0 ; i < methodLevelMappings.length; i++){
					
					String file = StringUtils.stripFilenameExtension(methodLevelMappings[i]);
					String extension = StringUtils.getFilenameExtension(methodLevelMappings[i]);

					
					
					if(PatternMatchUtils.simpleMatch(file, method.getName())){
						
						if(StringUtils.hasText(extension)){
							newValuesList.add(topLevelMapping + requestMapping+"."+extension);
							
						}else{
							newValuesList.add( topLevelMapping + requestMapping);
						}

						if(! mappingTitle.containsKey(topLevelMapping+"/")){
							mappingTitle.put(topLevelMapping+"/" , ClassUtils.getShortName(handler.getMetamodel().getQueryClass()));
						}

					}

				}
			}
			
			String[] newValues = new String[newValuesList.size()];
			newValuesList.toArray(newValues);
			return newValues;

		}catch(Exception e){
			e.printStackTrace();
			return new String[]{};
		}
		
	}
	
	
	
}
