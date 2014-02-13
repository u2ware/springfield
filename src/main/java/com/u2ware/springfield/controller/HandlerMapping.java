package com.u2ware.springfield.controller;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

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

import com.u2ware.springfield.security.Navigation;




public class HandlerMapping extends RequestMappingInfoHandlerMapping{
	RequestMappingHandlerMapping f;

	protected final Log logger = LogFactory.getLog(getClass());

	
	protected Map<String,String> mappingTitle = new LinkedHashMap<String,String>(new HashMap<String,String>());

	protected Map<String,Integer> mappingSeq = new LinkedHashMap<String,Integer>(new HashMap<String,Integer>());
	
	
	protected void initHandlerMethods() {
		super.initHandlerMethods();
		this.initNavigation();
	}
	
	protected void initNavigation(){
		
		if(getServletContext().getAttribute(Navigation.OBJECT_NAME) != null) return;
		
		HandlerMappingNavigation root = new HandlerMappingNavigation();
		root.setPath("/");
		root.setPattern("/**");
		root.setName("root.title");

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
					child.setPattern(p+"/**");
					child.setPath(p);
					
					String title = sub[sub.length-1];
					if(! StringUtils.hasText(title)){
						title = mappingTitle.get(mapping);
					}
					if(title.startsWith(".")){
						title = mappingTitle.get(mapping)+title;
					}
					
					
					child.setName(title);
	
					parent = (HandlerMappingNavigation) parent.addChild(child);
				}
			}
		}

		getServletContext().setAttribute(Navigation.OBJECT_NAME, root);			
		logger.warn("Navigation build success.");
		
		//if(logger.isInfoEnabled())
		//	printNavigation(0, root);
	}

/*	private void printNavigation(int i, Navigation n){
		logger.info(i + " "+n);
		if(n.getChildren() != null){
			int j = i+1;
			for(Navigation c : n.getChildren()){
				printNavigation(j ,c);
			}
		}
	}
*/
	@SuppressWarnings("serial")
	private class HandlerMappingNavigation extends Navigation{

		@Override
		public String getPath() {
			if(super.getChildren().size() > 0){
				return getChildren().get(0).getPath();
			}
			return super.getPath();
		}
		
		@Override
		public Navigation addChild(Navigation child){
			if(super.getChildren().contains(child)){
				return getChildren().get(getChildren().indexOf(child));
			}else{
				super.getChildren().add(child);
				return child;
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
