package com.u2ware.springfield.view;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.StringTokenizer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.core.Ordered;
import org.springframework.core.io.Resource;
import org.springframework.util.ClassUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.context.support.WebApplicationObjectSupport;
import org.springframework.web.servlet.View;
import org.springframework.web.servlet.ViewResolver;
import org.springframework.web.servlet.view.AbstractView;

public class EntityViewResolver extends WebApplicationObjectSupport implements Ordered, ViewResolver{

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	private Properties properties;
	private String[] locations;
	public List<ViewResolverSupport> entityViewResolvers = new ArrayList<ViewResolverSupport>();
	
	public void setProperties(Properties properties) {
		this.properties = properties;
	}
	public void setLocations(String[] locations) {
		this.locations = locations;
	}
	public void setEntityViewResolvers(List<ViewResolverSupport> entityViewResolvers) {
		this.entityViewResolvers = entityViewResolvers;
	}
	@Override
	public int getOrder() {
		return Ordered.HIGHEST_PRECEDENCE;
	}

	//////////////////////////////////////
	//
	///////////////////////////////////////
	public void addViewResolver(ViewResolverSupport viewResolver){
		entityViewResolvers.add(viewResolver);
	}
	public void removeViewResolver(ViewResolverSupport viewResolver){
		entityViewResolvers.remove(viewResolver);
	}
	public ViewResolverSupport findViewResolver(String viewKey){
		for(ViewResolverSupport r : entityViewResolvers){

			if(r.getBeanName().equals(viewKey)){
				return r;
			}
			if(ObjectUtils.containsElement(r.getExtensions(), viewKey)){
				return r;
			}
		}
		throw new NullPointerException("not found viewResolver by "+viewKey);
	}

	
	/////////////////////////////////////////////////
	//
	////////////////////////////////////////////////
	@Override
	public View resolveViewName(String orgViewName, Locale locale) throws Exception {
		
		
		String url = orgViewName;
		int sId = url.indexOf(";");
		if(sId > 0){
			url = url.substring(0, sId);
		}
		
		int qIdx = url.indexOf("?");
		String query = (qIdx > 0) ? url.substring( qIdx + 1) : null;
		String uri = (qIdx > 0) ? url.substring(0, qIdx) : url;

		Map<String,String> attributes = attributesCSV(properties, query);
		
		String filename = StringUtils.getFilename(uri);
		String extension = StringUtils.getFilenameExtension(filename);
		String path =  uri.replaceAll(filename, "");
		String method = StringUtils.stripFilenameExtension(filename);

    	logger.warn("Request ViewName: "+orgViewName);
		logger.warn("Request Query: "+query);
		logger.warn("Request Uri: "+uri);
		logger.warn("Request UriPath: "+path);
		logger.warn("Request UriName: "+method);
		logger.warn("Request UriExtension: "+extension);

		attributes.put("springfield.request.viewName", orgViewName);
		attributes.put("springfield.request.path", path);
		attributes.put("springfield.request.method", method);
		attributes.put("springfield.request.extension", extension);
		
		////////////////////////////////////////////
		//
		////////////////////////////////////////////
		String viewMethod = attributes.get("springfield.view.method."+method);
		if(viewMethod == null) viewMethod = method;
		String viewExtension = (extension != null ? extension : "none");
		String viewKey = attributes.get("springfield.view.extension."+viewExtension);
		if(viewKey == null) viewKey = viewExtension;

		ViewResolverSupport viewResolver = findViewResolver(viewKey);
		
		attributes.put("springfield.response.method", viewMethod);
		attributes.put("springfield.response.viewKey", viewKey);
		attributes.put("springfield.response.extension", viewResolver.getBaseExtensions());

		logger.warn("View Method: "+viewMethod);
		logger.warn("View Key: "+viewKey);

		///////////////////////////////////////////////////////////////
		//
		///////////////////////////////////////////////////////////////
		String viewName = null;
		if(viewResolver.isResourceRequired()){
			for(String location : locations){
				viewName = resolverViewName(location, attributes);
				if(viewName != null) break;
			}
		}
		if(viewName == null){
			viewName = uri;
		}
		
		
		attributes.put("springfield.response.viewName", viewName);
		
    	logger.warn("View Resolver Name: "+viewKey);
    	logger.warn("View Resolver : "+viewResolver);
    	logger.warn("View Name: "+viewName);
    	
		///////////////////////////////////////////////////////////////
		//
		///////////////////////////////////////////////////////////////
		View view = viewResolver.resolveViewName(viewName, locale);
    	logger.warn("View: "+view);
		if(view != null && ClassUtils.isAssignable(AbstractView.class, view.getClass())){
			AbstractView abstractView = (AbstractView)view;
        	abstractView.setAttributesMap(attributes);
		}
		return view;
	}
	
	private String resolverViewName(String location, Map<String,String> attr){

		logger.warn("Search Location: "+location);
		
		String reqPath = attr.get("springfield.request.path"); 
		String reqMethod = attr.get("springfield.request.method"); 
		String reqExtension = attr.get("springfield.request.extension"); 

		String viewMethod = attr.get("springfield.response.method"); 
		String viewExtension = attr.get("springfield.response.extension");
		
		String viewName = null;
        String extension1 = (reqExtension != null ? "."+reqExtension : "")+"."+viewExtension;
        String extension2 = "."+viewExtension;

        viewName = findResource(location, reqPath + reqMethod + extension1);
        if(viewName != null) return viewName;    
        
        viewName = findResource(location, "/" + reqMethod + extension1);
        if(viewName != null) return viewName;    
		
        viewName = findResource(location, reqPath + viewMethod + extension1);
        if(viewName != null) return viewName;    
		
        
        viewName = findResource(location, "/" + viewMethod + extension1);
        if(viewName != null) return viewName;   
		

        viewName = findResource(location, reqPath + reqMethod + extension2);
        if(viewName != null) return viewName;    
        
        viewName = findResource(location, "/" + reqMethod + extension2);
        if(viewName != null) return viewName;    
		
        viewName = findResource(location, reqPath + viewMethod + extension2);
        if(viewName != null) return viewName;    
		
        
        viewName = findResource(location, "/" + viewMethod + extension2);
        if(viewName != null) return viewName;   
        
        return null;
	}
	
	
	
	private String findResource(String location, String path){
        String filename = location + path ; 
    	Resource resource = getApplicationContext().getResource(filename);

    	if(resource.exists()){
        	logger.warn("Find Resource: "+resource);
    		return filename;
    	}
    	logger.warn("Search Resource: "+resource);
    	return null;
	}
	
	private Map<String,String> attributesCSV(Properties p, String propString) {

		
		logger.debug(""+p);
		
		
		Properties e = p == null ? new Properties() : p;
		Map<String,String> m = new HashMap<String,String>();
		
		m.put("springfield.response.locations", e.getProperty("springfield.response.locations", StringUtils.arrayToCommaDelimitedString(locations)));
		m.put("springfield.view.method.home", e.getProperty("springfield.view.method.home", "home"));
		m.put("springfield.view.method.find", e.getProperty("springfield.view.method.find", "list"));
		m.put("springfield.view.method.createForm", e.getProperty("springfield.view.method.createForm", "edit"));
		m.put("springfield.view.method.create", e.getProperty("springfield.view.method.create", "refresh"));
		m.put("springfield.view.method.read", e.getProperty("springfield.view.method.read", "edit"));
		m.put("springfield.view.method.updateForm", e.getProperty("springfield.view.method.updateForm", "edit"));
		m.put("springfield.view.method.update", e.getProperty("springfield.view.method.update", "refresh"));
		m.put("springfield.view.method.delete", e.getProperty("springfield.view.method.delete", "refresh"));
		
		if (propString != null) {
			StringTokenizer st = new StringTokenizer(propString, ",");
			while (st.hasMoreTokens()) {
				String tok = st.nextToken();
				int eqIdx = tok.indexOf("=");
				if (eqIdx == -1) {
					throw new IllegalArgumentException("Expected = in attributes CSV string '" + propString + "'");
				}
				if (eqIdx >= tok.length() - 2) {
					throw new IllegalArgumentException("At least 2 characters ([]) required in attributes CSV string '" + propString + "'");
				}
				String name = tok.substring(0, eqIdx);
				String value = tok.substring(eqIdx + 1);

				// Delete first and last characters of value: { and }
				value = value.substring(1);
				value = value.substring(0, value.length() - 1);

				m.put(name, value);
			}
		}

		return m;
	}
	
	public static class ViewResolverSupport implements BeanNameAware{

		private View view;
		private ViewResolver viewResolver;
		private String beanName;
		private String baseExtensions;	
		private String[] extensions;
		private boolean resourceRequired =false;

		public ViewResolverSupport(View view){
			this.view = view;
		}
		public ViewResolverSupport(ViewResolver viewResolver){
			this.viewResolver = viewResolver;
		}

		public boolean isResourceRequired() {
			return resourceRequired;
		}
		public void setResourceRequired(boolean resourceRequired) {
			this.resourceRequired = resourceRequired;
		}
		public String getBeanName() {
			return beanName;
		}
		public void setBeanName(String beanName) {
			this.beanName = beanName;
		}
		public String[] getExtensions() {
			return extensions;
		}
		public void setExtensions(String... extensions) {
			this.extensions = extensions;
		}
		public String getBaseExtensions() {
			return baseExtensions;
		}
		public void setBaseExtensions(String baseExtensions) {
			this.baseExtensions = baseExtensions;
		}

		public View resolveViewName(String viewName, Locale locale) throws Exception {
			return view != null ? view : viewResolver.resolveViewName(viewName, locale);
		}

		
		public String toString(){
			return view != null ? view.toString() : viewResolver.toString();
		}
	}	
}
