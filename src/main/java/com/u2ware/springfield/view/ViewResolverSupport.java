package com.u2ware.springfield.view;

import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.util.ClassUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.servlet.View;
import org.springframework.web.servlet.view.AbstractView;

import com.u2ware.springfield.controller.EntityController;

public class ViewResolverSupport {

	protected final Log logger = LogFactory.getLog(getClass());
	
	private ResourceLoader resourceLoader;

	private String staticName;
	private Map<String,String> staticAttributes;
	
	private String[] baseLocations;
	private String baseExtension;
	
	private String[] springfieldLocations = new String[]{"classpath:com/u2ware/springfield/view/resources"};
   	private String springfieldExtension = "springfield1";
	
	public void setResourceLoader(ResourceLoader resourceLoader) {
		this.resourceLoader = resourceLoader;
	}
	public void setStaticName(String staticName) {
		this.staticName = staticName;
	}
	public void setStaticAttributes(Map<String, String> staticAttributes) {
		this.staticAttributes = staticAttributes;
	}
	public void setBaseLocations(String[] baseLocations) {
		this.baseLocations = baseLocations;
	}
	public void setBaseExtension(String baseExtension) {
		this.baseExtension = baseExtension;
	}
	public String getStaticName() {
		return staticName;
	}
	public Map<String, String> getStaticAttributes() {
		return staticAttributes;
	}
	public String[] getBaseLocations() {
		return baseLocations;
	}
	public String getBaseExtension() {
		return baseExtension;
	}
	//////////////////////////////////////////////
	//
	//////////////////////////////////////////////
	private Map<String, String> attributesCSV(Map<String,String> target, String propString) {
		Map<String, String> staticAttributes = new HashMap<String, String>(target);
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

				staticAttributes.put(name, value);
			}
		}
		
		
		
		return staticAttributes;
	}

	public Map<String, String> attributes(String viewName){
		
		int qIdx = viewName.indexOf("?");
		String query = (qIdx > 0) ? viewName.substring( qIdx + 1) : null;
		String uri = (qIdx > 0) ? viewName.substring(0, qIdx) : viewName;

		Map<String, String> attributes = attributesCSV(staticAttributes, query);
		
		String filename = StringUtils.getFilename(uri);
		String extension = StringUtils.getFilenameExtension(filename);
		String path =  uri.replaceAll(filename, "");
		String method = StringUtils.stripFilenameExtension(filename);

		attributes.put("path", path);
		attributes.put("method", method);
		attributes.put("extension", extension);
		
		
		logger.trace("viewQuery: "+query);
		logger.trace("viewUri: "+uri);
		logger.trace("viewUriPath: "+path);
		logger.trace("viewUriName: "+method);
		logger.trace("viewUriExtension: "+extension);

		////////////////////////////////////////////
		//
		////////////////////////////////////////////
		String methodKey = "webmvc.view.method."+method;
		String methodValue = attributes.get(methodKey);
		String name = (methodValue != null) ? methodValue : method;

		logger.trace("methodKey: "+methodKey);
		logger.trace("methodValue: "+methodValue);
		logger.trace("name: "+name);
		
		attributes.put("name", name);
		
		////////////////////////////////////////////
		//
		////////////////////////////////////////////
		String resolverKey = "webmvc.view.extension."+(extension != null ? extension : "none");
		String resolver = attributes.get(resolverKey);

		logger.trace("resolverKey: "+resolverKey);
		logger.trace("resolver: "+resolver);

		attributes.put("webmvc.view.resolver", resolver);

		String initialExt = (extension != null ? "."+extension : "");
		String initialPath = path + name + initialExt;
		logger.info("["+staticName+"] Negotiating view  :  "+initialPath);
		
		return attributes;
	}
	
	public boolean resolveViewNameByResolverName(Map<String, String> attr){
		if(staticName.equals(attr.get("webmvc.view.resolver"))){
 			return true;
		}
		return false;
	}
	

	//////////////////////////////////////////////
	//
	//////////////////////////////////////////////
	public String resolveViewNameByBaseExtension(Map<String, String> attr){
	
		if(ObjectUtils.isEmpty(baseLocations) || ! StringUtils.hasText(baseExtension) )
			return null;
		
        String extension = (attr.get("extension") != null ? "."+attr.get("extension") : "")+"."+baseExtension;
        String resource = null;
        
		////////////////////////////////////////////
        String path1 = attr.get("path") + attr.get("name") + extension;
        resource = findResource(attr, staticName, baseLocations, path1);
        if(resource != null) return resource;
        
		////////////////////////////////////////////
        String path2 = "/" + attr.get("name") + extension;
        resource = findResource(attr, staticName, baseLocations,path2);
        if(resource != null) return resource;

        return null;
	}

	
	//////////////////////////////////////////////
	//
	//////////////////////////////////////////////
	public String resolveViewNameBySpringfield(Map<String, String> attr, String extension){

		String springfield = attr.get("webmvc.view.springfield");
		springfield =  (springfield != null ? springfield :springfieldExtension);
		
        String resource = null;
    	
        String path = "/"+springfield+"/" + attr.get("method") +"."+extension;
        resource = findResource(attr, "Springfield", springfieldLocations, path);
        if(resource != null) return resource;

        return null;
	}

	//////////////////////////////////////////////
	//
	//////////////////////////////////////////////
	private String findResource(Map<String, String> attr, String resolver, String[] locations, String path){
        for(String location : locations){
            String filename = location + path ; 
        	Resource resource = resourceLoader.getResource(filename);
        	logger.info("["+resolver+"] Search view: "+resource);

        	if(resource.exists()){
            	//logger.info("["+resolver+"] Returning view:  "+filename);
        		if(! attr.containsKey("webmvc.view.uri")){
                	attr.put("webmvc.view.uri", filename);
        		}
        		
        		if(! attr.containsKey("webmvc.view.name")){
                	attr.put("webmvc.view.name", StringUtils.getFilename(path));
        		}

        		return filename;
        	}
        }
        return null;
	}
	
	
	//////////////////////////////////////////////
	//
	//////////////////////////////////////////////
	public void addStaticAttributes(View view, Map<String, String> attr) {
		logger.info("staticAttributes  :  "+attr);
		if(view != null && ClassUtils.isAssignable(AbstractView.class, view.getClass())){
			
			AbstractView abstractView = (AbstractView)view;
			//abstractView.setAttributesCSV(getSpringfieldViewAttributesCVS(springfieldViewName));
			//String propString = resolveSpringfieldViewAttrsCVS(springfieldViewUrl);
			//abstractView.setAttributesCSV(propString);
        	//logger.info(propString);
        	abstractView.setAttributesMap(attr);
		}
	}
	
	
	private static String getRequestModelKey(Map<String, Object> model) {
		String modelKey = null;
		if(model.get(EntityController.MODEL_ENTITY) != null){
			modelKey = EntityController.MODEL_ENTITY;
		}else if(model.get(EntityController.MODEL_QUERY) != null){
			modelKey = EntityController.MODEL_QUERY;
		}else{
			return "";
		}
		return modelKey;
	}
	public static Object getRequestModel(Map<String, Object> model) {
		String modelKey = getRequestModelKey(model);
		return model.get(modelKey);
	}
	
	private static String getResponseModelKey(Map<String, Object> model) {
		String modelKey = null;
		if(model.get(EntityController.MODEL_ENTITY) != null){
			modelKey = EntityController.MODEL_ENTITY;
		}else if(model.get(EntityController.MODEL_QUERY_RESULT) != null){
			modelKey = EntityController.MODEL_QUERY_RESULT;
		}else{
			return "";
		}
		return modelKey;
	}
	public static Object getResponseModel(Map<String, Object> model) {
		String modelKey = getResponseModelKey(model);
		return model.get(modelKey);
	}
}
