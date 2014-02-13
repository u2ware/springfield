package com.u2ware.springfield.security.authorization;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ResourceLoaderAware;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.util.ClassUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.context.ServletContextAware;

import com.thoughtworks.xstream.XStream;

public class NavigationFactory implements ServletContextAware, ResourceLoaderAware{

	private static final Logger logger = LoggerFactory.getLogger(NavigationFactory.class);

	private final String REQUEST_MAPPING_INFO_MAP_KEY = "com.u2ware.springfield.domain.EntityInformation";

	private Resource[] resources;
	private ServletContext servletContext;
	private ResourceLoader resourceLoader;
	
	private Navigation defaultNavigation;
	private Map<Locale,Navigation> navigationMap = new HashMap<Locale,Navigation>();

	public void setResources(Resource[] resources) {
		this.resources = resources;
	}
	
	public void setServletContext(ServletContext servletContext) {
		this.servletContext = servletContext;
	}
	public void setResourceLoader(ResourceLoader resourceLoader) {
		this.resourceLoader = resourceLoader;
	}

	/////////////////////////////////////////////////////
	//
	////////////////////////////////////////////////////
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public Navigation resolveNavigation(ServletRequest request) throws Exception{
		
		request.setAttribute(Navigation.OBJECT_NAME, null);

		Locale current = LocaleContextHolder.getLocale();

		Locale[] locales = parseLocale(current);
		for(Locale locale : locales){

			Navigation navigation = navigationMap.get(locale);
			
			if(navigation != null){
				logger.debug("Navigation Find : "+locale+" "+navigation);
				request.setAttribute(Navigation.OBJECT_NAME, navigation);			
				return navigation;

			}else{
				Resource resource = getResource(locale);
				if(resource != null){

					navigation = this.buildNavigation(resource);
					navigationMap.put(locale, navigation);
					
					logger.debug("Navigation Parse : "+locale+" "+resource);
					request.setAttribute(Navigation.OBJECT_NAME, navigation);			
					return navigation;
				}
			}
		}
		
		
		if(defaultNavigation == null){

			if(ObjectUtils.isEmpty(resources)){
				Object value = servletContext.getAttribute(REQUEST_MAPPING_INFO_MAP_KEY);
				
				if(ClassUtils.isAssignableValue(Map.class, value)){
					this.defaultNavigation = this.buildNavigation((Map)value);
					logger.debug("Default Navigation Create: "+value);
				}
				
			}else{
				this.defaultNavigation = this.buildNavigation(resources[0]);
				logger.debug("Default Navigation Parse: "+resources[0]);
			}
		}
		
		logger.debug("Navigation Default : "+current+" "+defaultNavigation);
		request.setAttribute(Navigation.OBJECT_NAME, defaultNavigation);			
		return defaultNavigation;
	}
	
	//////////////////////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////////////////////
	private Locale[] parseLocale(Locale locale) throws IOException{

		String language = locale.getLanguage();
		String country = locale.getCountry();
		String variant = locale.getVariant();

		if(StringUtils.hasText(country) && StringUtils.hasText(variant)){
			return new Locale[]{locale, new Locale(language, country), new Locale(language)};

		}else if(StringUtils.hasText(country) && ! StringUtils.hasText(variant)){
			return new Locale[]{locale, new Locale(language)};
		
		}else if(! StringUtils.hasText(country) && ! StringUtils.hasText(variant)){
			return new Locale[]{locale};
		}else{
			return new Locale[]{};
		}
	}

	private Resource getResource(Locale locale) throws IOException{
		
		if(resources == null) return null;
		
		String location = resources[0].getURL().toString();
		
		String language = locale.getLanguage();
		String country = locale.getCountry();
		String variant = locale.getVariant();

		StringBuilder r = new StringBuilder();
		r.append(StringUtils.stripFilenameExtension(location));
		if(StringUtils.hasText(language)){
			r.append("_");
			r.append(language);
		}
		if(StringUtils.hasText(country)){
			r.append("_");
			r.append(country);
		}
		if(StringUtils.hasText(country)){
			r.append("_");
			r.append(variant);
		}
		r.append(".");
		r.append(StringUtils.getFilenameExtension(location));
		
		Resource result = resourceLoader.getResource(r.toString());
		if(result.exists()){
			return result;
		}
		return null;
	}
	

	private Navigation buildNavigation(Resource resource) throws IOException{

		XStream xstream = new XStream();
		xstream.autodetectAnnotations(true);

		Navigation root = new Navigation();
		xstream.toXML(root);
		
		return (Navigation)xstream.fromXML(resource.getURL(), root);
	}
	private Navigation buildNavigation(Map<String,String> mappingInfo) {
		
		if(mappingInfo == null) {
			return null;
		}
		
		HandlerMappingNavigation root = new HandlerMappingNavigation();
		root.setPath("/");
		root.setPattern("/**");
		root.setName("root.title");

		for(String mapping : mappingInfo.keySet()){
			
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
						title = mappingInfo.get(mapping);
					}
					if(title.startsWith(".")){
						title = mappingInfo.get(mapping)+title;
					}
					
					
					child.setName(title);
	
					parent = (HandlerMappingNavigation) parent.addChild(child);
				}
			}
		}

		return root;
	}
	
	@SuppressWarnings("serial")
	private class HandlerMappingNavigation extends Navigation{

		public String getPath() {
			if("/".equals(super.getPath())){
				return super.getPath();
			}
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
	
}
