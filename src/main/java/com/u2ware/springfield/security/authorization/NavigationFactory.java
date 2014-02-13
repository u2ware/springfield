package com.u2ware.springfield.security.authorization;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.servlet.ServletRequest;

import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.core.io.Resource;
import org.springframework.util.StringUtils;
import org.springframework.web.context.support.WebApplicationObjectSupport;

import com.thoughtworks.xstream.XStream;

public class NavigationFactory extends WebApplicationObjectSupport  {

	private Resource configLocation;
	private Map<Locale,Navigation> navigationMap = new HashMap<Locale,Navigation>();

	public void setConfigLocation(Resource configLocation) {
		this.configLocation = configLocation;
	}

	
	
	
	/////////////////////////////////////////////////////
	//
	////////////////////////////////////////////////////
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public Navigation resolveNavigation(ServletRequest request) throws Exception{
		
		Navigation navigation = null;
		
		Locale current = LocaleContextHolder.getLocale();
		Locale[] locales = parseLocale(current);
		for(Locale locale : locales){

			if(navigationMap.containsKey(locale)){
				navigation = navigationMap.get(locale);
				logger.warn("Navigation Find : "+locale);
			}else{
				Resource resource = getResource(configLocation, locale);
				if(resource != null && resource.exists()){

					navigation = this.buildNavigation(resource);
					logger.warn("Navigation Parse : "+locale+" "+resource);
					navigationMap.put(locale, navigation);
				}
			}
		}

		if(navigation == null){
			if(configLocation != null){
				navigation = this.buildNavigation(configLocation);
				logger.warn("Navigation Parse: "+current+" "+configLocation);

			}else{
				Object value = getServletContext().getAttribute(Navigation.OBJECT_NAME);
				navigation = this.buildNavigation((Map)value);
				logger.warn("Navigation Create: "+current+" "+value);
			}
			navigationMap.put(current, navigation);
		}
		
		try{
			request.setAttribute(Navigation.OBJECT_NAME, navigation);
		}catch(Exception e){
			getServletContext().setAttribute(Navigation.OBJECT_NAME, navigation);
		}
		
		return navigation;
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

	private Resource getResource(Resource resource, Locale locale) throws IOException{
		
		if(resource == null) return null;
		
		String location = resource.getURL().toString();
		
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
		
		Resource result = getWebApplicationContext().getResource(r.toString());
		if(result.exists()){
			return result;
		}
		return null;
	}
	

	private Navigation buildNavigation(Resource resource) throws IOException{

		if(resource == null) return null;

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
		root.setName("Springfield");

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
