package com.u2ware.springfield.view.tiles;

import java.util.Locale;
import java.util.Map;

import org.springframework.context.ResourceLoaderAware;
import org.springframework.core.io.ResourceLoader;
import org.springframework.util.StringUtils;
import org.springframework.web.servlet.View;
import org.springframework.web.servlet.view.UrlBasedViewResolver;

import com.u2ware.springfield.view.ViewResolverSupport;

public class TilesViewResolver extends UrlBasedViewResolver implements ResourceLoaderAware{

	private final ViewResolverSupport support = new ViewResolverSupport(); 

	public void setStaticName(String staticName) {
		support.setStaticName(staticName);
	}
	public void setStaticAttributes(Map<String, String> staticAttributes) {
		support.setStaticAttributes(staticAttributes);
	}
	public void setResourceLoader(ResourceLoader resourceLoader) {
		support.setResourceLoader(resourceLoader);
	}
	public void setBaseLocations(String[] baseLocations) {
		support.setBaseLocations(baseLocations);
	}
	public void setBaseExtension(String baseExtension) {
		support.setBaseExtension(baseExtension);
	}

	@Override
	public View resolveViewName(String viewUrl, Locale locale) throws Exception {

		Map<String,String> attr = support.attributes(viewUrl);

		if(support.resolveViewNameByResolverName(attr)){
			String viewName = support.resolveViewNameByBaseExtension(attr);
			if(viewName != null){
	        	logger.info("Returning view :  "+viewName);
				
				viewName = StringUtils.stripFilenameExtension(viewName);
				for(String baseLocation : support.getBaseLocations()){
					viewName = viewName.replaceAll(baseLocation, "");
				}
	        	logger.info("Returning view :  "+viewName);
				
				View view = super.resolveViewName(viewName, locale);
	        	logger.info("Returning view :  "+view);
	        	support.addStaticAttributes(view, attr);
	        	return view;
			}
	        return null;
		}
        return null;
	}
	
	/*
	@Override
	public View resolveViewName(String viewUrl, Locale locale) throws Exception {
		String viewName = support.resolveViewName(viewUrl, locale);
		if(viewName != null){
			viewName = StringUtils.stripFilenameExtension(viewName);
			for(String baseLocation : baseLocations){
				viewName = viewName.replaceAll(baseLocation, "");
			}
			
			View view = super.resolveViewName(viewName, locale);
        	logger.info("Returning view :  "+view);
        	support.addStaticAttributes(view, viewName);
        	return view;
		}
        return null;
	}
	*/
}
