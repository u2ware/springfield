package com.u2ware.springfield.view.thymeleaf;

import java.util.Locale;
import java.util.Map;

import org.springframework.context.ResourceLoaderAware;
import org.springframework.core.io.ResourceLoader;
import org.springframework.web.servlet.View;

import com.u2ware.springfield.view.ViewResolverSupport;

public class ThymeleafViewResolver extends org.thymeleaf.spring3.view.ThymeleafViewResolver implements ResourceLoaderAware{

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
	public void setSampleLocations(String[] sampleLocations) {
		support.setSampleLocations(sampleLocations);
	}

	@Override
	public View resolveViewName(String viewUrl, Locale locale) throws Exception {

		Map<String,String> attr = support.attributes(viewUrl);

		String viewName = null;
		if(support.resolveViewNameByResolverName(attr)){
			viewName = support.resolveViewNameByBaseExtension(attr);
		}
		
		if(viewName == null){
			viewName = support.resolveViewNameBySampleLocations(attr, "html");
		}
		
		if(viewName != null){
			View view = super.resolveViewName(viewName, locale);
	    	logger.info("Returning view :  "+view);
	    	support.addStaticAttributes(view, attr);
	    	return view;
		}
		return null;
	}
}
