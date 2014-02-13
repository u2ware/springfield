package com.u2ware.springfield.view.xstream;

import java.util.Locale;
import java.util.Map;

import org.springframework.context.ResourceLoaderAware;
import org.springframework.core.io.ResourceLoader;
import org.springframework.web.servlet.View;
import org.springframework.web.servlet.view.AbstractCachingViewResolver;

import com.u2ware.springfield.view.ViewResolverSupport;

public class XmlViewResolver extends AbstractCachingViewResolver implements ResourceLoaderAware{

	public XmlViewResolver(XmlView view){
		this.view = view;
	}
	
	///////////////////////////////////////
	//
	///////////////////////////////////////
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
	
	
	///////////////////////////////////////
	//
	///////////////////////////////////////
	private View view;
	
	@Override
	protected View loadView(String viewUrl, Locale locale) throws Exception {
		return view;
 	}

	@Override
	public View resolveViewName(String viewUrl, Locale locale) throws Exception {

		Map<String,String> attr = support.attributes(viewUrl);

		if(support.resolveViewNameByResolverName(attr)){
			View view = super.resolveViewName(viewUrl, locale);
        	support.addStaticAttributes(view, attr);
        	logger.info("Returning view :  "+view);
        	return view;
		}
        return null;
	}

}
