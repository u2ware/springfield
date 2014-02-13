package com.u2ware.springfield.view.thymeleaf;

import java.io.IOException;
import java.io.InputStream;

import org.springframework.context.ResourceLoaderAware;
import org.springframework.core.io.ResourceLoader;
import org.thymeleaf.TemplateProcessingParameters;
import org.thymeleaf.resourceresolver.IResourceResolver;
import org.thymeleaf.templateresolver.TemplateResolver;

public class ResourceBasedTemplateResolver extends TemplateResolver implements IResourceResolver, ResourceLoaderAware{

	private ResourceLoader resourceLoader;
	
	public ResourceBasedTemplateResolver() {
        super();
        super.setResourceResolver(this);
    }

	public InputStream getResourceAsStream(TemplateProcessingParameters templateProcessingParameters,String resourceName) {
		try {
			return resourceLoader.getResource(resourceName).getInputStream();
		} catch (IOException e) {
			
			return null;
		}
	}

	public void setResourceLoader(ResourceLoader resourceLoader) {
        this.resourceLoader = resourceLoader;
	}

	

}
