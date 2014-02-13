package com.u2ware.springfield.view.tiles2;

import java.io.IOException;

import org.apache.tiles.TilesException;
import org.springframework.context.ResourceLoaderAware;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;


public class TilesConfigurer extends org.springframework.web.servlet.view.tiles2.TilesConfigurer implements ResourceLoaderAware{
	
	private boolean noDefinitions = true;
	private ResourceLoader resourceLoader;
	
	@Override
	public void setDefinitions(String[] definitions) {
		super.setDefinitions(definitions);
		if(definitions != null){
			for(String definition : definitions){
				logger.debug(definition);
			}
			noDefinitions = false;
		}
	}
	@Override
	public void setResourceLoader(ResourceLoader resourceLoader) {
		this.resourceLoader = resourceLoader;
	}
	
	@Override
	public void afterPropertiesSet() throws TilesException {
		
		if(noDefinitions){
			
			try{
				Resource r = resourceLoader.getResource("classpath:com/u2ware/springfield/view/tiles2/tiles-definitions.xml");
				String[] definitions = new String[]{r.getURI().toString()};
				super.setDefinitions(definitions);
				for(String definition : definitions){
					logger.debug(definition);
				}
			}catch(IOException e){
				e.printStackTrace();
			}
		}
		super.afterPropertiesSet();
	}


	
}
