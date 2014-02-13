package com.u2ware.springfield.support.resource;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ResourceLoaderAware;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.core.io.support.ResourcePatternResolver;
import org.springframework.core.io.support.ResourcePatternUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

public class ResourcePatternResolverBean implements ResourceLoaderAware, InitializingBean{

	protected final Log logger = LogFactory.getLog(getClass());

	private String targetName;
	private String[] locationPatterns;
	private ResourcePatternResolver resourcePatternResolver;
	//private ResourcePatternResolverBeanSupport resourcePatternResolverBeanSupport;
	
	

	public void setLocationPatterns(String[] locationPatterns) {
		this.locationPatterns = locationPatterns;
	}
	public void setResourceLoader(ResourceLoader resourceLoader) {
		
		this.resourcePatternResolver = ResourcePatternUtils.getResourcePatternResolver(resourceLoader);
		//this.resourcePatternResolverBeanSupport = new ResourcePatternResolverBeanSupport(resourceLoader);
	}
	public void setTargetName(String targetName) {
		this.targetName = targetName;
	}

	///////////////////////////////////////////////
	//
	//////////////////////////////////////////////
	private HashSet<Resource> resourcesSet;
	private Resource[] resources;
	private String[] locations;
	private String[] basenames;

	public Set<Resource> getResourcesSet() {
		return resourcesSet;
	}
	public Resource[] getResources() {
		return resources;
	}
	public String[] getLocations() {
		return locations;
	}
	public String[] getBasenames() {
		return basenames;
	}
	///////////////////////////////////////////////
	//
	//////////////////////////////////////////////
	public void afterPropertiesSet() throws Exception {
		
		if (ObjectUtils.isEmpty(locationPatterns)) return;

	
		resourcesSet = new HashSet<Resource>();
		
		for(String locationPattern : locationPatterns){
			logger.warn(targetName+" Pattern: "+locationPattern);
			
			try{
				Resource[] resources = resourcePatternResolver.getResources(locationPattern);
				for(Resource r : resources){
					if(r.exists()){
						resourcesSet.add(r);
						logger.warn(targetName+" Adding: "+r);
					}else{
						//logger.fatal("Adding: ");
						
					}
				}
			}catch(Exception e){
				//e.printStackTrace();
			}
		}

		if(resourcesSet.size() < 1) return;

		logger.warn(targetName+": "+resourcesSet.size()+" file(s) configurated");
		
		resources = new Resource[resourcesSet.size()];
		locations = new String[resourcesSet.size()];
		basenames = new String[resourcesSet.size()];
		int i = 0;
		for(Resource r : resourcesSet){
			resources[i] = r;
			locations[i] = r.getURL().toString();
			basenames[i] = StringUtils.stripFilenameExtension(locations[i]);
			i++;
		}
	}
}