package com.u2ware.springfield.support.resource;

import java.io.IOException;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ResourceLoaderAware;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.core.io.support.ResourcePatternResolver;
import org.springframework.core.io.support.ResourcePatternUtils;
import org.springframework.util.ClassUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

public class ResourcePatternResolverBean implements ResourceLoaderAware, InitializingBean{

	private static final Logger logger = LoggerFactory.getLogger(ResourcePatternResolverBean.class);


	private String targetName;
	private String[] packageToScan = new String[]{""};
	private String[] locationPatterns;
	private ResourcePatternResolver resourcePatternResolver;
	
	
	public void setPackageToScan(String[] packageToScan) {
		this.packageToScan = packageToScan;
	}
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
	private int addResources(HashSet<Resource> resourcesSet, String pattern){
		logger.warn(targetName+" Pattern: "+pattern);
		int count = 0;
		try{
			Resource[] resources = resourcePatternResolver.getResources(pattern);
			for(Resource r : resources){
				if(r.exists()){
					resourcesSet.add(r);
					logger.warn(targetName+" Adding: "+r);
					count++;
				}else{
					//logger.fatal(r);
				}
			}
		}catch(Exception e){
			//e.printStackTrace();
		}
		return count;
	}
	
	public void afterPropertiesSet() throws Exception {
		
		if (ObjectUtils.isEmpty(locationPatterns)) return;

		resourcesSet = new HashSet<Resource>();

		for(String packageToScanPath : packageToScan){
			
			String classBasedResourcePath = "classpath:"+ClassUtils.convertClassNameToResourcePath(packageToScanPath);
			String webappBasedResourcePath = "/WEB-INF/"+ClassUtils.convertClassNameToResourcePath(packageToScanPath);
			
			for(String locationPattern : locationPatterns){
				int r1 = addResources(resourcesSet, classBasedResourcePath+locationPattern);
				int r2 = addResources(resourcesSet, webappBasedResourcePath+locationPattern);
				if(r1 + r2 < 1){
					addResources(resourcesSet, locationPattern);
				}
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
	
	public String getLocation(Locale locale, String location){
		
		String language = locale.getLanguage();
		String country = locale.getCountry();
		
		StringBuilder result = new StringBuilder();
		result.append(StringUtils.stripFilenameExtension(location));
		if(StringUtils.hasText(language)){
			result.append("_");
			result.append(language);
		}
		if(StringUtils.hasText(country)){
			result.append("_");
			result.append(country);
		}
		result.append(".");
		result.append(StringUtils.getFilenameExtension(location));
		return result.toString();
	}
	public Resource getResource(Locale locale, Resource resource) throws IOException{
		
		String src = resource.getURL().toString();
		String location = getLocation(locale, src); 
		
		Resource dest = resourcePatternResolver.getResource(location);
		if(dest.exists()){
			return dest;
		}
		return resource;
	}
}