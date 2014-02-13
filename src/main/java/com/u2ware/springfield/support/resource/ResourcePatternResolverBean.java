package com.u2ware.springfield.support.resource;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.context.ResourceLoaderAware;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.core.io.support.ResourcePatternResolver;
import org.springframework.core.io.support.ResourcePatternUtils;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;

public class ResourcePatternResolverBean implements ResourceLoaderAware, BeanNameAware{

	private Logger logger = LoggerFactory.getLogger(getClass());
	
	private String beanName;
	private String[] packagesToScan;
	private String[] resourcePatterns;
	private ResourcePatternResolver resolver;
	
	public void setPackagesToScan(String... packagesToScan) {
		this.packagesToScan = packagesToScan;
	}
	public void setResourcePatterns(String... resourcePatterns) {
		this.resourcePatterns = resourcePatterns;
	}
	public void setResourceLoader(ResourceLoader resourceLoader) {
		this.resolver = ResourcePatternUtils.getResourcePatternResolver(resourceLoader);
	}
	public void setBeanName(String beanName) {
		this.beanName = beanName;
	}
	public String[] getPackagesToScan() {
		return packagesToScan;
	}
	public String[] getResourcePatterns() {
		return resourcePatterns;
	}

	
	
	
	public String[] getFilenames() {
		try{
			Set<Resource> resources = findResources(beanName, resolver, packagesToScan, resourcePatterns);
			if(resources == null) return null;
			String[] result = new String[resources.size()];
			int i = 0;
			for(Resource r : resources){
				result[i] = StringUtils.stripFilenameExtension(r.getURL().toString());
				logger.warn(beanName+" Configuration: "+result[i]);
				i++;
			}
			return result;
		}catch(IOException e){
			e.printStackTrace();
			return null;
		}
	}
	
	public String[] getLocations() {
		try{
			Set<Resource> resources = findResources(beanName, resolver, packagesToScan, resourcePatterns);
			if(resources == null) return null;
			String[] result = new String[resources.size()];
			int i = 0;
			for(Resource r : resources){
				result[i] = r.getURI().toString();
				logger.warn(beanName+" Configuration: "+result[i]);
				i++;
			}
			return result;
		}catch(IOException e){
			e.printStackTrace();
			return null;
		}
	}
	
	public Resource[] getResources() {
		try{
			Set<Resource> resources = findResources(beanName, resolver, packagesToScan, resourcePatterns);
			if(resources == null) return null;
			Resource[] result = new Resource[resources.size()];
			int i = 0;
			for(Resource r : resources){
				result[i] = r;
				logger.warn(beanName+" Configuration: "+result[i]);
				i++;
			}
			return result;
		}catch(IOException e){
			e.printStackTrace();
			return null;
		}
	}
	
	
	
	
	private Set<Resource> findResources(String title, ResourcePatternResolver resolver, String[] packages, String[] parttens)throws IOException {
		
		if(resolver == null || packages == null || parttens == null ) return null;
		
		HashSet<Resource> resourcesSet = new HashSet<Resource>();
		for(String packageToScan : packages){
			
			String classBasedResourcePath = "classpath:"+ClassUtils.convertClassNameToResourcePath(packageToScan);
			String webappBasedResourcePath = "/WEB-INF/"+ClassUtils.convertClassNameToResourcePath(packageToScan);
			
			for(String partten : parttens){
				addResources(title, resolver, resourcesSet, classBasedResourcePath+partten);
				addResources(title, resolver, resourcesSet, webappBasedResourcePath+partten);
			}
		}
		if(resourcesSet.size() < 1) return null;
		
		return resourcesSet;
	}

	private void addResources(String title, ResourcePatternResolver resolver, HashSet<Resource> resourcesSet, String pattern) throws IOException {
		logger.warn(title+ " Pattern : "+pattern);
		try{
			Resource[] resources = resolver.getResources(pattern);
			for(Resource r : resources){
				if(r.exists()){
					if(! resourcesSet.contains(r)){
						logger.warn(title+ " Resource : "+r);
						resourcesSet.add(r);
					}
				}
			}
		}catch(Exception e){
			
		}
	}

}