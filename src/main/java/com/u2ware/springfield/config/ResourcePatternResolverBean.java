package com.u2ware.springfield.config;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ResourceLoaderAware;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.core.io.support.ResourcePatternResolver;
import org.springframework.core.io.support.ResourcePatternUtils;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;

class ResourcePatternResolverBean implements ResourceLoaderAware{

	private Logger logger = LoggerFactory.getLogger(getClass());
	
	private Iterable<String> basePackages;
	private String[] resourcePatterns;
	private ResourcePatternResolver resolver;
	
	
	public ResourcePatternResolverBean(Iterable<String> basePackages, String... resourcePatterns){
		
		this.basePackages = basePackages;
		this.resourcePatterns = resourcePatterns;
	}

	@Override
	public void setResourceLoader(ResourceLoader resourceLoader) {
		this.resolver = ResourcePatternUtils.getResourcePatternResolver(resourceLoader);
	}
	
	
	public String[] getBasenames() {
		try{
			Set<Resource> resources = findResources(resolver, basePackages, resourcePatterns);
			if(resources == null) return null;
			String[] result = new String[resources.size()];
			int i = 0;
			for(Resource r : resources){
				result[i] = StringUtils.stripFilenameExtension(r.getURL().toString());
				logger.warn(" Register: "+result[i]);
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
			Set<Resource> resources = findResources(resolver, basePackages, resourcePatterns);
			if(resources == null) return null;
			String[] result = new String[resources.size()];
			int i = 0;
			for(Resource r : resources){
				result[i] = r.getURI().toString();
				logger.warn(" Register: "+result[i]);
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
			Set<Resource> resources = findResources(resolver, basePackages, resourcePatterns);
			if(resources == null) return null;
			Resource[] result = new Resource[resources.size()];
			int i = 0;
			for(Resource r : resources){
				result[i] = r;
				logger.warn(" Register: "+result[i]);
				i++;
			}
			return result;
		}catch(IOException e){
			e.printStackTrace();
			return null;
		}
	}
	
	
	
	
	private Set<Resource> findResources(ResourcePatternResolver resolver, Iterable<String> basePackages, String[] parttens)throws IOException {
		
		if(resolver == null || basePackages == null || parttens == null ) return null;
		
		HashSet<Resource> resourcesSet = new HashSet<Resource>();
		for(String packageToScan : basePackages){
			
			String classBasedResourcePath = "classpath:"+ClassUtils.convertClassNameToResourcePath(packageToScan);
			String webappBasedResourcePath = "/WEB-INF/"+ClassUtils.convertClassNameToResourcePath(packageToScan);
			
			for(String partten : parttens){
				addResources(resolver, resourcesSet, classBasedResourcePath+partten);
				addResources(resolver, resourcesSet, webappBasedResourcePath+partten);
			}
		}
		if(resourcesSet.size() < 1) return null;
		
		return resourcesSet;
	}

	private void addResources(ResourcePatternResolver resolver, HashSet<Resource> resourcesSet, String pattern) throws IOException {
		logger.warn(" Pattern : "+pattern);
		try{
			Resource[] resources = resolver.getResources(pattern);
			for(Resource r : resources){
				if(r.exists()){
					if(! resourcesSet.contains(r)){
						logger.warn(" Pattern Resource : "+r);
						resourcesSet.add(r);
					}
				}
			}
		}catch(Exception e){
			
		}
	}

}
