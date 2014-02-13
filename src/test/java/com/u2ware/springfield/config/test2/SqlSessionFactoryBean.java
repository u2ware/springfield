package com.u2ware.springfield.config.test2;

import org.springframework.beans.factory.BeanNameAware;
import org.springframework.context.ResourceLoaderAware;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;

import com.u2ware.springfield.support.resource.ResourcePatternResolverBean;

public class SqlSessionFactoryBean extends org.mybatis.spring.SqlSessionFactoryBean implements ResourceLoaderAware, BeanNameAware{

	private ResourcePatternResolverBean mapperLocationsResolver = new ResourcePatternResolverBean();
	private ResourcePatternResolverBean configLocationResolver = new ResourcePatternResolverBean();

	@Override
	public void setResourceLoader(ResourceLoader resourceLoader) {
		mapperLocationsResolver.setResourceLoader(resourceLoader);
		configLocationResolver.setResourceLoader(resourceLoader);
	}
	@Override
	public void setBeanName(String beanName) {
		mapperLocationsResolver.setBeanName(beanName);
		configLocationResolver.setBeanName(beanName);
	}
	public void setPackagesToScan(String[] packagesToScan) {
		mapperLocationsResolver.setPackagesToScan(packagesToScan);
		configLocationResolver.setPackagesToScan(packagesToScan);
	}
	public void setMapperLocationsPatterns(String[] mapperLocationsPatterns) {
		mapperLocationsResolver.setResourcePatterns(mapperLocationsPatterns);
	}
	public void setConfigLocationPatterns(String[] configLocationPatterns) {
		configLocationResolver.setResourcePatterns(configLocationPatterns);
	}
	
	@Override
	public void afterPropertiesSet() throws Exception {
		Resource[] configLocations = configLocationResolver.getResources();
		if(configLocations != null){
			super.setConfigLocation(configLocations[0]);
		}
		Resource[] mapperLocations = mapperLocationsResolver.getResources();
		if(mapperLocations != null){
			super.setMapperLocations(mapperLocations);
		}
		super.afterPropertiesSet();
	}
}
