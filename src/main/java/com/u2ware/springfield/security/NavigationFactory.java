package com.u2ware.springfield.security;

import javax.servlet.ServletContext;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.core.io.Resource;
import org.springframework.util.ObjectUtils;
import org.springframework.web.context.ServletContextAware;

import com.thoughtworks.xstream.XStream;

public class NavigationFactory implements FactoryBean<Navigation>, InitializingBean, ServletContextAware{

	protected final Log logger = LogFactory.getLog(getClass());

	private ServletContext servletContext;
	private Resource[] resources;
	private Navigation object;
	
	/////////////////////////////////////
	//
	//////////////////////////////////////
	public void setServletContext(ServletContext servletContext) {
		this.servletContext = servletContext;
	}
	public void setResources(Resource[] resources) {
		this.resources = resources;
	}

	/////////////////////////////////////
	//
	//////////////////////////////////////
	public Navigation getObject() throws Exception {
		return object;
	}
	public Class<?> getObjectType() {
		return Navigation.class;
	}
	public boolean isSingleton() {
		return true;
	}

	
	/////////////////////////////////////
	//
	//////////////////////////////////////
	public void afterPropertiesSet() throws Exception {
		
		
		try{
			if(ObjectUtils.isEmpty(resources)){
				throw new Exception("resources is not found");
			}

			XStream xstream = new XStream();
			xstream.autodetectAnnotations(true);

			Object root = getObjectType().newInstance();  
			xstream.toXML(root);
			
			this.object = (Navigation)xstream.fromXML( resources[0].getURL(), root);
			
			servletContext.setAttribute(Navigation.OBJECT_NAME, this.object);
			logger.warn("Navigation build success.");

		}catch(Exception e){
			logger.warn("Navigation build failure.", e);
		}
	}

	
}
