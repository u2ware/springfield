package com.u2ware.springfield.support.xstream;

import javax.servlet.ServletContext;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.core.io.Resource;
import org.springframework.util.ClassUtils;
import org.springframework.web.context.ServletContextAware;

import com.thoughtworks.xstream.XStream;

public class XStreamBasedFactoryBean<T> implements FactoryBean<T>, InitializingBean, ServletContextAware{

	private ServletContext servletContext;
	private Class<?> objectType;
	private Resource objectResource;
	private T object;
	
	/////////////////////////////////////
	//
	//////////////////////////////////////
	public void setServletContext(ServletContext servletContext) {
		this.servletContext = servletContext;
	}
	public void setObjectType(Class<?> objectType) {
		this.objectType = objectType;
	}
	public void setObjectResource(Resource objectResource) {
		this.objectResource = objectResource;
	}
	public void setObject(T object) {
		this.object = object;
	}

	/////////////////////////////////////
	//
	//////////////////////////////////////
	public T getObject() throws Exception {
		return object;
	}
	public Class<?> getObjectType() {
		return objectType;
	}
	public boolean isSingleton() {
		return true;
	}

	
	/////////////////////////////////////
	//
	//////////////////////////////////////
	@SuppressWarnings("unchecked")
	public void afterPropertiesSet() throws Exception {
		
		XStream xstream = new XStream();
		xstream.autodetectAnnotations(true);

		Object root = getObjectType().newInstance();  
		xstream.toXML(root);
		
		setObject((T)xstream.fromXML( objectResource.getURL(), root));
		
		servletContext.setAttribute(ClassUtils.getShortNameAsProperty(getObjectType()), this.object);
	}

}
