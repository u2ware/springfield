package com.u2ware.springfield.support.context;

import javax.servlet.ServletContext;

import org.springframework.util.ClassUtils;
import org.springframework.web.context.ServletContextAware;

public class ServletContextBroker implements ServletContextAware, ContextBroker{

	private ServletContext servletContext;

	public void setServletContext(ServletContext servletContext) {
		this.servletContext = servletContext;
	}

	public <O> void put(O object) {
		String name = ClassUtils.getQualifiedName(object.getClass());
		servletContext.setAttribute(name, object);
	}

	public <O> O get(Class<O> type){
		return get(type, true);
	}

	@SuppressWarnings("unchecked")
	public <O> O get(Class<O> type, boolean throwException){
		String name = ClassUtils.getQualifiedName(type);
		Object result = servletContext.getAttribute(name);
		if(result == null && throwException)
			throw new NullPointerException(type+" is not found in SessionContextBroker  ");
		return (O)result;
	}
	

	public <O> O remove(Class<O> type) {
		String name = ClassUtils.getQualifiedName(type);
		O result = get(type);
		servletContext.removeAttribute(name);
		return result;
	}
}
