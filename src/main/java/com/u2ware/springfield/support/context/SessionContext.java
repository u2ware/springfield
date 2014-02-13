package com.u2ware.springfield.support.context;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import org.springframework.util.ClassUtils;


public class SessionContext implements Serializable, ContextBroker{

	private static final long serialVersionUID = 5428377917566924847L;
	
	public Map<String,Object> attributes = new HashMap<String,Object>();

	public <O> void put(O object){
		String name = ClassUtils.getQualifiedName(object.getClass());
		attributes.put(name, object);
	}

	public <O> O get(Class<O> type){
		return get(type, true);
	}

	@SuppressWarnings("unchecked")
	public <O> O get(Class<O> type, boolean throwException){
		String name = ClassUtils.getQualifiedName(type);
		if(! attributes.containsKey(name) && throwException)
			throw new NullPointerException(type+" is not found in SessionContextBroker  ");
		return (O)attributes.get(name);
	}
	
	@SuppressWarnings("unchecked")
	public <O> O remove(Class<O> type) {
		String name = ClassUtils.getQualifiedName(type);
		return (O)attributes.remove(name);
	}
}