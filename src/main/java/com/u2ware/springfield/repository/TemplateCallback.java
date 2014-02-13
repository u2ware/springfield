package com.u2ware.springfield.repository;

import java.lang.reflect.ParameterizedType;


public abstract class TemplateCallback<R, X> {
	
	public abstract R doInTemplate(X template);

	
	public final Class<?> getTemplateType(){
		ParameterizedType type = (ParameterizedType)getClass().getGenericSuperclass();
		Class<?> r = (Class<?>)(type.getActualTypeArguments()[1]);
		return r;
	}
	
}