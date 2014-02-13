package com.u2ware.springfield.service;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.u2ware.springfield.domain.EntityPageImpl;
import com.u2ware.springfield.domain.EntityPageable;

public class EntityServiceTemplate<T,Q> implements EntityService<T, Q>{

	protected final Log logger = LogFactory.getLog(getClass());

	public EntityServiceTemplate(){
	}
	
	public Object home(Q query) {
		return query;
	}
	public Iterable<T> findForm(Q query, EntityPageable pageable) {
		return new EntityPageImpl<T>();
	}

	public Iterable<T> find(Q query, EntityPageable pageable) {
		return new EntityPageImpl<T>();
	}
	
	public T createForm(T entity) {
		return entity;
	}

	public T create(T entity) {
		return entity;
	}

	public T read(T entity) {
		return entity;
	}

	public T updateForm(T entity) {
		return entity;
	}

	public T update(T entity) {
		return entity;
	}

	public T delete(T entity) {
		return entity;
	}

	////////////////////////////////////
	//
	////////////////////////////////////
	private ServiceEventDispatcher eventDispatcher;

	protected void addEventListener(ServiceEventListener listener){
		if(eventDispatcher == null)
			this.eventDispatcher = new ServiceEventDispatcher();
		eventDispatcher.addEventListener(listener);
	}
	protected void removeEventListener(ServiceEventListener listener){
		if(eventDispatcher != null)
			eventDispatcher.addEventListener(listener);
	}
	
	protected void firePreHandle(String targetMethod, Object source){
		if(eventDispatcher != null)
			eventDispatcher.firePreHandle(getClass(), targetMethod, source);
	}
	protected void firePostHandle(String targetMethod, Object source){
		if(eventDispatcher != null)
			eventDispatcher.firePostHandle(getClass(), targetMethod, source);
	}

}
