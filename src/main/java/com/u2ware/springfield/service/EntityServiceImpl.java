package com.u2ware.springfield.service;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.domain.EntityPageImpl;
import com.u2ware.springfield.domain.EntityPageable;
import com.u2ware.springfield.repository.EntityRepository;

public class EntityServiceImpl<T, Q> implements EntityService<T, Q>  {

	protected static final Logger logger = LoggerFactory.getLogger(EntityServiceImpl.class);

	private EntityRepository<T, ?> repository;

	protected EntityServiceImpl(){
	}
	
	protected EntityServiceImpl(EntityRepository<T, ?> repository) {
		super();
		this.repository = repository;
	}

	////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	protected EntityRepository<T, ?> getRepository() {
		return repository;
	}
	protected void setRepository(EntityRepository<T, ?> repository) {
		this.repository = repository;
	}

/*
	@Override
	public String[] getIdAttributeNames() {
		if(getRepository() == null) return null;
		return getRepository().getIdAttributeNames();
	}
*/	
	////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	@Transactional
	public Object home(Q query) {
		return query;
	}

	public Iterable<?> findForm(Q query, EntityPageable pageable) {
		return find(query, pageable);
	}
	
	@Transactional
	public Iterable<?> find(Q query, EntityPageable pageable) {
		if(getRepository() == null) return new EntityPageImpl<T>();
		if(pageable != null && pageable.isEnable()){
			return new EntityPageImpl<T>(getRepository().findAll(query, pageable));
		}else{
			return getRepository().findAll(query);
		}
	}
	

	public T readForm(T entity) {
		return read(entity);
	}

	
	@Transactional
	public T read(final T entity)  {
		if(getRepository() == null) return entity;
		T result = getRepository().read(entity);
		return result;
	}
	
	
	@Transactional
	public T createForm(T entity) {
		return entity;
	}
	
	@Transactional
	public T create(final T entity) {
		if(getRepository() == null) return entity;
		T result = getRepository().create(entity);
		return result;
	}


	@Transactional
	public T updateForm(T entity) {
		if(getRepository() == null) return entity;
		T result = getRepository().read(entity);
		return result;
	}
	
	@Transactional
	public T update(final T entity){
		if(getRepository() == null) return entity;
		T result =  getRepository().update(entity);
		return result;
	}

	@Transactional
	public T delete(final T entity) {
		if(getRepository() == null) return entity;
		getRepository().delete(entity);
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