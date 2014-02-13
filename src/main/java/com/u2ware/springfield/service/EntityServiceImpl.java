package com.u2ware.springfield.service;


import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.domain.EntityPageImpl;
import com.u2ware.springfield.domain.EntityPageable;
import com.u2ware.springfield.repository.EntityRepository;

public class EntityServiceImpl<T, Q> implements EntityService<T, Q>  {

	protected final Log logger = LogFactory.getLog(getClass());

	private EntityRepository<T, ?> repository;
	private String repositoryName;

	public EntityServiceImpl(EntityRepository<T, ?> repository){
		this.repository = repository;
	}
	public EntityServiceImpl(String repositoryName, EntityRepository<T, ?> repository){
		this.repositoryName = repositoryName;
		this.repository = repository;
	}

	////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	public EntityRepository<T, ?> getRepository() {
		logger.info("repository : "+getRepositoryName());
		return repository;
	}
	public String getRepositoryName() {
		return repositoryName;
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
		
	////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	public Object home(Q query) {
		return query;
	}

	@Transactional
	public Iterable<T> findForm(Q query, EntityPageable pageable) {
		if(pageable != null && pageable.isEnable()){
			return new EntityPageImpl<T>(getRepository().findAll(query, pageable));
		}else{
			return getRepository().findAll(query);
		}
	}
	
	@Transactional
	public Iterable<T> find(Q query, EntityPageable pageable) {
		if(pageable != null && pageable.isEnable()){
			return new EntityPageImpl<T>(getRepository().findAll(query, pageable));
		}else{
			return getRepository().findAll(query);
		}
	}

		
	
	@Transactional
	public T read(final T entity)  {
		T result = getRepository().read(entity);
		return result;
	}
	
	
	public T createForm(T entity) {
		return entity;
	}
	
	@Transactional
	public T create(final T entity) {
		T result = null;
		if(! getRepository().exists(entity, true)){
			result = getRepository().create(entity);
		}
		return result;
	}


	@Transactional
	public T updateForm(T entity) {
		T result = getRepository().read(entity);
		return result;
	}
	
	@Transactional
	public T update(final T entity){
		T result =  getRepository().update(entity);
		return result;
	}

	@Transactional
	public T delete(final T entity) {
		getRepository().delete(entity);
		return entity;
	}
}