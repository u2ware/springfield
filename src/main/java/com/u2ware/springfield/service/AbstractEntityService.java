package com.u2ware.springfield.service;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Pageable;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionTemplate;

import com.u2ware.springfield.repository.EntityRepository;

public abstract class AbstractEntityService<T, Q> implements EntityService<T, Q> {

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	protected abstract EntityRepository<T, ?> getRepository();

	protected abstract TransactionTemplate getTransactionTemplate();

	////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	/*
	public Object home(final Q query) {
		return query;
	}
	*/
	
	public Iterable<?> find(final Q query, final Pageable pageable) {
		logger.debug("find: "+getRepository());
		logger.debug("find: "+getTransactionTemplate());

		if(getRepository() == null || getTransactionTemplate() == null) return null;
		
		return getTransactionTemplate().execute(new TransactionCallback<Iterable<?>>() {
			public Iterable<?> doInTransaction(TransactionStatus status) {
				if(pageable != null){
					return getRepository().findAll(query, pageable);
				}else{
					return getRepository().findAll(query);
				}
			}
		});
	}
	

	public T readForm(final T entity) {
		logger.debug("readForm: "+getRepository());
		logger.debug("readForm: "+getTransactionTemplate());

		if(getRepository() == null|| getTransactionTemplate() == null) return entity;

		return getTransactionTemplate().execute(new TransactionCallback<T>() {
			public T doInTransaction(TransactionStatus status) {
				T result = getRepository().findOne(entity);
				return result;
			}
		});
	}

	
	public T read(final T entity)  {
		logger.debug("read: "+getRepository());
		logger.debug("read: "+getTransactionTemplate());

		if(getRepository() == null|| getTransactionTemplate() == null) return entity;

		return getTransactionTemplate().execute(new TransactionCallback<T>() {
			public T doInTransaction(TransactionStatus status) {
				T result = getRepository().findOne(entity);
				return result;
			}
		});
	}
	
	public T createForm(final T entity) {
		logger.debug("createForm: "+getRepository());
		logger.debug("createForm: "+getTransactionTemplate());
		return entity;
	}
	
	public T create(final T entity) {
		logger.debug("create: "+getRepository());
		logger.debug("create: "+getTransactionTemplate());

		if(getRepository() == null || getTransactionTemplate() == null) return entity;
		
		return getTransactionTemplate().execute(new TransactionCallback<T>() {
			public T doInTransaction(TransactionStatus status) {
				T result = getRepository().save(entity);
				return result;
			}
		});
	}


	public T updateForm(final T entity) {
		logger.debug("updateForm: "+getRepository());
		logger.debug("updateForm: "+getTransactionTemplate());

		if(getRepository() == null || getTransactionTemplate() == null) return entity;

		return getTransactionTemplate().execute(new TransactionCallback<T>() {
			public T doInTransaction(TransactionStatus status) {
				T result = getRepository().findOne(entity);
				return result;
			}
		});
	}
	
	public T update(final T entity){
		logger.debug("update: "+getRepository());
		logger.debug("update: "+getTransactionTemplate());

		if(getRepository() == null || getTransactionTemplate() == null) return entity;

		return getTransactionTemplate().execute(new TransactionCallback<T>() {
			public T doInTransaction(TransactionStatus status) {
				T result =  getRepository().save(entity);
				return result;
			}
		});
	}

	public T delete(final T entity) {
		logger.debug("delete: "+getRepository());
		logger.debug("delete: "+getTransactionTemplate());

		if(getRepository() == null || getTransactionTemplate() == null) return entity;

		return getTransactionTemplate().execute(new TransactionCallback<T>() {
			public T doInTransaction(TransactionStatus status) {
				getRepository().delete(entity);
				return entity;
			}
		});
	}
}