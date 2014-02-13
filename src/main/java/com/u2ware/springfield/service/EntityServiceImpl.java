package com.u2ware.springfield.service;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.transaction.support.TransactionTemplate;

import com.u2ware.springfield.repository.EntityRepository;

public class EntityServiceImpl<T, Q> extends AbstractEntityService<T, Q> {

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	private TransactionTemplate transactionTemplate;
	private EntityRepository<T, ?> repository;

	protected EntityServiceImpl(){
	}
	
	protected EntityServiceImpl(TransactionTemplate transactionTemplate, EntityRepository<T, ?> repository) {
		super();
		this.repository = repository;
		this.transactionTemplate = transactionTemplate;
	}

	////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	protected void setRepository(EntityRepository<T, ?> repository) {
		this.repository = repository;
	}
	protected void setTransactionTemplate(TransactionTemplate transactionTemplate) {
		this.transactionTemplate = transactionTemplate;
	}
	protected EntityRepository<T, ?> getRepository() {
		return repository;
	}
	protected TransactionTemplate getTransactionTemplate() {
		return transactionTemplate;
	}
}