package com.u2ware.springfield.repository.mongodb;

import org.springframework.data.mongodb.core.MongoOperations;

import com.mysema.query.mongodb.MongodbQuery;

public class MongodbQueryExecutor<T> extends AbstractMongodbQueryExecutor<T>{

	public MongodbQueryExecutor(MongoOperations mongoOperations, Class<T> entityClass) {
		super(entityClass);
	}

	@Override
	protected MongodbQuery<T> createQuery() {
		return null;
	}

}
