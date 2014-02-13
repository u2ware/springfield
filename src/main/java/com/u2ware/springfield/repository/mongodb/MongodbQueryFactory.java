package com.u2ware.springfield.repository.mongodb;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.repository.support.QuerydslRepositorySupport;

import com.mysema.query.mongodb.MongodbQuery;
import com.mysema.query.types.EntityPath;

public class MongodbQueryFactory extends QuerydslRepositorySupport{

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	public MongodbQueryFactory(MongoOperations operations) {
		super(operations);
	}
	public <T> MongodbQuery<T> from(final EntityPath<T> path) {
		return super.from(path);
	}
	public <T> MongodbQuery<T> from(final EntityPath<T> path, String collection) {
		return super.from(path, collection);
	}
}