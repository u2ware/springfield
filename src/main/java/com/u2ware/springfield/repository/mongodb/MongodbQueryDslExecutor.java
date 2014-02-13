package com.u2ware.springfield.repository.mongodb;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoOperations;

import com.mysema.query.mongodb.MongodbQuery;
import com.mysema.query.types.Predicate;
import com.mysema.query.types.path.PathBuilder;
import com.u2ware.springfield.domain.Pagination;
import com.u2ware.springfield.repository.AbstractQueryDslExecutor;

public class MongodbQueryDslExecutor<T> extends AbstractQueryDslExecutor<T> {

	private final PathBuilder<T> metamodel;
	private final MongodbQueryFactory factory;

	public MongodbQueryDslExecutor(Class<T> entityClass, MongoOperations operations) {
		this.metamodel = AbstractQueryDslExecutor.createPathBuilder(entityClass);
		this.factory = new MongodbQueryFactory(operations);
	}

	@Override
	public PathBuilder<T> getPathBuilder() {
		return metamodel;
	}

	@Override
	public List<T> findAll(Predicate predicate, Sort... sorts) {
		MongodbQuery<T> query = factory.from(metamodel);
		applyWhere(query, predicate);
		applySorting(query, sorts);
		return query.list(metamodel);
	}
	
	@Override
	public Page<T> findAll(Predicate predicate, Pageable pageable, Sort... sorts) {
		MongodbQuery<T> query = factory.from(metamodel);
		applyWhere(query, predicate);
		applySorting(query, sorts);
		applyPagination(query, pageable);
		List<T> content = query.list(metamodel);
		
		MongodbQuery<T> countQuery = factory.from(metamodel);
		applyWhere(countQuery, predicate);
		long total = countQuery.count();

		return new Pagination<T>(content, pageable, total);
	}
	@Override
	public long count(Predicate predicate) {
		MongodbQuery<T> query = factory.from(metamodel);
		applyWhere(query, predicate);
		return query.count();
	}

	@Override
	public long deleteAll(Predicate predicate) {
		throw new RuntimeException("not supported!!!");
	}
}
