package com.u2ware.springfield.repository.jpa;

import java.util.List;

import javax.persistence.EntityManager;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import com.mysema.query.jpa.impl.JPADeleteClause;
import com.mysema.query.jpa.impl.JPAQuery;
import com.mysema.query.types.Predicate;
import com.mysema.query.types.path.PathBuilder;
import com.u2ware.springfield.domain.Pagination;
import com.u2ware.springfield.repository.AbstractQueryDslExecutor;

public class JpaQueryDslExecutor<T> extends AbstractQueryDslExecutor<T>{

	private final PathBuilder<T> metamodel;
	private final EntityManager entityManager;

	public JpaQueryDslExecutor(Class<T> entityClass, EntityManager entityManager) {
		this.metamodel = AbstractQueryDslExecutor.createPathBuilder(entityClass);
		this.entityManager = entityManager;
	}

	@Override
	public PathBuilder<T> getPathBuilder() {
		return metamodel;
	}
	
	@Override
	public List<T> findAll(Predicate predicate, Sort... sorts) {
		JPAQuery query = new JPAQuery(entityManager).from(metamodel);
		applyWhere(query, predicate);
		applySorting(query, sorts);
		return query.list(metamodel);
	}
	
	@Override
	public Page<T> findAll(Predicate predicate, Pageable pageable, Sort... sorts) {
		JPAQuery query = new JPAQuery(entityManager).from(metamodel);
		applyWhere(query, predicate);
		applySorting(query, sorts);
		applyPagination(query, pageable);
		List<T> content = query.list(metamodel);
		
		JPAQuery countQuery = new JPAQuery(entityManager).from(metamodel);
		applyWhere(countQuery, predicate);
		long total = countQuery.count();

		return new Pagination<T>(content, pageable, total);
	}
	@Override
	public long count(Predicate predicate) {
		JPAQuery query = new JPAQuery(entityManager).from(metamodel);
		applyWhere(query, predicate);
		return query.count();
	}
	@Override
	public long deleteAll(Predicate predicate) {
		JPADeleteClause query = new JPADeleteClause(entityManager, metamodel);
		applyWhere(query, predicate);
		return query.execute();
	}
}
