package com.u2ware.springfield.repository.jpa;

import javax.persistence.EntityManager;

import com.mysema.query.jpa.JPQLQuery;
import com.mysema.query.jpa.impl.JPAQuery;
import com.u2ware.springfield.repository.hibernate.AbstractJPQLQueryExecutor;

public class JpaQueryExecutor<T> extends AbstractJPQLQueryExecutor<T> {

	private EntityManager entityManager;
	
	public JpaQueryExecutor(EntityManager entityManager, Class<T> entityClass) {
		super(entityClass);
		this.entityManager = entityManager;
	}

	@Override
	protected JPQLQuery createQuery() {
		return  new JPAQuery(entityManager);
	}
}
