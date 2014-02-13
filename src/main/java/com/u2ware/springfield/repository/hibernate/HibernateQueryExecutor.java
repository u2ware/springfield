package com.u2ware.springfield.repository.hibernate;

import org.hibernate.Session;

import com.mysema.query.jpa.JPQLQuery;
import com.mysema.query.jpa.hibernate.HibernateQuery;

public class HibernateQueryExecutor<T> extends AbstractJPQLQueryExecutor<T> {

	private final Session session;
	
	public HibernateQueryExecutor(Session session, Class<T> entityClass) {
		super(entityClass);
		this.session = session;
	}

	@Override
	protected JPQLQuery createQuery() {
		return new HibernateQuery(session);
	}
}
