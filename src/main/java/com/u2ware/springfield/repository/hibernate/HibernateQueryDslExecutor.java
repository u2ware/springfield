package com.u2ware.springfield.repository.hibernate;

import java.sql.SQLException;
import java.util.List;

import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.orm.hibernate3.HibernateCallback;
import org.springframework.orm.hibernate3.HibernateTemplate;

import com.mysema.query.jpa.hibernate.HibernateDeleteClause;
import com.mysema.query.jpa.hibernate.HibernateQuery;
import com.mysema.query.types.Predicate;
import com.mysema.query.types.path.PathBuilder;
import com.u2ware.springfield.domain.Pagination;
import com.u2ware.springfield.repository.AbstractQueryDslExecutor;

public class HibernateQueryDslExecutor<T> extends AbstractQueryDslExecutor<T>{
	
	private final PathBuilder<T> metamodel;
	private final HibernateTemplate hibernateTemplate;

	public HibernateQueryDslExecutor(Class<T> entityClass, HibernateTemplate hibernateTemplate) {
		this.metamodel = AbstractQueryDslExecutor.createPathBuilder(entityClass);
		this.hibernateTemplate = hibernateTemplate;
	}

	@Override
	public PathBuilder<T> getPathBuilder() {
		return metamodel;
	}

	@Override
	public List<T> findAll(final Predicate predicate, final Sort... sorts) {

		return hibernateTemplate.executeWithNativeSession(new HibernateCallback<List<T>>() {
			public List<T> doInHibernate(Session session) throws HibernateException, SQLException {
				HibernateQuery query = new HibernateQuery(session).from(metamodel);
				applyWhere(query, predicate);
				applySorting(query, sorts);
				return query.list(metamodel);
			}
		});
	}
	
	
	@Override
	public Page<T> findAll(final Predicate predicate, final Pageable pageable, final Sort... sorts) {
		
		return hibernateTemplate.executeWithNativeSession(new HibernateCallback<Page<T>>() {
			public Page<T> doInHibernate(Session session) throws HibernateException, SQLException {
				
				HibernateQuery query = new HibernateQuery(session).from(metamodel);
				applyWhere(query, predicate);
				applySorting(query, sorts);
				applyPagination(query, pageable);
				List<T> content = query.list(metamodel);
				
				
				HibernateQuery countQuery = new HibernateQuery(session).from(metamodel);
				applyWhere(countQuery, predicate);
				long total = countQuery.count();

				return new Pagination<T>(content, pageable, total);
			}
		});
	}
	@Override
	public long count(final Predicate predicate) {
		return hibernateTemplate.executeWithNativeSession(new HibernateCallback<Long>() {
			public Long doInHibernate(Session session) throws HibernateException, SQLException {
				HibernateQuery query = new HibernateQuery(session).from(metamodel);
				applyWhere(query, predicate);
				return query.count();
			}
		});
	}
	@Override
	public long deleteAll(final Predicate predicate) {
		return hibernateTemplate.executeWithNativeSession(new HibernateCallback<Long>() {
			public Long doInHibernate(Session session) throws HibernateException, SQLException {
				HibernateDeleteClause query = new HibernateDeleteClause(session, metamodel);
				applyWhere(query, predicate);
				return query.execute();
			}
		});
	}

	
}
