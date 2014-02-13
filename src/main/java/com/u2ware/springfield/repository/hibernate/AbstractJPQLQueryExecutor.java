package com.u2ware.springfield.repository.hibernate;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Order;

import com.mysema.query.alias.Alias;
import com.mysema.query.jpa.JPQLQuery;
import com.mysema.query.types.EntityPath;
import com.mysema.query.types.Expression;
import com.mysema.query.types.OrderSpecifier;
import com.mysema.query.types.Predicate;
import com.mysema.query.types.path.PathBuilder;
import com.u2ware.springfield.repository.QueryMethodPredicate;
import com.u2ware.springfield.repository.QueryMethodPredicateExecutor;

public abstract class AbstractJPQLQueryExecutor<T> implements QueryMethodPredicateExecutor<T>{

	private final Class<T> entityClass;
	private final EntityPath<T> path;
	private final PathBuilder<T> builder;

	public AbstractJPQLQueryExecutor(Class<T> entityClass) {
		this.entityClass = entityClass;
		this.path = Alias.$(Alias.alias(entityClass, "a"));
		this.builder = new PathBuilder<T>(path.getType(), path.getMetadata());
	}
	
	public Class<T> getEntityClass() {
		return entityClass;
	}
	public EntityPath<T> getEntityPath() {
		return path;
	}
	public PathBuilder<T> getEntityPathBuilder() {
		return builder;
	}

	///////////////////////////////////////////////////////
	//
	///////////////////////////////////////////////////////
	public long count(Object query) {
		QueryMethodPredicate resolver = new QueryMethodPredicate(getEntityClass(), getEntityPathBuilder(), query);
		Predicate predicate = resolver.getPredicate();
		//Sort sortSet = resolver.getSort();
		return count(predicate);
	}

	public List<T> findAll(Object query) {
		QueryMethodPredicate resolver = new QueryMethodPredicate(getEntityClass(), getEntityPathBuilder(), query);
		Predicate predicate = resolver.getPredicate();
		Sort sortSet = resolver.getSort();
		return findAll(predicate, sortSet);
	}

	public List<T> findAll(Object query, Sort sort) {
		QueryMethodPredicate resolver = new QueryMethodPredicate(getEntityClass(), getEntityPathBuilder(), query);
		Predicate predicate = resolver.getPredicate();
		Sort sortSet = resolver.getSort(sort);
		return findAll(predicate, sortSet);
	}

	public Page<T> findAll(Object query, Pageable pageable) {
		QueryMethodPredicate resolver = new QueryMethodPredicate(getEntityClass(), getEntityPathBuilder(), query);
		Predicate predicate = resolver.getPredicate();
		//Sort sortSet = resolver.getSort();
		return findAll(predicate, pageable);
	}
		
	
	///////////////////////////////////////////////////////
	//
	///////////////////////////////////////////////////////
	public long count() {
		return createQuery(path).count();
	}

	public List<T> findAll() {
		return createQuery(path).list(path);
	}

	public List<T> findAll(Sort sort) {
		return this.applySorting(sort, createQuery(path)).list(path);
	}

	public Page<T> findAll(Pageable pageable) {
		JPQLQuery countQuery = createQuery(path);
		JPQLQuery query = this.applyPagination(pageable, createQuery(path));
		return new PageImpl<T>(query.list(path), pageable, countQuery.count());
	}	
	
	///////////////////////////////////////////////////////
	//
	///////////////////////////////////////////////////////
	public T findOne(Predicate predicate) {
		return createQuery(predicate).uniqueResult(path);
	}

	public List<T> findAll(Predicate predicate) {
		return createQuery(predicate).list(path);
	}

	public List<T> findAll(Predicate predicate, OrderSpecifier<?>... orders) {
		return createQuery(predicate).orderBy(orders).list(path);
	}

	public List<T> findAll(Predicate predicate, Sort sort) {
		return this.applySorting(sort, createQuery(predicate)).list(path);
	}
	
	
	public Page<T> findAll(Predicate predicate, Pageable pageable) {
		JPQLQuery countQuery = createQuery(predicate);
		JPQLQuery query = this.applyPagination(pageable, createQuery(predicate));
		return new PageImpl<T>(query.list(path), pageable, countQuery.count());
	}

	public long count(Predicate predicate) {
		return createQuery(predicate).count();
	}

	/////////////////////////////////////////////////////////////
	//
	/////////////////////////////////////////////////////////////
	protected JPQLQuery createQuery(Predicate... predicate) {
		return this.createQuery(path).where(predicate);
	}
	protected JPQLQuery createQuery(EntityPath<?>... paths) {
		return this.createQuery().from(paths);
	}
	
	protected abstract JPQLQuery createQuery();

	protected JPQLQuery applyPagination(Pageable pageable, JPQLQuery query) {

		if (pageable == null) {
			return query;
		}
		query.offset(pageable.getOffset());
		query.limit(pageable.getPageSize());

		return applySorting(pageable.getSort(), query);
	}

	protected JPQLQuery applySorting(Sort sort, JPQLQuery query) {

		if (sort == null) {
			return query;
		}
		for (Order order : sort) {
			query.orderBy(toOrder(order));
		}
		return query;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	protected OrderSpecifier<?> toOrder(Order order) {
		Expression<Object> property = builder.get(order.getProperty());
		return new OrderSpecifier(order.isAscending() ? com.mysema.query.types.Order.ASC
				: com.mysema.query.types.Order.DESC, property);
	}
}
