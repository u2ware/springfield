package com.u2ware.springfield.repository;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Order;

import com.mysema.query.FilteredClause;
import com.mysema.query.SimpleQuery;
import com.mysema.query.types.Expression;
import com.mysema.query.types.OrderSpecifier;
import com.mysema.query.types.Predicate;
import com.mysema.query.types.path.PathBuilder;
import com.mysema.query.types.path.PathBuilderFactory;

public abstract class AbstractQueryDslExecutor<T> implements QueryDslExecutor<T>{

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	private final static PathBuilderFactory pathBuilderFactory = new PathBuilderFactory();

	protected final static <X> PathBuilder<X> createPathBuilder(Class<X> entityClass){
		return pathBuilderFactory.create(entityClass);
	}
	
	
	////////////////////////////////////////////
	//
	////////////////////////////////////////////
	public abstract PathBuilder<T> getPathBuilder();

	public abstract List<T> findAll(Predicate predicate, Sort... sorts);
	public abstract Page<T> findAll(Predicate predicate, Pageable pageable, Sort... sorts);
	public abstract long count(Predicate predicate);
	public abstract long deleteAll(Predicate predicate);
	

	////////////////////////////////////////////
	//
	////////////////////////////////////////////
	public FilteredClause<?> applyWhere(FilteredClause<?> query, Predicate predicate) {
		if (predicate == null) {
			return query;
		}
		query.where(predicate);
		return query;
	}
	
	public SimpleQuery<?> applyPagination(SimpleQuery<?> query, Pageable pageable) {
		if (pageable == null) {
			return query;
		}
		query.offset(pageable.getOffset());
		query.limit(pageable.getPageSize());
		applySorting(query, pageable.getSort());
		return query;
	}

	public SimpleQuery<?> applySorting(SimpleQuery<?> query, Sort... sorts) {

		if (sorts == null) {
			return query;
		}
		for(Sort sort : sorts){
			if(sort != null){
				for (Order order : sort) {
					query.orderBy(toOrder(order));
				}
			}
		}
		return query;
	}
	
	@SuppressWarnings({ "rawtypes", "unchecked" })
	private OrderSpecifier<?> toOrder(Order order) {
		Expression<Object> property = getPathBuilder().get(order.getProperty());
		return new OrderSpecifier(order.isAscending() ? com.mysema.query.types.Order.ASC
				: com.mysema.query.types.Order.DESC, property);
	}
	////////////////////////////////////////////
	//
	////////////////////////////////////////////
	@Override
	public List<T> findAll() {
		return findAll((Predicate)null, (Sort)null, (Sort)null);
	}
	@Override
	public List<T> findAll(Sort sort) {
		return findAll((Predicate)null,  sort, (Sort)null);
	}
	@Override
	public Page<T> findAll(Pageable pageable) {
		return findAll((Predicate)null, pageable, (Sort)null);
	}
	@Override
	public long count() {
		return count((Predicate)null);
	}
	@Override
	public long deleteAll() {
		return deleteAll((Predicate)null);
	}
	
	////////////////////////////////////////////
	//
	////////////////////////////////////////////
	@Override
	public List<T> findAll(Object queryMethod) {
		QueryMethodResolver<T> r = new QueryMethodResolver<T>(getPathBuilder(), queryMethod);
		return findAll(r.predicate(), r.sort(), null);
	}
	@Override
	public List<T> findAll(Object queryMethod, Sort sort) {
		QueryMethodResolver<T> r = new QueryMethodResolver<T>(getPathBuilder(), queryMethod);
		return findAll(r.predicate(), r.sort(), sort);
	}
	
	@Override
	public Page<T> findAll(Object queryMethod, Pageable pageable) {
		QueryMethodResolver<T> r = new QueryMethodResolver<T>(getPathBuilder(), queryMethod);
		return findAll(r.predicate(), pageable, r.sort());
	}
	@Override
	public long count(Object queryMethod) {
		QueryMethodResolver<T> r = new QueryMethodResolver<T>(getPathBuilder(), queryMethod);
		return count(r.predicate());
	}
	@Override
	public long deleteAll(Object queryMethod) {
		QueryMethodResolver<T> r = new QueryMethodResolver<T>(getPathBuilder(), queryMethod);
		return deleteAll(r.predicate());
	}

	
}
