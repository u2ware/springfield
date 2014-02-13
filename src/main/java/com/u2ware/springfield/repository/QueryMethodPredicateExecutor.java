package com.u2ware.springfield.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.querydsl.QueryDslPredicateExecutor;

public interface QueryMethodPredicateExecutor<T> extends QueryDslPredicateExecutor<T>{

	public long count();
	public Iterable<T> findAll();
	public Iterable<T> findAll(Sort sort);
	public Page<T> findAll(Pageable pageable);
	
	
	public long count(Object query);
	public Iterable<T> findAll(Object query);
	public Iterable<T> findAll(Object query, Sort sort);
	public Page<T> findAll(Object query, Pageable pageable);
	
}
