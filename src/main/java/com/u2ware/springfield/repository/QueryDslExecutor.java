package com.u2ware.springfield.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import com.mysema.query.types.Predicate;

public interface QueryDslExecutor<T> {

	public Iterable<T> findAll(Predicate predicate, Sort... sort);
	public Page<T> findAll(Predicate predicate, Pageable pageable, Sort... sort);
	public long count(Predicate predicate);
	public long deleteAll(Predicate predicate);

	public List<T> findAll();
	public List<T> findAll(Sort sort);
	public Page<T> findAll(Pageable pageable);
	public long count();
	public long deleteAll();
	

	public List<T> findAll(Object queryMethod);
	public List<T> findAll(Object queryMethod, Sort sort);
	public Page<T> findAll(Object queryMethod, Pageable pageable);
	public long count(Object queryMethod);
	public long deleteAll(Object queryMethod);

	
}
