package com.u2ware.springfield.repository;

import java.io.Serializable;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

public interface EntityRepository<T, ID extends Serializable> {

	public boolean exists(T entity);
	public boolean exists(ID id);//CrudRepository

	public T findOne(T entity);
	public T findOne(ID id);//CrudRepository

	public <S extends T> S save(S entity);//CrudRepository
	public <S extends T> Iterable<S> save(Iterable<S> entities);//CrudRepository

	public void delete(ID id);//CrudRepository
	public void delete(T entity);//CrudRepository
	public void delete(Iterable<? extends T> entities);//CrudRepository


	public long deleteAll();//CrudRepository
	public long count();//CrudRepository
	public List<T> findAll();//CrudRepository
	//public Iterable<T> findAll(Iterable<ID> ids);//CrudRepository
	public Iterable<T> findAll(Sort sort); //PagingAndSortingRepository
	public Page<T> findAll(Pageable pageable); //PagingAndSortingRepository
	
	
	public long deleteAll(Object queryMethod);
	public long count(Object queryMethod);
	public List<T> findAll(Object queryMethod);
	public List<T> findAll(Object queryMethod, Sort sort);
	public Page<T> findAll(Object queryMethod, Pageable pageable);
	
	public <R, X> R execute(TemplateCallback<R, X> callback);
}