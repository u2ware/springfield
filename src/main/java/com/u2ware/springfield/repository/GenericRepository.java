package com.u2ware.springfield.repository;

import java.io.Serializable;

import org.springframework.data.repository.NoRepositoryBean;
import org.springframework.data.repository.PagingAndSortingRepository;

@NoRepositoryBean
public interface GenericRepository<T, ID extends Serializable> 
extends PagingAndSortingRepository<T,ID> , QueryMethodExecutor<T>{
	
	public Class<T> getEntityClass();
	public ID getIdValue(T entity);

}


/*
	//PagingAndSortingRepository...
	Iterable<T> findAll(Sort sort);
	Page<T> findAll(Pageable pageable);

	//CrudRepository...
	<S extends T> S save(S entity);
	<S extends T> Iterable<S> save(Iterable<S> entities);
	T findOne(ID id);
	boolean exists(ID id);
	Iterable<T> findAll();
	Iterable<T> findAll(Iterable<ID> ids);
	long count();
	void delete(ID id);
	void delete(T entity);
	void delete(Iterable<? extends T> entities);
	void deleteAll();


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
*/