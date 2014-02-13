package com.u2ware.springfield.repository.jpa;

import java.io.Serializable;
import java.util.List;

import javax.persistence.EntityManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.support.JpaEntityInformation;
import org.springframework.data.jpa.repository.support.JpaEntityInformationSupport;
import org.springframework.data.jpa.repository.support.SimpleJpaRepository;
import org.springframework.util.ClassUtils;

import com.u2ware.springfield.domain.Pagination;
import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.repository.TemplateCallback;

public class JpaRepository<T, ID extends Serializable> implements EntityRepository<T, ID> {

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	private final JpaEntityInformation<T, ID> entityInformation;
	private final EntityManager entityManager;

	//private final Class<T> entityClass;
	private final SimpleJpaRepository<T, ID> template;
	private final JpaQueryDslExecutor<T> queryDslExecutor;
	
	@SuppressWarnings("unchecked")
	public JpaRepository(Class<T> entityClass, EntityManager em) {
		this((JpaEntityInformation<T, ID>) JpaEntityInformationSupport.getMetadata(entityClass, em), em);
	}
	
	private JpaRepository(JpaEntityInformation<T, ID> info, EntityManager em) {
		//this.entityClass = info.getJavaType();
		this.entityInformation = info;
		this.entityManager = em;
		
		this.template = new SimpleJpaRepository<T, ID>(info, em);
		this.queryDslExecutor = new JpaQueryDslExecutor<T>(info.getJavaType(), em);
	}
	
	//////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////
	@Override
	public boolean exists(T entity) {
		return template.exists(entityInformation.getId(entity));
	}

	@Override
	public boolean exists(ID id) {
		return template.exists(id);
	}

	@Override
	public T findOne(T entity) {
		return template.findOne(entityInformation.getId(entity));
	}

	@Override
	public T findOne(ID id) {
		return template.findOne(id);
	}

	@Override
	public <S extends T> S save(S entity) {
		return template.save(entity);
	}

	@Override
	public <S extends T> Iterable<S> save(Iterable<S> entities) {
		return template.save(entities);
	}

	@Override
	public void delete(ID id) {
		template.delete(id);
	}

	@Override
	public void delete(T entity) {
		template.delete(entity);
	}

	@Override
	public void delete(Iterable<? extends T> entities) {
		template.delete(entities);
	}

	//////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////
	@Override
	public long deleteAll() {
		return queryDslExecutor.deleteAll();
	}

	@Override
	public long count() {
		return template.count();
	}

	@Override
	public List<T> findAll() {
		return template.findAll();
	}

	@Override
	public Iterable<T> findAll(Sort sort) {
		return template.findAll(sort);
	}

	@Override
	public Page<T> findAll(Pageable pageable) {
		return new Pagination<T>(template.findAll(pageable), pageable);
	}

	//////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////
	@Override
	public long deleteAll(Object queryMethod) {
		return queryDslExecutor.deleteAll(queryMethod);
	}
	
	@Override 
	public long count(Object queryMethod) {
		return queryDslExecutor.count(queryMethod);
	}

	@Override
	public List<T> findAll(Object queryMethod) {
		return queryDslExecutor.findAll(queryMethod);
	}

	@Override
	public List<T> findAll(Object queryMethod, Sort sort) {
		return queryDslExecutor.findAll(queryMethod, sort);
	}

	@Override
	public Page<T> findAll(Object queryMethod, Pageable pageable) {
		return queryDslExecutor.findAll(queryMethod, pageable);
	}
	

	
	//////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////
	@Override  @SuppressWarnings("unchecked")
	public <R, X> R execute(TemplateCallback<R,X> callback) {

		Class<?> type = callback.getTemplateType();
		if(ClassUtils.isAssignable(EntityManager.class, type)){
			return callback.doInTemplate( (X)entityManager );
		}else{
			throw new RuntimeException("Template Type is wrong.");
		}
	}
}