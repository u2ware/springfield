package com.u2ware.springfield.repository.jpa;

import java.io.Serializable;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.support.JpaEntityInformation;
import org.springframework.data.jpa.repository.support.JpaEntityInformationSupport;
import org.springframework.data.jpa.repository.support.SimpleJpaRepository;

import com.u2ware.springfield.repository.EntityRepository;

public class EntityJpaRepository<T, ID extends Serializable> implements EntityRepository<T,ID> {

	//private static final Logger logger = LoggerFactory.getLogger(EntityJpaRepository.class);

	private Class<T> entityClass;
	private EntityManager entityManager;
	private JpaEntityInformation<T, ID> metadata;
	private SimpleJpaRepository<T, ID> repository;
	
	public EntityJpaRepository(Class<T> entityClass) {
		this(entityClass, null);
	}
	public EntityJpaRepository(Class<T> entityClass, EntityManager em) {
		this.entityClass = entityClass;
		if(em != null) setTemplate(em);
	}

	
	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	@PersistenceContext @SuppressWarnings("unchecked")
	public void setTemplate(EntityManager em){
		if(entityManager != null) return;
		this.entityManager = em;
		this.metadata = (JpaEntityInformation<T, ID>)JpaEntityInformationSupport.getMetadata(entityClass, em);
		this.repository = new SimpleJpaRepository<T,ID>(metadata, em);
	}
	
	@Override @SuppressWarnings("unchecked")
	public EntityManager getTemplate() {
		return entityManager;
	}
	
	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	@Override
	public boolean exists(ID id) {
		return repository.exists(id);
	}
	@Override
	public boolean exists(T entity) {
		return repository.exists(metadata.getId(entity));
	}
	@Override
	public T read(ID id) {
		return repository.findOne(id);
	}
	@Override
	public T read(T entity) {
		return repository.findOne(metadata.getId(entity));
	}
	@Override
	public T create(T entity) {
		return repository.save(entity);
	}
	@Override
	public T update(T entity) {
		return repository.save(entity);
	}
	@Override
	public T createOrUpdate(T entity) {
		return repository.saveAndFlush(entity);
	}
	@Override
	public void delete(T entity) {
		repository.delete(entity);
	}
	@Override
	public void delete(ID id) {
		repository.delete(id);
	}
	@Override
	public void deleteAll() {
		repository.deleteAll();
	}

	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	@Override
	public long count() {
		return repository.count();
	}
	@Override
	public List<T> findAll() {
		return repository.findAll();
	}
	@Override
	public List<T> findAll(Sort sort) {
		return repository.findAll(sort);
	}
	@Override
	public Page<T> findAll(Pageable pageable) {
		return repository.findAll(pageable);
	}

	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	@Override
	public long count(Object query) {
		JpaQueryExecutor<T> executor = new JpaQueryExecutor<T>(entityManager, entityClass);
		return executor.count(query);
	}

	@Override
	public List<T> findAll(Object query) {
		JpaQueryExecutor<T> executor = new JpaQueryExecutor<T>(entityManager, entityClass);
		return executor.findAll(query);
	}

	@Override
	public List<T> findAll(Object query, Sort sort) {
		JpaQueryExecutor<T> executor = new JpaQueryExecutor<T>(entityManager, entityClass);
		return executor.findAll(query, sort);
	}

	@Override
	public Page<T> findAll(Object query, Pageable pageable) {
		JpaQueryExecutor<T> executor = new JpaQueryExecutor<T>(entityManager, entityClass);
		return executor.findAll(query, pageable);
	}

	@Override
	public void deleteAll(Object query) {
		repository.delete(findAll(query));
	}
	
}