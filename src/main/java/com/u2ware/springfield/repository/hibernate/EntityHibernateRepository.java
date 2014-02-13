package com.u2ware.springfield.repository.hibernate;

import java.io.Serializable;
import java.sql.SQLException;
import java.util.List;

import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.metadata.ClassMetadata;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanWrapper;
import org.springframework.beans.BeanWrapperImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.orm.hibernate3.HibernateCallback;
import org.springframework.orm.hibernate3.HibernateTemplate;

import com.u2ware.springfield.repository.EntityRepository;

public class EntityHibernateRepository<T, ID extends Serializable>  implements EntityRepository<T,ID> {

	//private static final Logger logger = LoggerFactory.getLogger(EntityHibernateRepository.class);
	
	private Class<T> entityClass;
	private ClassMetadata metadata;
	private HibernateTemplate hibernateTemplate;
	
	public EntityHibernateRepository(Class<T> entityClass) {
		this(entityClass, null);
	}
	public EntityHibernateRepository(Class<T> entityClass, SessionFactory sessionFactory){
		this.entityClass = entityClass;
		if(sessionFactory != null){
			this.setTemplate(sessionFactory);
		}
	}
	

	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	@Autowired(required=false)
	public void setTemplate(SessionFactory sessionFactory){
		this.hibernateTemplate = new HibernateTemplate(sessionFactory);
		this.metadata = sessionFactory.getClassMetadata(entityClass);
	}

	@Override @SuppressWarnings("unchecked")
	public HibernateTemplate getTemplate() {
		return hibernateTemplate;
	}

	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	@SuppressWarnings("unchecked")
	private ID getId(T entity) {
		BeanWrapper bw = new BeanWrapperImpl(entity);
		return (ID)bw.getPropertyValue(metadata.getIdentifierPropertyName());
	}
	private void setId(T entity, ID id) {
		BeanWrapper bw = new BeanWrapperImpl(entity);
		bw.setPropertyValue(metadata.getIdentifierPropertyName() , id);
	}

	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	@Override
	public boolean exists(ID id) {
		return hibernateTemplate.get(entityClass, id) != null;
	}
	@Override
	public boolean exists(T entity) {
		return exists(getId(entity));
	}
	@Override
	public T read(ID id) {
		return hibernateTemplate.get(entityClass, id);
	}
	@Override
	public T read(T entity) {
		return hibernateTemplate.get(entityClass, getId(entity));
	}
	
	@Override @SuppressWarnings("unchecked")
	public T create(T entity) {
		ID id = (ID)hibernateTemplate.save(entity);
		setId(entity, id);
		return  entity;
	}
	@Override
	public T update(T entity) {
		hibernateTemplate.update(entity);
		return entity;
	}
	@Override
	public T createOrUpdate(T entity) {
		hibernateTemplate.saveOrUpdate(entity);
		return entity;
	}
	@Override
	public void delete(T entity) {
		hibernateTemplate.delete(entity);
	}
	@Override
	public void delete(ID id) {
		hibernateTemplate.delete(read(id));
	}
	@Override
	public void deleteAll() {
		hibernateTemplate.deleteAll(findAll());
	}

	@Override
	public void deleteAll(Object query) {
		hibernateTemplate.deleteAll(findAll(query));
	}

	
	/////////////////////////////////////////
	//
	//////////////////////////////////////////
	@Override
	public long count() {
		
		return hibernateTemplate.executeWithNativeSession(new HibernateCallback<Long>() {
			public Long doInHibernate(Session session) throws HibernateException, SQLException {
				HibernateQueryExecutor<T> executor = new HibernateQueryExecutor<T>(session, entityClass);
				return executor.count();
			}
		});
	}

	@Override
	public List<T> findAll() {
		return hibernateTemplate.executeWithNativeSession(new HibernateCallback<List<T>>() {
			public List<T> doInHibernate(Session session) throws HibernateException, SQLException {
				HibernateQueryExecutor<T> executor = new HibernateQueryExecutor<T>(session, entityClass);
				return executor.findAll();
			}
		});
	}

	@Override
	public List<T> findAll(final Sort sort) {
		return hibernateTemplate.executeWithNativeSession(new HibernateCallback<List<T>>() {
			public List<T> doInHibernate(Session session) throws HibernateException, SQLException {
				HibernateQueryExecutor<T> executor = new HibernateQueryExecutor<T>(session, entityClass);
				return executor.findAll(sort);
			}
		});
	}

	@Override
	public Page<T> findAll(final Pageable pageable) {
		return hibernateTemplate.executeWithNativeSession(new HibernateCallback<Page<T>>() {
			public Page<T> doInHibernate(Session session) throws HibernateException, SQLException {
				HibernateQueryExecutor<T> executor = new HibernateQueryExecutor<T>(session, entityClass);
				return executor.findAll(pageable);
			}
		});
	}

	/////////////////////////////////////////
	//
	//////////////////////////////////////////
	@Override
	public long count(final Object query) {
		return hibernateTemplate.executeWithNativeSession(new HibernateCallback<Long>() {
			public Long doInHibernate(Session session) throws HibernateException, SQLException {
				HibernateQueryExecutor<T> executor = new HibernateQueryExecutor<T>(session, entityClass);
				return executor.count(query);
			}
		});
	}

	@Override
	public List<T> findAll(final Object query) {
		return hibernateTemplate.executeWithNativeSession(new HibernateCallback<List<T>>() {
			public List<T> doInHibernate(Session session) throws HibernateException, SQLException {
				HibernateQueryExecutor<T> executor = new HibernateQueryExecutor<T>(session, entityClass);
				return executor.findAll(query);
			}
		});
	}

	@Override
	public List<T> findAll(final Object query, final Sort sort) {
		return hibernateTemplate.executeWithNativeSession(new HibernateCallback<List<T>>() {
			public List<T> doInHibernate(Session session) throws HibernateException, SQLException {
				HibernateQueryExecutor<T> executor = new HibernateQueryExecutor<T>(session, entityClass);
				return executor.findAll(query, sort);
			}
		});
	}

	@Override
	public Page<T> findAll(final Object query, final Pageable pageable) {
		return hibernateTemplate.executeWithNativeSession(new HibernateCallback<Page<T>>() {
			public Page<T> doInHibernate(Session session) throws HibernateException, SQLException {
				HibernateQueryExecutor<T> executor = new HibernateQueryExecutor<T>(session, entityClass);
				return executor.findAll(query, pageable);
			}
		});
	}
}
