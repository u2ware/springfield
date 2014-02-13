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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.orm.hibernate3.HibernateCallback;
import org.springframework.orm.hibernate3.HibernateTemplate;
import org.springframework.util.ClassUtils;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.repository.TemplateCallback;

public class HibernateRepository<T, ID extends Serializable> implements EntityRepository<T,ID> {

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	private final Class<T> entityClass;
	private final HibernateTemplate template;
	private final HibernateQueryDslExecutor<T> queryDslExecutor;
	
	public HibernateRepository(Class<T> entityClass, SessionFactory sessionFactory){
		this.entityClass = entityClass;
		this.template = new HibernateTemplate(sessionFactory);
		this.queryDslExecutor = new HibernateQueryDslExecutor<T>(entityClass, template);
	}


	//////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////
	@Override
	public boolean exists(final T entity) {
		return template.executeWithNativeSession(new HibernateCallback<Boolean>() {
			@SuppressWarnings("unchecked")
			public Boolean doInHibernate(Session session) throws HibernateException, SQLException {
				ClassMetadata metadata = session.getSessionFactory().getClassMetadata(entityClass);
				BeanWrapper bw = new BeanWrapperImpl(entity);
				ID id = (ID)bw.getPropertyValue(metadata.getIdentifierPropertyName());

				return session.get(entityClass, id) != null;
			}
		});
	}
	
	@Override
	public boolean exists(ID id) {
		return template.get(entityClass, id) != null;
	}

	@Override
	public T findOne(final T entity) {
		return template.executeWithNativeSession(new HibernateCallback<T>() {
			@SuppressWarnings("unchecked")
			public T doInHibernate(Session session) throws HibernateException, SQLException {
				ClassMetadata metadata = session.getSessionFactory().getClassMetadata(entityClass);
				BeanWrapper bw = new BeanWrapperImpl(entity);
				ID id = (ID)bw.getPropertyValue(metadata.getIdentifierPropertyName());
				
				return (T)session.get(entityClass, id);
			}
		});
	}
	
	@Override
	public T findOne(ID id) {
		return template.get(entityClass, id);
	}

	@Override
	public <S extends T> S save(final S entity) {
		return template.executeWithNativeSession(new HibernateCallback<S>() {
			public S doInHibernate(Session session) throws HibernateException, SQLException {
				session.saveOrUpdate(entity);
				return entity;
			}
		});
	}

	@Override
	public <S extends T> Iterable<S> save(final Iterable<S> entities) {
		return template.executeWithNativeSession(new HibernateCallback<Iterable<S>>() {
			public Iterable<S> doInHibernate(Session session) throws HibernateException, SQLException {
				for(S entity : entities){
					session.saveOrUpdate(entity);
				}
				return entities;
			}
		});
	}

	@Override
	public void delete(final ID id) {
		template.executeWithNativeSession(new HibernateCallback<Object>() {
			public Object doInHibernate(Session session) throws HibernateException, SQLException {
				Object entity = session.get(entityClass, id);
				session.delete(entity);
				return null;
			}
		});
	}

	@Override
	public void delete(T entity) {
		template.delete(entity);
	}

	@Override
	public void delete(final Iterable<? extends T> entities) {
		template.executeWithNativeSession(new HibernateCallback<Object>() {
			public Object doInHibernate(Session session) throws HibernateException, SQLException {
				for(Object entity : entities){
					session.delete(entity);
				}
				return null;
			}
		});
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
		return queryDslExecutor.count();
	}
	@Override
	public List<T> findAll() {
		return queryDslExecutor.findAll();
	}
	@Override
	public List<T> findAll(Sort sort) {
		return queryDslExecutor.findAll(sort);
	}
	@Override
	public Page<T> findAll(Pageable pageable) {
		return queryDslExecutor.findAll(pageable);
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
	@Override @SuppressWarnings("unchecked")
	public <R, X> R execute(final TemplateCallback<R,X> callback) {

		Class<?> type = callback.getTemplateType();
		if(ClassUtils.isAssignable(Session.class, type)){
			return template.executeWithNativeSession(new HibernateCallback<R>() {
				public R doInHibernate(Session session) throws HibernateException, SQLException {
					return callback.doInTemplate( (X)session );
				}
			});
		}else{
			throw new RuntimeException("Template Type is wrong.");
		}
	}
}
