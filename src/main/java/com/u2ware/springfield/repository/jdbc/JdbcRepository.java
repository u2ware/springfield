package com.u2ware.springfield.repository.jdbc;

import java.io.Serializable;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.jdbc.core.ConnectionCallback;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.util.ClassUtils;

import com.mysema.query.sql.SQLTemplates;
import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.repository.TemplateCallback;

public class JdbcRepository<T, ID extends Serializable> implements EntityRepository<T,ID> {

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	private final Class<T> entityClass;
	private final JdbcTemplate template;
	private final JdbcQueryDslExecutor<T> queryDslExecutor;
	
	public JdbcRepository(Class<T> entityClass, JdbcTemplate template){
		this.entityClass = entityClass;
		this.template = template;
		this.queryDslExecutor = new JdbcQueryDslExecutor<T>(entityClass, template);
	}

	public void setDialect(SQLTemplates dialect) {
		this.queryDslExecutor.setDialect(dialect);
	}

	/////////////////////////////////////////
	//
	/////////////////////////////////////////
	@Override
	public boolean exists(T entity) {
		throw new UnsupportedOperationException(entityClass+ " Not Supported Method");
	}

	@Override
	public boolean exists(ID id) {
		throw new UnsupportedOperationException(entityClass+ " Not Supported Method");
	}

	@Override
	public T findOne(T entity) {
		throw new UnsupportedOperationException(entityClass+ " Not Supported Method");
	}
	@Override
	public T findOne(ID id) {
		throw new UnsupportedOperationException(entityClass+ " Not Supported Method");
	}

	@Override
	public <S extends T> S save(S entity) {
		throw new UnsupportedOperationException(entityClass+ " Not Supported Method");
	}

	@Override
	public <S extends T> Iterable<S> save(Iterable<S> entities) {
		throw new UnsupportedOperationException(entityClass+ " Not Supported Method");
	}

	@Override
	public void delete(ID id) {
		throw new UnsupportedOperationException(entityClass+ " Not Supported Method");
	}

	@Override
	public void delete(T entity) {
		throw new UnsupportedOperationException(entityClass+ " Not Supported Method");
	}

	@Override
	public void delete(Iterable<? extends T> entities) {
		throw new UnsupportedOperationException(entityClass+ " Not Supported Method");
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
		if(ClassUtils.isAssignable(Connection.class, type)){
			return template.execute(new ConnectionCallback<R>() {
				public R doInConnection(Connection conn) throws SQLException, DataAccessException {
					return callback.doInTemplate((X)conn);
				}
			});
		}else if(ClassUtils.isAssignable(JdbcTemplate.class, type)){
			return callback.doInTemplate((X)template);
	
		}else{
			throw new RuntimeException("Template Type is wrong.");
		}
	}
}