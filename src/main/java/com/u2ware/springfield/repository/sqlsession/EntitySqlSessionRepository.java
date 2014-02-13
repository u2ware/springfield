package com.u2ware.springfield.repository.sqlsession;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.ibatis.session.RowBounds;
import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.dao.InvalidDataAccessResourceUsageException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.repository.QueryMethod;

public class EntitySqlSessionRepository<T,ID extends Serializable> implements EntityRepository<T,ID>{

	protected final Log logger = LogFactory.getLog(getClass());

	private String sqlSessionNamespace;
	private SqlSessionTemplate sqlSessionTemplate;
	
	public EntitySqlSessionRepository(Class<T> entityClass){
		this(entityClass, null);
	}
	public EntitySqlSessionRepository(Class<T> entityClass, SqlSessionTemplate sqlSessionTemplate){
		if(sqlSessionTemplate !=null)this.setTemplate(sqlSessionTemplate);
		this.sqlSessionNamespace = entityClass.getName();
	}

	@Autowired(required=false)
	public void setTemplate(SqlSessionTemplate sqlSessionTemplate){
		this.sqlSessionTemplate = sqlSessionTemplate;
	}

	@Override @SuppressWarnings("unchecked")
	public SqlSessionTemplate getTemplate() {
		return sqlSessionTemplate;
	}

	
	
	//////////////////////////////////
	// exists
	//////////////////////////////////
	@Override 
	public boolean exists(ID id) {		
		throw new InvalidDataAccessResourceUsageException("exists");
	}
	@Override 
	public boolean exists(T entity) {		
		String statement = sqlSessionNamespace+".exists";
		Integer result = (Integer)getTemplate().selectOne(statement, entity);
		return result == 1;
	}

	
	//////////////////////////////////
	// crud
	//////////////////////////////////
	@Override 
	public T create(T entity) {
		String statement = sqlSessionNamespace+".create";
		getTemplate().insert(statement, entity);
		return entity;
	}

	@Override 
	public T read(ID id) {
		throw new InvalidDataAccessResourceUsageException("read");
	}

	@Override 
	public T read(T entity) {
		String statement = sqlSessionNamespace+".read";
		T result = getTemplate().selectOne(statement, entity);
		return result;
	}

	@Override 
	public T update(T entity) {
		String statement = sqlSessionNamespace+".update";
		int result = getTemplate().update(statement, entity);
		if(result == 1){
			return entity;
		}else{
			throw new InvalidDataAccessResourceUsageException("update");
		}
	}

	@Override 
	public void delete(T entity) {
		String statement = sqlSessionNamespace+".delete";
		int result = getTemplate().delete(statement, entity);
		if(result != 1){
			throw new InvalidDataAccessResourceUsageException("delete");
		}
	}
	
	@Override
	public void delete(ID id) {
		throw new InvalidDataAccessResourceUsageException("delete");
	}
	
	@Override 
	public T createOrUpdate(T entity) {
		return exists(entity) ? update(entity) : create(entity);
	}

	
	@Override 
	public long count(Object query) {
		String statement = sqlSessionNamespace+"."+quessStatement(query, "findAll")+"Count";
		Map<String,Object> param = createQueryParameter(query, null, null);

		//logger.debug("statement : "+statement);
		Long total = getTemplate().selectOne(statement,  param);
		//logger.debug("result "+total);
		
		return total;
	}
	//////////////////////////////////
	// find
	//////////////////////////////////
	@Override
	public List<T> findAll() {
		return findAll(null);
	}
	@Override 
	public List<T> findAll(Object query) {
		String statement = sqlSessionNamespace+"."+quessStatement(query, "findAll");
		Map<String,Object> param = createQueryParameter(query, null, null);
		return getTemplate().selectList(statement, param);
	}

	@Override 
	public List<T> findAll(Object query, Sort sort) {
		String statement = sqlSessionNamespace+"."+quessStatement(query, "findAll");
		Map<String,Object> param = createQueryParameter(query, null, sort);
		return getTemplate().selectList(statement, param);
	}
	
	@Override 
	public Page<T> findAll(Object query, Pageable pageable) {

		long total = count(query);
		List<T> contents = null;
		if(total > 0){
			String statement = sqlSessionNamespace+"."+quessStatement(query, "findAll");
			Map<String,Object> param = createQueryParameter(query, pageable, null);
			
			//logger.debug("statement : "+statement);
			int offset = pageable.getPageSize() * pageable.getPageNumber();
			int limit  = pageable.getPageSize();
			contents = getTemplate().selectList(statement, param, new RowBounds(offset, limit));

		}else{
			contents = new ArrayList<T>();
		}

		return new PageImpl<T>(contents, pageable, total);		
	}

	///////////////////////////////////////
	//
	//////////////////////////////////////
	@Override 
	public void deleteAll(){
		deleteAll(null);
	}

	@Override 
	public void deleteAll(Object query) {
		String statement = sqlSessionNamespace+"."+quessStatement(query, "deleteAll");
		Map<String,Object> param = createQueryParameter(query, null, null);
		logger.debug(statement);
		
		int rows = getTemplate().delete(statement, param);
		logger.debug(rows + " rows delete");
	}
	
	protected Map<String,Object> createQueryParameter(Object query, Pageable pageable, Sort sort){
		Map<String,Object> param = new HashMap<String,Object>();
		param.put("query", query);
		param.put("pageable", pageable);
		param.put("sort", sort);
		//logger.debug(param);
		
		return param;
	}

	
	protected String quessStatement(Object query, String suffix){
		
		if(query == null) return suffix;
		
		Class<?> queryClass =  query.getClass();
		if(sqlSessionNamespace.equals(queryClass.getName())){
			return suffix;
		}
		
		Class<?> beanClass = query.getClass();
		String queryMethodName = ClassUtils.getShortNameAsProperty(beanClass);
		QueryMethod queryMethod = AnnotationUtils.findAnnotation(beanClass, QueryMethod.class);

		if(queryMethod != null && StringUtils.hasText(queryMethod.value())){
			queryMethodName = queryMethod.value();
		}

		logger.info(query.getClass() +" quessStatement is "+queryMethodName);
		return queryMethodName;
	}
}

/*
	//////////////////////////////////
	// QueryMethodExecutor
	//////////////////////////////////
	public List<T> findAll(Object query) {
		String sqlSessionStatement = quessQueryMethodName(query);
		return findAll(sqlSessionStatement , query);
	}
	public List<T> findAll(Object query, Sort sort) {
		String sqlSessionStatement = quessQueryMethodName(query);
		return findAll(sqlSessionStatement, query);
	}
	public Page<T> findAll(Object query, Pageable pageable) {
		String sqlSessionStatement = quessQueryMethodName(query);
		return findAll(sqlSessionStatement, query, pageable);
	}
	public List<T> findAll(String statement, Object query) {
		Map<String,Object> sqlSessionParam = quessQueryParameter(query, null, null);
		return getSqlSessionTemplate().selectList(statement, sqlSessionParam);
	}
	public List<T> findAll(String statement, Object query, Sort sort) {
		Map<String,Object> sqlSessionParam = quessQueryParameter(query, null, sort);
		return getSqlSessionTemplate().selectList(statement, sqlSessionParam);
	}
	public Page<T> findAll(String statement, Object query, Pageable pageable) {

		Map<String,Object> sqlSessionParam = quessQueryParameter(query, pageable, null);

		logger.debug("statement : "+statement+"Count");

		Object result = getSqlSessionTemplate().selectOne(statement+"Count",  sqlSessionParam);
		logger.debug(result+" "+result.getClass());
		Long total = (Long)result;

		logger.debug("statement : "+statement);
		int offset = pageable.getPageSize() * pageable.getPageNumber();
		int limit  = pageable.getPageSize();

		List<T> contents = getSqlSessionTemplate().selectList(statement, sqlSessionParam, new RowBounds(offset, limit));
		return new PageImpl<T>(contents, pageable, total);		
	}

	protected Map<String,Object> quessQueryParameter(Object query, Pageable pageable, Sort sort){
		Map<String,Object> param = new HashMap<String,Object>();
		param.put("query", query);
		param.put("pageable", pageable);
		param.put("sort", sort);
		return param;
	}

	//////////////////////////////////
	// NativeQueryExecutor
	//////////////////////////////////
	public List<?> executeQuery(String statement) {
		return getSqlSessionTemplate().selectList(statement);
	}
	public List<?> executeQuery(String statement, Object param) {
		return getSqlSessionTemplate().selectList(statement, param);
	}
	public int executeUpdate(String statement) {
		return getSqlSessionTemplate().update(statement);
	}
	public int executeUpdate(String statement, Object param) {
		return getSqlSessionTemplate().update(statement, param);
	}

	public String[] id() {
		return null;
	}
*/