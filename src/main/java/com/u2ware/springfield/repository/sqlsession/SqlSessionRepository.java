package com.u2ware.springfield.repository.sqlsession;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.ibatis.session.RowBounds;
import org.apache.ibatis.session.SqlSessionFactory;
import org.mybatis.spring.SqlSessionTemplate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.util.ClassUtils;

import com.u2ware.springfield.domain.Pagination;
import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.repository.QueryMethodUtil;
import com.u2ware.springfield.repository.TemplateCallback;

public class SqlSessionRepository<T,ID extends Serializable> implements EntityRepository<T,ID>{

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	private final Class<T> entityClass;
	private final SqlSessionTemplate template;
	
	public SqlSessionRepository(Class<T> entityClass, SqlSessionFactory sqlSessionFactory){
		this.entityClass = entityClass;
		this.template = new SqlSessionTemplate(sqlSessionFactory);
	}
	
	protected String getExistsStatement(){
		return entityClass.getName()+".exists";
	}
	protected String getFindOneStatement(){
		return entityClass.getName()+".findOne";
	}
	protected String getUpdateStatement(){
		return entityClass.getName()+".update";
	}
	protected String getInsertStatement(){
		return entityClass.getName()+".insert";
	}
	protected String getDeleteStatement(){
		return entityClass.getName()+".delete";
	}
	protected String getDeleteAllStatement(Object... query){
		if(query.length == 0 || ClassUtils.isAssignableValue(entityClass, query[0])){
			return entityClass.getName()+".deleteAll";
		}else{
			return entityClass.getName()+"."+QueryMethodUtil.methodName(query[0]).replaceAll("findBy", "deleteBy");
		}
	}
	protected String getCountAllStatement(Object... query){
		if(query.length == 0 || ClassUtils.isAssignableValue(entityClass, query[0])){
			return entityClass.getName()+".countAll";
		}else{
			return entityClass.getName()+"."+QueryMethodUtil.methodName(query[0]).replaceAll("findBy", "countBy");
		}
	}
	protected String getFindAllStatement(Object... query){
		if(query.length == 0 || ClassUtils.isAssignableValue(entityClass, query[0])){
			return entityClass.getName()+".findAll";
		}else{
			return entityClass.getName()+"."+QueryMethodUtil.methodName(query[0]);
		}
	}
	
	
	//////////////////////////////////
	// exists
	//////////////////////////////////
	@Override 
	public boolean exists(ID id) {
		Map<String,Object> paramter = new HashMap<String,Object>();
		paramter.put("id", id);

		Integer result = template.selectOne(getExistsStatement(), paramter);
		return result > 0;
	}
	@Override 
	public boolean exists(T entity) {
		Map<String,Object> paramter = new HashMap<String,Object>();
		paramter.put("entity", entity);

		Integer result = template.selectOne(getExistsStatement(), paramter);
		return result > 0;
	}

	@Override
	public T findOne(ID id) {
		Map<String,Object> paramter = new HashMap<String,Object>();
		paramter.put("id", id);

		T result = template.selectOne(getFindOneStatement(), paramter);
		return result;
	}

	@Override
	public T findOne(T entity) {
		Map<String,Object> paramter = new HashMap<String,Object>();
		paramter.put("entity", entity);

		T result = template.selectOne(getFindOneStatement(), paramter);
		return result;
	}

	@Override
	public <S extends T> S save(S entity) {

		Map<String,Object> parameter = new HashMap<String,Object>();
		parameter.put("entity", entity);
		
		if(exists(entity)){
			template.update(getUpdateStatement(), parameter);
			return entity;
			
		}else{
			template.insert(getInsertStatement(), parameter);
			return entity;
		}
	}

	@Override
	public <S extends T> Iterable<S> save(Iterable<S> entities) {
		for(S entity : entities){
			this.save(entity);
		}
		return entities;
	}

	@Override
	public void delete(ID id) {
		Map<String,Object> parameter = new HashMap<String,Object>();
		parameter.put("id", id);
		
		template.delete(getDeleteStatement(), parameter);
	}

	@Override
	public void delete(T entity) {
		Map<String,Object> parameter = new HashMap<String,Object>();
		parameter.put("entity", entity);
		
		template.delete(getDeleteStatement(), parameter);
	}

	@Override
	public void delete(Iterable<? extends T> entities) {
		for(T entity : entities){
			this.delete(entity);
		}
	}

	//////////////////////////////////////////
	//
	//////////////////////////////////////////
	@Override
	public long deleteAll() {
		Map<String,Object> parameter = new HashMap<String,Object>();

		return template.delete(getDeleteAllStatement(), parameter);
	}
	
	@Override
	public long count() {
		Map<String,Object> parameter = new HashMap<String,Object>();

		Long total = template.selectOne(getCountAllStatement(),  parameter);
		return total;
	}

	@Override
	public List<T> findAll() {
		Map<String,Object> parameter = new HashMap<String,Object>();

		return template.selectList(getFindAllStatement(), parameter);
	}

	@Override
	public List<T> findAll(Sort sort) {
		Map<String,Object> parameter = new HashMap<String,Object>();
		parameter.put("sort", sort);

		return template.selectList(getFindAllStatement(), parameter);
	}

	@Override
	public Page<T> findAll(Pageable pageable) {

		long total = count();
		List<T> contents = null;
		if(total > 0){
			
			Map<String,Object> parameter = new HashMap<String,Object>();
			parameter.put("pageable", pageable);

			int offset = pageable.getPageSize() * pageable.getPageNumber();
			int limit  = pageable.getPageSize();

			contents = template.selectList(getFindAllStatement(), parameter, new RowBounds(offset, limit));
		}else{
			contents = new ArrayList<T>();
		}
		return new Pagination<T>(contents, pageable, total);
	}


	
	
	//////////////////////////////////////////
	//
	//////////////////////////////////////////
	@Override
	public long deleteAll(Object query) {
		Map<String,Object> parameter = new HashMap<String,Object>();
		parameter.put("query", query);
		
		return template.delete(getDeleteAllStatement(query), parameter);
	}
	
	@Override
	public long count(Object query) {
		Map<String,Object> parameter = new HashMap<String,Object>();
		parameter.put("query", query);
		
		Long total = template.selectOne(getCountAllStatement(query),  parameter);
		return total;
	}
	
	@Override
	public List<T> findAll(Object query) {

		Map<String,Object> parameter = new HashMap<String,Object>();
		parameter.put("query", query);

		return template.selectList(getFindAllStatement(query), parameter);
	}

	@Override
	public List<T> findAll(Object query, Sort sort) {
		Map<String,Object> parameter = new HashMap<String,Object>();
		parameter.put("query", query);
		parameter.put("sort", sort);

		return template.selectList(getFindAllStatement(query), parameter);
	}

	@Override
	public Page<T> findAll(Object query, Pageable pageable) {
		long total = count(query);
		List<T> contents = null;
		if(total > 0){
			
			Map<String,Object> parameter = new HashMap<String,Object>();
			parameter.put("query", query);
			parameter.put("pageable", pageable);

			int offset = pageable.getPageSize() * pageable.getPageNumber();
			int limit  = pageable.getPageSize();

			contents = template.selectList(getFindAllStatement(query), parameter, new RowBounds(offset, limit));
		}else{
			contents = new ArrayList<T>();
		}
		return new Pagination<T>(contents, pageable, total);
	}



	//////////////////////////////////////////
	//
	//////////////////////////////////////////
	@Override @SuppressWarnings("unchecked")
	public <R, X> R execute(final TemplateCallback<R,X> callback) {

		Class<?> type = callback.getTemplateType();
		if(ClassUtils.isAssignable(SqlSessionTemplate.class, type)){
			return callback.doInTemplate((X)template);
			
		}else{
			throw new RuntimeException("Template Type is wrong.");
		}
	}
}
