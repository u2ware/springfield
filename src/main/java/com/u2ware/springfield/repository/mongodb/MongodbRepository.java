package com.u2ware.springfield.repository.mongodb;

import java.io.Serializable;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.mapping.MongoPersistentEntity;
import org.springframework.data.mongodb.repository.query.MongoEntityInformation;
import org.springframework.data.mongodb.repository.support.MappingMongoEntityInformation;
import org.springframework.data.mongodb.repository.support.SimpleMongoRepository;

import com.u2ware.springfield.domain.Pagination;
import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.repository.TemplateCallback;

public class MongodbRepository<T, ID extends Serializable> implements EntityRepository<T,ID> {

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	private final MongoEntityInformation<T, ID> entityInformation;
	private final MongoOperations mongoOperations;
	
	//private final Class<T> entityClass;
	private final SimpleMongoRepository<T,ID> template;
	private final MongodbQueryDslExecutor<T> queryDslExecutor;
	
	@SuppressWarnings("unchecked")
	public MongodbRepository(Class<T> entityClass, MongoOperations operations) {
		this(new MappingMongoEntityInformation<T, ID>(
				(MongoPersistentEntity<T>) 
					(operations.getConverter().getMappingContext().getPersistentEntity(entityClass))
			) , operations);
	}

	private MongodbRepository(MongoEntityInformation<T, ID> info, MongoOperations operations) {
		//this.entityClass = info.getJavaType();
		this.entityInformation = info;
		this.mongoOperations = operations;
		
		this.template = new SimpleMongoRepository<T,ID>(info, operations);
		this.queryDslExecutor = new MongodbQueryDslExecutor<T>(info.getJavaType(), operations);
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
		template.deleteAll();
		return Integer.MIN_VALUE;
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
	@Override @SuppressWarnings("unchecked")
	public <R, X> R execute(final TemplateCallback<R,X> callback) {
		return callback.doInTemplate((X)mongoOperations);
		
		/*
		Class<X> clazz = (Class<X>) ((ParameterizedType) callback.getClass()
                .getGenericSuperclass()).getActualTypeArguments()[1];
		
		if(ClassUtils.isAssignable(MongoOperations.class, clazz)){
			return callback.doInTemplate((X)mongoOperations);
			
		}else{
			throw new RuntimeException("Template Type is wrong.");
		}
		*/
	}

}
