package com.u2ware.springfield.repository.mongodb;

import java.io.Serializable;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.mapping.MongoPersistentEntity;
import org.springframework.data.mongodb.repository.query.MongoEntityInformation;
import org.springframework.data.mongodb.repository.support.MappingMongoEntityInformation;
import org.springframework.data.mongodb.repository.support.SimpleMongoRepository;

import com.u2ware.springfield.repository.EntityRepository;

public class EntityMongodbRepository<T, ID extends Serializable> implements EntityRepository<T,ID> {

	//private static final Logger logger = LoggerFactory.getLogger(EntityMongodbRepository.class);

	private Class<T> entityClass;
	private MongoOperations mongoOperations;
	private MongoEntityInformation<T, ID> metadata;
	private SimpleMongoRepository<T, ID> repository;
	
	
	public EntityMongodbRepository(Class<T> entityClass) {
		this(entityClass, null);
	}
	public EntityMongodbRepository(Class<T> entityClass, MongoOperations mo) {
		this.entityClass = entityClass;
		if(mo != null) setTemplate(mo);
	}
	
	
	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	@Autowired(required=false) @SuppressWarnings("unchecked") 
	public void setTemplate(MongoOperations mo) {
		if(mongoOperations != null) return;
		this.mongoOperations = mo;
		this.metadata = new MappingMongoEntityInformation<T, ID>(
				(MongoPersistentEntity<T>) 
				(mo.getConverter().getMappingContext().getPersistentEntity(entityClass)));
		this.repository = new SimpleMongoRepository<T,ID>(metadata, mo);
	}
	
	@Override @SuppressWarnings("unchecked")
	public MongoOperations getTemplate() {
		return mongoOperations;
	}

	
	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	@Override
	public boolean exists(T entity) {
		return repository.exists(metadata.getId(entity));
	}
	@Override
	public boolean exists(ID id) {
		return repository.exists(id);
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
		return repository.save(entity);
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
	@Override
	public void deleteAll(Object query) {
		repository.delete(findAll(query));
	}

	/////////////////////////////////////////
	//
	//////////////////////////////////////////
	@Override
	public long count() {
		MongodbQueryExecutor<T> executor = new MongodbQueryExecutor<T>(mongoOperations, entityClass);
		return executor.count();
	}
	@Override
	public List<T> findAll() {
		MongodbQueryExecutor<T> executor = new MongodbQueryExecutor<T>(mongoOperations, entityClass);
		return executor.findAll();
	}
	@Override
	public List<T> findAll(Sort sort) {
		MongodbQueryExecutor<T> executor = new MongodbQueryExecutor<T>(mongoOperations, entityClass);
		return executor.findAll(sort);
	}
	@Override
	public Page<T> findAll(Pageable pageable) {
		MongodbQueryExecutor<T> executor = new MongodbQueryExecutor<T>(mongoOperations, entityClass);
		return executor.findAll(pageable);
	}
	
	
	/////////////////////////////////////////
	//
	//////////////////////////////////////////
	@Override
	public long count(Object query) {
		MongodbQueryExecutor<T> executor = new MongodbQueryExecutor<T>(mongoOperations, entityClass);
		return executor.count(query);
	}

	@Override
	public List<T> findAll(Object query) {
		MongodbQueryExecutor<T> executor = new MongodbQueryExecutor<T>(mongoOperations, entityClass);
		return executor.findAll(query);
	}

	@Override
	public List<T> findAll(Object query, Sort sort) {
		MongodbQueryExecutor<T> executor = new MongodbQueryExecutor<T>(mongoOperations, entityClass);
		return executor.findAll(query, sort);
	}

	@Override
	public Page<T> findAll(Object query, Pageable pageable) {
		MongodbQueryExecutor<T> executor = new MongodbQueryExecutor<T>(mongoOperations, entityClass);
		return executor.findAll(query, pageable);
	}
}
