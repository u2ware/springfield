package com.u2ware.springfield.repository.mongodb;

import java.io.Serializable;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.repository.query.MongoEntityInformation;
import org.springframework.data.mongodb.repository.support.DefaultEntityInformationCreator;
import org.springframework.data.mongodb.repository.support.SimpleMongoRepository;

import com.u2ware.springfield.repository.EntityRepository;

public class EntityMongodbRepository<T,ID extends Serializable> implements EntityRepository<T,ID>{

	protected final Log logger = LogFactory.getLog(getClass());
	
	private final Class<T> entityClass;
	private MongoOperations mongoOperations;
	private MongoEntityInformation<T,ID> entityInformation;
	private SimpleMongoRepository<T, ID> simpleMongoRepository;

	public EntityMongodbRepository(Class<T> entityClass){
		this(entityClass, null);
	}
	public EntityMongodbRepository(Class<T> entityClass, MongoOperations mongoOperations){
		if(mongoOperations !=null)this.setTemplate(mongoOperations);
		this.entityClass = entityClass;
	}

	@Autowired(required=false)
	public void setTemplate(MongoOperations mongoOperations) {
		this.mongoOperations = mongoOperations;
		this.entityInformation = new DefaultEntityInformationCreator(
						mongoOperations.getConverter().getMappingContext())
				.getEntityInformation(entityClass);
		this.simpleMongoRepository = new SimpleMongoRepository<T,ID>(entityInformation, mongoOperations);
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public MongoOperations getTemplate() {
		return mongoOperations;
	}

	//////////////////////////////////
	// exists
	//////////////////////////////////
	@Override
	public boolean exists(ID id) {		
		return simpleMongoRepository.exists(id);
	}
	@Override
	public boolean exists(T entity) {		
		ID id = entityInformation.getId(entity);
		return simpleMongoRepository.exists(id);
	}

	//////////////////////////////////
	// crud
	//////////////////////////////////
	@Override
	public T read(ID id) {		
		if(id == null) return null;
		return simpleMongoRepository.findOne(id);
	}
	@Override
	public T read(T entity) {		
		ID id = entityInformation.getId(entity);
		return simpleMongoRepository.findOne(id);
	}
	@Override
	public T create(T entity) {
		getTemplate().insert(entity);
		return entity;
	}
	@Override
	public T update(T entity) {
		return simpleMongoRepository.save(entity);
	}
	@Override
	public void delete(T entity) {
		simpleMongoRepository.delete(entity);
	}
	@Override
	public void delete(ID id) {
		simpleMongoRepository.delete(id);
	}
	@Override
	public T createOrUpdate(T entity) {
		return simpleMongoRepository.save(entity);
	}
	//////////////////////////////////
	// findAll
	//////////////////////////////////
	@Override
	public List<T> findAll() {
		return simpleMongoRepository.findAll();
	}
	@Override
	public List<T> findAll(Object query) {
		PartTreeQuery spec = new PartTreeQuery(entityClass, query);
		Sort specSort = spec.createSort();
		//logger.info("sort : "+specSort);
		Query mongoQuery = createQuery(spec, null, specSort);
		return getTemplate().find(mongoQuery, entityInformation.getJavaType(), entityInformation.getCollectionName());
	}
	@Override
	public List<T> findAll(Object query, Sort sort) {
		PartTreeQuery spec = new PartTreeQuery(entityClass, query);
		Sort specSort = spec.createSort();
		Sort sortAll =  (specSort != null) ? specSort.and(sort) : sort;
		//logger.info("sort : "+sortAll);
		Query mongoQuery = createQuery(spec, null, sortAll);
		return getTemplate().find(mongoQuery, entityInformation.getJavaType(), entityInformation.getCollectionName());
	}
	@Override
	public Page<T> findAll(Object query, Pageable pageable) {
		PartTreeQuery spec = new PartTreeQuery(entityClass, query);
		Sort specSort = spec.createSort();
		Sort sortAll = (specSort != null) ? specSort.and(pageable.getSort()) : pageable.getSort();
		//logger.info("sort : "+sortAll);

		Query mongoQuery = createQuery(spec, new PageRequest(pageable.getPageNumber(), pageable.getPageSize(), sortAll), null);
		long count = getTemplate().count(mongoQuery, entityInformation.getCollectionName());
		List<T> result = getTemplate().find(mongoQuery, entityClass, entityInformation.getCollectionName());
		return new PageImpl<T>(result, pageable, count);
	}

	@Override
	public long count(Object query) {
		PartTreeQuery spec = new PartTreeQuery(entityClass, query);
		Query mongoQuery = createQuery(spec, null, null);
		long count = getTemplate().count(mongoQuery, entityInformation.getCollectionName());
		return count;
	}
	
	//////////////////////////////////
	// deleteAll
	//////////////////////////////////
	@Override 
	public void deleteAll(){
		simpleMongoRepository.deleteAll();
	}
	
	@Override 
	public void deleteAll(Object query) {
		PartTreeQuery spec = new PartTreeQuery(entityClass, query);
		Query mongoQuery = createQuery(spec, null, null);
		mongoOperations.remove(mongoQuery, entityInformation.getCollectionName());
	}
	
	protected Query createQuery(PartTreeQuery spec, Pageable pageable, Sort sort){
		Query mongoQuery = new Query();
		Criteria criteria = spec.toPredicate(mongoOperations);
		if(criteria != null){
			mongoQuery.addCriteria(criteria);
		}
		return mongoQuery.with(pageable).with(sort);
	}
	
	
}