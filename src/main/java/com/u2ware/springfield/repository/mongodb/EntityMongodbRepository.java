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

import com.u2ware.springfield.domain.ValidationRejectableException;
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
	public boolean exists(ID id, boolean throwsDuplicateException) {		
		boolean result = simpleMongoRepository.exists(id);
		if(throwsDuplicateException && result){
			throw new ValidationRejectableException("com.u2ware.springfield.repository.DuplicateKey.message");
		}else{
			return result;
		}
	}
	@Override
	public boolean exists(T entity, boolean throwsDuplicateException) {		
		boolean result = false;
		ID id = entityInformation.getId(entity);
		if(id != null){
			result = simpleMongoRepository.exists(id);
		}
		if(throwsDuplicateException && result){
			throw new ValidationRejectableException("com.u2ware.springfield.repository.DuplicateKey.message");
		}else{
			return result;
		}
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
	public T createOrUpdate(T entity) {
		return simpleMongoRepository.save(entity);
	}
	//////////////////////////////////
	// findAll
	//////////////////////////////////
	@Override
	public List<T> findAll(Object query) {
		PartTreeQuery spec = new PartTreeQuery(entityClass, query);
		Sort specSort = spec.createSort();
		logger.info("sort : "+specSort);
		Query mongoQuery = createQuery(spec, null, specSort);
		return getTemplate().find(mongoQuery, entityInformation.getJavaType(), entityInformation.getCollectionName());
	}
	@Override
	public List<T> findAll(Object query, Sort sort) {
		PartTreeQuery spec = new PartTreeQuery(entityClass, query);
		Sort specSort = spec.createSort();
		Sort sortAll =  (specSort != null) ? specSort.and(sort) : sort;
		logger.info("sort : "+sortAll);
		Query mongoQuery = createQuery(spec, null, sortAll);
		return getTemplate().find(mongoQuery, entityInformation.getJavaType(), entityInformation.getCollectionName());
	}
	@Override
	public Page<T> findAll(Object query, Pageable pageable) {
		PartTreeQuery spec = new PartTreeQuery(entityClass, query);
		Sort specSort = spec.createSort();
		Sort sortAll = (specSort != null) ? specSort.and(pageable.getSort()) : pageable.getSort();
		logger.info("sort : "+sortAll);

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
	
	
	
	protected Query createQuery(PartTreeQuery spec, Pageable pageable, Sort sort){
		Query mongoQuery = new Query();
		Criteria criteria = spec.toPredicate(mongoOperations);
		if(criteria != null){
			mongoQuery.addCriteria(criteria);
		}
		return mongoQuery.with(pageable).with(sort);
	}
}

/*


	//////////////////////////////////
	// QueryMethodExecutor
	//////////////////////////////////
	public List<T> findAll(Object query) {
		return findAll(quessQueryMethodName(query), query);
	}
	public List<T> findAll(Object query, Sort sort) {
		return findAll(quessQueryMethodName(query), query, sort);
	}
	public Page<T> findAll(Object query, Pageable pageable) {
		return findAll(quessQueryMethodName(query), query, pageable);
	}
	public List<T> findAll(String queryMethodName, Object query) {
		return findAll(new PartTreeQuerySpecification<T>(queryMethodName, entityClass, query));
	}
	public List<T> findAll(String queryMethodName, Object query, Sort sort) {
		return findAll(new PartTreeQuerySpecification<T>(queryMethodName, entityClass, query), sort);
	}
	public Page<T> findAll(String queryMethodName, Object query, Pageable pageable) {
		return findAll(new PartTreeQuerySpecification<T>(queryMethodName, entityClass, query), pageable);
	}

	protected String quessQueryMethodName(Object query){

		Class<?> beanClass = query.getClass();
		String queryMethodName = ClassUtils.getShortNameAsProperty(beanClass);
		QueryMethod queryMethod = AnnotationUtils.findAnnotation(beanClass, QueryMethod.class);

		if(queryMethod != null && StringUtils.hasText(queryMethod.value())){
			queryMethodName = queryMethod.value();
		}

		if(! queryMethodName.toLowerCase().startsWith("findby")){
			queryMethodName = "findBy";
		}

		logger.debug(query.getClass() +" quessQueryMethodName is "+queryMethodName);
		return queryMethodName;
	}

	//////////////////////////////////
	// Specification...
	//////////////////////////////////
	public List<T> findAll(Specification<T> spec) {
		Query query = getQuery(spec, (Sort) null);
		return mongoOperations.find(query, entityClass, entityInformation.getCollectionName());
	}

	public List<T> findAll(Specification<T> spec, Sort sort) {
		Query query = getQuery(spec, (Sort) sort);
		return mongoOperations.find(query, entityClass, entityInformation.getCollectionName());
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public Page<T> findAll(Specification<T> spec, Pageable pageable) {
		Query query = getQuery(spec, pageable);
		long count = mongoOperations.count(query, entityInformation.getCollectionName());
		List<?> result = mongoOperations.find(query.with(pageable), entityClass, entityInformation.getCollectionName());
		return new PageImpl(result, pageable, count);
	}
	private Query getQuery(Specification<T> spec, Sort sort) {
		Query query = new Query();
		Criteria criteria = spec.toCriteria(entityInformation, query, mongoOperations);
		if(criteria != null){
			query.addCriteria(criteria);
		}
		return query.with(sort);
	}
	private Query getQuery(Specification<T> spec, Pageable pageable) {
		Query query = new Query();
		Criteria criteria = spec.toCriteria(entityInformation, query, mongoOperations);
		if(criteria != null){
			query.addCriteria(criteria);
		}
		Query q =  query.with(pageable);
		return q;
	}

	//////////////////////////////////
	// NativeQueryExecutor
	//////////////////////////////////
	public List<?> executeQuery(String statement) {
		return null;
	}
	public List<?> executeQuery(String statement, Object param) {
		return null;
	}
	public int executeUpdate(String statement) {
		return 0;
	}
	public int executeUpdate(String statement, Object param) {
		return 0;
	}

	public String[] id() {
		return null;
	}




*/