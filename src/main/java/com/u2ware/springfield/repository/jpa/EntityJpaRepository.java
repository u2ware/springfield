package com.u2ware.springfield.repository.jpa;

import java.io.Serializable;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.support.JpaEntityInformation;
import org.springframework.data.jpa.repository.support.JpaEntityInformationSupport;
import org.springframework.data.jpa.repository.support.SimpleJpaRepository;

import com.u2ware.springfield.repository.EntityRepository;


public class EntityJpaRepository<T, ID extends Serializable> implements EntityRepository<T,ID> {
	
	protected final Log logger = LogFactory.getLog(getClass());

	private final Class<T> entityClass;
	private EntityManager em;
	private JpaEntityInformation<T, ID> entityInformation;
	private SimpleJpaRepository<T, ID> simpleJpaRepository;

	public EntityJpaRepository(Class<T> entityClass){
		this(entityClass, null);
	}
	public EntityJpaRepository(Class<T> entityClass, EntityManager em){
		if(em != null) this.setTemplate(em);
		this.entityClass = entityClass;
	}
	
	@PersistenceContext 
	@SuppressWarnings("unchecked")
	public void setTemplate(EntityManager em){
		this.em = em;
		this.entityInformation = (JpaEntityInformation<T, ID>)JpaEntityInformationSupport.getMetadata(entityClass, em);
		this.simpleJpaRepository = new SimpleJpaRepository<T,ID>(entityInformation, em);
	}
	
	@Override 
	@SuppressWarnings("unchecked")
	public EntityManager getTemplate() {
		return em;
	}
	

	//////////////////////////////////
	// exists
	//////////////////////////////////
	@Override 
	public boolean exists(ID id) {	
		return simpleJpaRepository.exists(id);
	}
	@Override 
	public boolean exists(T entity) {
		ID id = entityInformation.getId(entity);
		return simpleJpaRepository.exists(id);
	}
	
	//////////////////////////////////
	// crud
	//////////////////////////////////
	@Override 
	public T read(ID id) {		
		if(id == null) return null;
		return simpleJpaRepository.findOne(id);
	}
	@Override 
	public T read(T entity) {		
		ID id = entityInformation.getId(entity);
		return simpleJpaRepository.findOne(id);
	}
	@Override 
	public T create(T entity) {
		getTemplate().persist(entity);
		getTemplate().flush();
		return entity;
	}
	@Override 
	public T update(T entity) {
		T result = getTemplate().merge(entity);
		getTemplate().flush();
		return result;
	}
	@Override 
	public void delete(T entity) {
		simpleJpaRepository.delete(entity);
	}
	@Override
	public void delete(ID id) {
		simpleJpaRepository.delete(id);
	}
	@Override 
	public T createOrUpdate(T entity) {
		return simpleJpaRepository.saveAndFlush(entity);
	}

	
	//////////////////////////////////
	// count
	//////////////////////////////////
	@Override 
	public long count(Object query) {
		PartTreeQuery<T> spec = new PartTreeQuery<T>(entityClass, query);
		return simpleJpaRepository.count(spec);
	}
	//////////////////////////////////
	// find
	//////////////////////////////////
	@Override 
	public List<T> findAll() {
		return simpleJpaRepository.findAll();
	}
	@Override 
	public List<T> findAll(Object query) {
		PartTreeQuery<T> spec = new PartTreeQuery<T>(entityClass, query);
		Sort specSort = spec.createSort();
		//logger.info("sort : "+specSort);
		return simpleJpaRepository.findAll(spec, specSort);
	}
	@Override 
	public List<T> findAll(Object query, Sort sort) {
		PartTreeQuery<T> spec = new PartTreeQuery<T>(entityClass, query);
		Sort specSort = spec.createSort();
		Sort sortAll =  (specSort != null) ? specSort.and(sort) : sort;
		//logger.info("sort : "+sortAll);
		return simpleJpaRepository.findAll(spec, sortAll);
	}
	@Override
	public Page<T> findAll(Object query, Pageable pageable) {
		PartTreeQuery<T> spec = new PartTreeQuery<T>(entityClass, query);
		Sort specSort = spec.createSort();
		Sort sortAll = (specSort != null) ? specSort.and(pageable.getSort()) : pageable.getSort();
		//logger.info("sort : "+sortAll);
		return simpleJpaRepository.findAll(spec, new PageRequest(pageable.getPageNumber(), pageable.getPageSize(), sortAll));
	}
	
	//////////////////////////////////
	// deleteAll
	//////////////////////////////////
	@Override 
	public void deleteAll(){
		simpleJpaRepository.deleteAll();
	}
	
	@Override 
	public void deleteAll(Object query) {
		
		Iterable<T> entities = findAll(query);
		simpleJpaRepository.delete(entities);
	}

}

/*
private T clearOneToManyProperties(T entity) {
	final T target = load(entity);
	
	logger.debug(" clearOneToManyProperties "+entityInformation.getJavaType());
	ReflectionUtils.doWithMethods(
		entityInformation.getJavaType(),
		new MethodCallback(){
			public void doWith(Method method) throws IllegalArgumentException,IllegalAccessException {
				logger.debug(" xxxxxxxxxxxxxxx "+method);
				Collection<?> oneTomany = (Collection<?>)ReflectionUtils.invokeMethod(method, target);
				oneTomany.clear();
			}
		}, 
		new MethodFilter(){
			public boolean matches(Method method) {
				logger.debug(" clearOneToManyProperties "+method.getReturnType());
				return method.isAccessible() &&  (ClassUtils.isAssignable(method.getReturnType(), List.class) || ClassUtils.isAssignable(method.getReturnType(), Set.class));
			}
		}
	);
	em.merge(target);
	return target;
}
private void resetOneToManyProperties(T entity) {
	// TODO Auto-generated method stub
	
}
*/



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
	return simpleJpaRepository.findAll(new PartTreeQuerySpecification<T>(queryMethodName, entityClass, query));
}
public List<T> findAll(String queryMethodName, Object query, Sort sort) {
	return simpleJpaRepository.findAll(new PartTreeQuerySpecification<T>(queryMethodName, entityClass, query), sort);
}
public Page<T> findAll(String queryMethodName, Object query, Pageable pageable) {

	return simpleJpaRepository.findAll(new PartTreeQuerySpecification<T>(queryMethodName, entityClass, query), pageable);
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
// NativeQueryExecutor
//////////////////////////////////
public List<?> executeQuery(String sqlString) {
	return em.createNativeQuery(sqlString).getResultList();
}
public List<?> executeQuery(String sqlString, Object param) {
	return setParameters(em.createNativeQuery(sqlString), param).getResultList();
}
public int executeUpdate(String sqlString) {
	return em.createNativeQuery(sqlString).executeUpdate();
}
public int executeUpdate(String sqlString, Object param) {
	return setParameters(em.createNativeQuery(sqlString), param).executeUpdate();
}
protected Query setParameters(Query query, Object param){
	BeanWrapper paramWrapper = PropertyAccessorFactory.forBeanPropertyAccess(param);
	Set<Parameter<?>> parameters = query.getParameters();
	for(Parameter<?> parameter : parameters){
		String name = parameter.getName();
		Object value = paramWrapper.getPropertyValue(name);		
		query.setParameter(name, value);
	}
	return query;
}

public String[] id() {
	//Springfield f = null;
	return null;
}
*/








