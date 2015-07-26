package com.u2ware.springfield.repository;

import java.io.Serializable;

import javax.persistence.EntityManager;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.support.JpaEntityInformation;
import org.springframework.data.jpa.repository.support.JpaEntityInformationSupport;
import org.springframework.data.jpa.repository.support.QueryDslJpaRepository;
import org.springframework.transaction.annotation.Transactional;

@Transactional
public class GenericRepositoryImpl<T, ID extends Serializable> 
extends QueryDslJpaRepository<T, ID>
implements GenericRepository<T, ID>, QueryMethodExecutor<T> {

	protected Log logger = LogFactory.getLog(getClass());

    protected EntityManager entityManager;
    protected JpaEntityInformation<T,ID> entityInformation;


    @SuppressWarnings("unchecked")
	public GenericRepositoryImpl(Class<T> entityClass,
			EntityManager entityManager) {
		this((JpaEntityInformation<T, ID>)JpaEntityInformationSupport.getMetadata(entityClass, entityManager), entityManager);

    }
    
    private GenericRepositoryImpl(JpaEntityInformation<T, ID> entityInformation,
			EntityManager entityManager) {
		super(entityInformation, entityManager, QueryMethodUtil.ENTITY_PATH_RESOLVER);
		this.entityManager = entityManager;
		this.entityInformation = entityInformation;
	}
    
	public ID getIdValue(T entity) {
		return entityInformation.getId(entity);
	}
	public Class<T> getEntityClass() {
		return entityInformation.getJavaType();
	}

	///////////////////////
	//QueryMethodExecutor
	///////////////////////
	@Override
	public Page<T> findAll(Object queryMethodArgsAndName, Pageable pageable) {
		QueryMethodPredicate q = new QueryMethodPredicate(
				entityInformation.getJavaType(), 
				queryMethodArgsAndName,
				QueryMethodUtil.methodName(queryMethodArgsAndName));
		return super.findAll(q.predicate(), pageable);
	}

	@Override
	public Page<T> findAll(String queryMethodName, Object queryMethodArgs, Pageable pageable) {
		QueryMethodPredicate q = new QueryMethodPredicate(
				entityInformation.getJavaType(), 
				queryMethodArgs, 
				queryMethodName);
		return super.findAll(q.predicate(), pageable);
	}

}