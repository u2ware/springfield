package com.u2ware.springfield.service;


import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.repository.GenericRepository;

@Transactional
public class GenericServiceImpl<D, T, ID extends Serializable> implements GenericService<D> {
	
	protected final Log logger = LogFactory.getLog(getClass());

	@Autowired(required=false) 
	protected ModelMapper modelMapper;
	
	protected Class<D> domainClass;
	protected GenericRepository<T, ID> repository;

	public void setDomainClass(Class<D> domainClass) {
		this.domainClass = domainClass;
	}
	public void setRepository(GenericRepository<T, ID> repository) {
		this.repository = repository;
	}
	public void setModelMapper(ModelMapper modelMapper) {
		this.modelMapper = modelMapper;
	}
	
	public Class<D> getDomainClass() {
		return domainClass;
	}
	@Override
	public Page<D> find(String queryMethodName, D queryMethodArgs, Pageable pageable) throws Exception{
		
		if(repository == null) return new PageImpl<D>(null);
		
		if(pageable == null){
			pageable = new PageRequest(0, 20);
		}
		logger.debug("request dto: "+queryMethodArgs);
		logger.debug("request query: "+queryMethodName);
		logger.debug("request pageable: "+pageable);
		
		List<D> content = new ArrayList<D>(); 
		Page<T> page = null;
		if(queryMethodName == null || queryMethodArgs == null){
			page = repository.findAll(pageable);
		}else{
			page = repository.findAll("findBy"+queryMethodName, queryMethodArgs, pageable);
		}
		for(T entity : page.getContent()){
			content.add(modelMapper.map(entity, domainClass));
		}
		logger.debug("response entities: "+page.getTotalElements()+" row(s)");
		return new PageImpl<D>(content, pageable, page.getTotalElements());
	}

	@Override
	public D create(D domain) throws Exception {		
		if(repository == null) return domain;
		logger.debug("request dto: "+domain.hashCode()+" "+domain);
		
		T entity = modelMapper.map(domain, repository.getEntityClass());
		logger.debug("request entity: "+entity.hashCode()+" "+entity);

		ID id = repository.getIdValue(entity);
		if(id != null && repository.exists(id)){
			String errorCode = "com.u2ware.springfield.validation.DuplicateKey.message";
			logger.debug("response error: "+errorCode);
			throw new ValidationException(errorCode);
		}
		T result = repository.save(entity);
		logger.debug("response entity: "+result.hashCode()+" "+result);

		modelMapper.map(result, domain);
		logger.debug("response dto: "+domain.hashCode()+" "+domain);
		return domain;
	}
	
	
	@Override
	public D read(D domain) throws Exception {
		if(repository == null) return domain;
		logger.debug("request dto: "+domain.hashCode()+" "+domain);

		T entity = modelMapper.map(domain, repository.getEntityClass());
		logger.debug("request entity: "+entity.hashCode()+" "+entity);

		ID id = repository.getIdValue(entity);
		if(id == null || ! repository.exists(id)){
			String errorCode = "com.u2ware.springfield.validation.NoExists.message";
			logger.debug("response error: "+errorCode);
			throw new ValidationException(errorCode);
		}
		T result = repository.findOne(id);
		logger.debug("response entity: "+result.hashCode()+" "+result);
		
		modelMapper.map(result, domain);
		logger.debug("response dto: "+domain.hashCode()+" "+domain);
		return domain;
	}
	
	
	@Override
	public D update(D domain) throws Exception {
		if(repository == null) return domain;
		logger.debug("request dto: "+domain.hashCode()+" "+domain);
		
		T entity = modelMapper.map(domain, repository.getEntityClass());
		logger.debug("request entity: "+entity.hashCode()+" "+entity);
		
		ID id = repository.getIdValue(entity);
		if(id == null || ! repository.exists(id)){
			String errorCode = "com.u2ware.springfield.validation.NoExists.message";
			logger.debug("response error: "+errorCode);
			throw new ValidationException(errorCode);
		}
		
		T result = repository.save(entity);
		logger.debug("response entity: "+result.hashCode()+" "+result);

		modelMapper.map(result, domain);
		logger.debug("response dto: "+domain.hashCode()+" "+domain);
		return domain;
	}
	
	@Override
	public D delete(D domain) throws Exception {
		if(repository == null) return domain;
		logger.debug("request dto: "+domain.hashCode()+" "+domain);
		
		T entity = modelMapper.map(domain, repository.getEntityClass());
		logger.debug("request entity: "+entity.hashCode()+" "+entity);
		
		ID id = repository.getIdValue(entity);
		if(id == null || ! repository.exists(id)){
			String errorCode = "com.u2ware.springfield.validation.NoExists.message";
			logger.debug("response error: "+errorCode);
			throw new ValidationException(errorCode);
		}
		repository.delete(id);
		
		logger.debug("response dto: "+domain.hashCode()+" "+domain);
		return domain;
	}
	
	
}