package com.u2ware.springfield.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;


public interface GenericService<D> {

	public Class<D> getDomainClass();
	
	public Page<D> find(String queryMethodName, D queryMethodArgs, Pageable pageable) throws Exception;
	
	public D create(D domain) throws Exception;
	
	public D read(D domain) throws Exception;
	
	public D update(D domain) throws Exception;
	
	public D delete(D domain) throws Exception;
}