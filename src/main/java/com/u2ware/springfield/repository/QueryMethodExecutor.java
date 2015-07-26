package com.u2ware.springfield.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface QueryMethodExecutor<T> {

	Page<T> findAll(Object queryMethodArgsAndName, Pageable pageable);
	Page<T> findAll(String queryMethodName, Object queryMethodArgs, Pageable pageable);
	
}
