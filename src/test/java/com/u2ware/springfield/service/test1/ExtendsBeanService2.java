package com.u2ware.springfield.service.test1;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.service.EntityServiceImpl;


//@Service("extendsBeanService")
public class ExtendsBeanService2 extends EntityServiceImpl<ExtendsBean, ExtendsBean>{

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	@Autowired
	@Qualifier("extendsBeanRepository")
	private EntityRepository<ExtendsBean, String> extendsBeanRepository;
	
	@Override
	protected EntityRepository<ExtendsBean, String> getRepository() {
		return extendsBeanRepository;
	}
	
	
	//////////////////////////////////////////
	//
	//////////////////////////////////////////
	@Override
	@Transactional
	public ExtendsBean create(ExtendsBean entity) {
		logger.debug("ExtendsBeanService2");
		logger.debug("ExtendsBeanService2");
		logger.debug("ExtendsBeanService2");
		return getRepository().save(entity);
	}
}
