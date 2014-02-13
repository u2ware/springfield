package com.u2ware.springfield.service.test1;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.service.EntityService;


@Service("extendsBeanService")
public class ExtendsBeanService3 implements EntityService<ExtendsBean, ExtendsBean>{

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	@Autowired
	@Qualifier("extendsBeanRepository")
	private EntityRepository<ExtendsBean, String> extendsBeanRepository;
	
	
	@Override
	@Transactional
	public ExtendsBean create(ExtendsBean entity) {
		logger.debug("ExtendsBeanService3");
		logger.debug("ExtendsBeanService3");
		logger.debug("ExtendsBeanService3");
		return extendsBeanRepository.save(entity);
	}
	@Override
	public ExtendsBean read(ExtendsBean entity) {
		return extendsBeanRepository.findOne(entity);
	}

	
	@Override
	public Iterable<?> find(ExtendsBean query, Pageable pageable) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ExtendsBean createForm(ExtendsBean entity) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ExtendsBean updateForm(ExtendsBean entity) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ExtendsBean update(ExtendsBean entity) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ExtendsBean delete(ExtendsBean entity) {
		// TODO Auto-generated method stub
		return null;
	}


	
	
	
}
