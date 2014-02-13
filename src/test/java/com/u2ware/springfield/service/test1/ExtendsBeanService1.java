package com.u2ware.springfield.service.test1;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionTemplate;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.service.AbstractEntityService;


//@Service("extendsBeanService")
public class ExtendsBeanService1 extends AbstractEntityService<ExtendsBean, ExtendsBean>{

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	@Autowired @Qualifier("extendsBeanRepository")
	private EntityRepository<ExtendsBean, String> extendsBeanRepository;
	
	@Autowired 
	private TransactionTemplate transactionTemplate;

	@Override
	protected EntityRepository<ExtendsBean, String> getRepository() {
		return extendsBeanRepository;
	}
	
	@Override
	protected TransactionTemplate getTransactionTemplate() {
		return transactionTemplate;
	}

	//////////////////////////////////////////
	//
	//////////////////////////////////////////
	@Override
	public ExtendsBean create(final ExtendsBean entity) {
		return getTransactionTemplate().execute(new TransactionCallback<ExtendsBean>() {
			public ExtendsBean doInTransaction(TransactionStatus status) {
				
				
				ExtendsBean result = getRepository().save(entity);
				return result;
			}
		});
	}
	
	@Override
	@Transactional
	public ExtendsBean read(ExtendsBean entity) {
		return getRepository().findOne(entity);
	}

}
