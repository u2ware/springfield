package com.u2ware.springfield.sample.part3.step1;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.service.EntityServiceImpl;
import com.u2ware.springfield.validation.RejectableException;

@Service
public class TargetBeanService extends EntityServiceImpl<TargetBean, TargetBean> {

	@Autowired @Qualifier("targetBeanRepository")
	private EntityRepository<TargetBean, String> targetBeanRepository;

	@Override
	protected EntityRepository<TargetBean, String> getRepository() {
		return targetBeanRepository;
	}
	
	@Override
	@Transactional
	public TargetBean create(TargetBean entity) {
		if(getRepository().exists(entity)){
			throw new RejectableException("id" , "Duplication", "중복키입니다.");
		}
		return getRepository().create(entity);
	}
	
}