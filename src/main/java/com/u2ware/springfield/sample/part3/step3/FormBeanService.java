package com.u2ware.springfield.sample.part3.step3;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.domain.EntityPageImpl;
import com.u2ware.springfield.domain.EntityPageable;
import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.sample.part3.step1.TargetBean;
import com.u2ware.springfield.service.EntityService;

@Service
public class FormBeanService implements EntityService<FormBean, FormBean>{

	protected final Log logger = LogFactory.getLog(getClass());

	@Autowired 
	@Qualifier("targetBeanRepository")
	protected EntityRepository<TargetBean, String> targetBeanRepository;

	@Override
	@Transactional
	public Iterable<?> findForm(FormBean query, EntityPageable pageable) {
		return find(query, pageable);
	}

	@Override
	@Transactional
	public Iterable<?> find(FormBean query, EntityPageable pageable) {
		if(pageable != null && pageable.isEnable()){
			return new EntityPageImpl<TargetBean>(targetBeanRepository.findAll(query, pageable));
		}else{
			return targetBeanRepository.findAll(query, pageable);
		}
	}
	
	@Override
	public Object home(FormBean query) {
		return query;
	}

	@Override
	public FormBean createForm(FormBean entity) {
		return entity;
	}

	@Override
	public FormBean updateForm(FormBean entity) {
		return entity;
	}

	@Override
	@Transactional
	public FormBean read(FormBean entity) {
		TargetBean target = targetBeanRepository.read(entity.getId());
		entity.setId(target.getId());
		entity.setAge(target.getAge());
		return entity;
	}

	
	@Override
	@Transactional
	public FormBean create(FormBean entity) {
		TargetBean target = new TargetBean();
		target.setId(entity.getId());
		target.setPassword("password"+entity.getId());
		target.setName("name"+entity.getId());
		target.setAge(entity.getAge());

		targetBeanRepository.create(target);
		return entity;
	}


	@Override
	@Transactional
	public FormBean update(FormBean entity) {
		TargetBean target = targetBeanRepository.read(entity.getId());
		target.setId(entity.getId());
		target.setAge(entity.getAge());
		return entity;
	}

	@Override
	@Transactional
	public FormBean delete(FormBean entity) {
		targetBeanRepository.delete(entity.getId());
		return entity;
	}

	


	@Override
	@Transactional
	public boolean validate(FormBean entity) {
		return targetBeanRepository.exists(entity.getId());
	}

	@Override
	@Transactional
	public boolean reset(FormBean entity) {
		return true;
	}
}