package com.u2ware.springfield.sample.part3.step2;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.domain.EntityPageable;
import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.sample.part3.step1.TargetBean;
import com.u2ware.springfield.service.EntityServiceImpl;


@Service
public class CustomBeanService extends EntityServiceImpl<TargetBean, CustomBean>{

	@Autowired @Qualifier("targetBeanRepository")
	private EntityRepository<TargetBean, String> targetBeanRepository;

	@Override
	protected EntityRepository<TargetBean, ?> getRepository() {
		return targetBeanRepository;
	}
	
	@Override
	@Transactional
	public Iterable<?> findForm(CustomBean request, EntityPageable pageable) {
		logger.debug("Overide findForm ");
		return super.findForm(request, pageable);
	}
}