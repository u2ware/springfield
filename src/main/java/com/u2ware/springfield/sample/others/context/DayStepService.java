package com.u2ware.springfield.sample.others.context;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.service.EntityServiceImpl;
import com.u2ware.springfield.support.context.ContextBroker;

@Service
public class DayStepService extends EntityServiceImpl<DayStep, DayStep>{

	@Autowired @Qualifier("sessionContextBroker")
	private ContextBroker sessionContextBroker;	
	
	@Autowired @Qualifier("dayStepRepository")
	private EntityRepository<DayStep, String> dayStepRepository;
	
	@Override
	protected EntityRepository<DayStep, String> getRepository() {
		return dayStepRepository;
	}
	
	//////////////////////////////////
	//
	//////////////////////////////////
	@Override
	@Transactional
	public DayStep createForm(DayStep entity) {
		DayStep saved = sessionContextBroker.get(DayStep.class, false);
		if(saved != null){
			entity.setName(saved.getName());
			entity.setStep(saved.getStep().plusDays(1));
		}
		return entity;
	}

	@Override
	@Transactional
	public DayStep create(DayStep entity) {
		DayStep newEntity = getRepository().create(entity);
		sessionContextBroker.put(newEntity);
		return newEntity;
	}
}