package com.u2ware.springfield.service.test2;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.service.EntityService;


@Service("dtoBeanService")
public class DtoBeanService implements EntityService<DtoBean, DtoBean>{

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	@Autowired @Qualifier("customFooRepository")
	private EntityRepository<CustomFoo, String> customFooRepository;

	@Autowired @Qualifier("customBarRepository")
	private EntityRepository<CustomBar, Integer> customBarRepository;
	
	
	@Override
	@Transactional
	public DtoBean create(DtoBean entity) {

		CustomFoo foo = new CustomFoo();
		foo.setName(entity.getParam1());
		foo.setAge(entity.getParam2());
		customFooRepository.save(foo);

		CustomBar bar = new CustomBar();
		bar.setSeq(entity.getParam2());
		bar.setDesc(entity.getParam1());
		customBarRepository.save(bar);
		
		return entity;
	}

	@Override
	public DtoBean read(DtoBean entity) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Iterable<?> find(DtoBean query, Pageable pageable) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public DtoBean createForm(DtoBean entity) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public DtoBean updateForm(DtoBean entity) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public DtoBean update(DtoBean entity) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public DtoBean delete(DtoBean entity) {
		// TODO Auto-generated method stub
		return null;
	}
	

	

}
