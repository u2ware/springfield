package com.u2ware.springfield.sample.part1.step1;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.service.EntityServiceImpl;


@Service("mybatisBeanService")
public class MybatisBeanTransactional extends EntityServiceImpl<MybatisBean, MybatisBean>{

	@Autowired @Qualifier("mybatisBeanRepository")
	private EntityRepository<MybatisBean, ?> repository;

	@Override
	protected EntityRepository<MybatisBean, ?> getRepository() {
		return repository;
	}
	
	
	@Override
	@Transactional
	public MybatisBean delete(MybatisBean entity) {

		getRepository().create(new MybatisBean("id1" , "pwd"+1, "name"+1, 1));
		getRepository().create(new MybatisBean("id1" , "pwd"+1, "name"+1, 1));
		
		return entity;
	}
	
}
