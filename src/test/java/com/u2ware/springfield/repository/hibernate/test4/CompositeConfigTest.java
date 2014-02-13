package com.u2ware.springfield.repository.hibernate.test4;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.AbstractContextTestRoot;
import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.repository.hibernate.test1.SpringfieldPagination;
import com.u2ware.springfield.repository.hibernate.test2.SpringfieldQuery;
import com.u2ware.springfield.repository.hibernate.test3.SpringfieldQueryTemplate;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class CompositeConfigTest extends AbstractContextTestRoot {

	@Autowired
	@Qualifier("springfieldPaginationRepository")
	private EntityRepository<SpringfieldPagination, String> springfieldPaginationRepository;

	@Autowired @Qualifier("springfieldQueryRepository")
	private EntityRepository<SpringfieldQuery,String> springfieldQueryRepository;
	
	@Autowired
	@Qualifier("springfieldQueryTemplateRepository")
	private EntityRepository<SpringfieldQueryTemplate, String> springfieldQueryTemplateRepository;

	@Test
	@Transactional
	public void testQuery() throws Exception {

		logger.debug(""+springfieldPaginationRepository.findAll());
		logger.debug(""+springfieldQueryRepository.findAll());
		logger.debug(""+springfieldQueryTemplateRepository.findAll());
		
	}
}
