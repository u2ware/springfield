package com.u2ware.springfield.repository.hibernate.test1;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.AbstractContextTestRoot;
import com.u2ware.springfield.domain.PaginationRequest;
import com.u2ware.springfield.repository.EntityRepository;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class SpringfieldPaginationTest extends AbstractContextTestRoot{

	@Autowired
	@Qualifier("springfieldPaginationRepository")
	private EntityRepository<SpringfieldPagination, String> springfieldPaginationRepository;
	
	
	@Test
	@Transactional
	public void testQuery() throws Exception {
	
		springfieldPaginationRepository.save(new SpringfieldPagination("a", 1));
		springfieldPaginationRepository.save(new SpringfieldPagination("b", 2));
		springfieldPaginationRepository.save(new SpringfieldPagination("c", 3));
		
		Pageable pageable = null;
		Page<SpringfieldPagination> page = null;
		
		
		
		pageable = new PaginationRequest(1, 1);
		page = springfieldPaginationRepository.findAll(pageable);
		for(SpringfieldPagination p : page){
			logger.warn(""+p);
		}
		Assert.assertEquals(1 , page.getContent().size());
		Assert.assertEquals("b", page.getContent().get(0).getName());
		
		
		
		pageable = new PaginationRequest(0, 2, "name,desc");
		page = springfieldPaginationRepository.findAll(pageable);
		for(SpringfieldPagination p : page){
			logger.warn(""+p);
		}
		Assert.assertEquals(2 , page.getContent().size());
		Assert.assertEquals("c", page.getContent().get(0).getName());
	
		
		
		pageable = new PageRequest(1, 1, new Sort(Direction.ASC, "age"));
		page = springfieldPaginationRepository.findAll(pageable);
		for(SpringfieldPagination p : page){
			logger.warn(""+p);
		}
		Assert.assertEquals(1 , page.getContent().size());
		Assert.assertEquals("b", page.getContent().get(0).getName());
		
	}
	
	
	
}
