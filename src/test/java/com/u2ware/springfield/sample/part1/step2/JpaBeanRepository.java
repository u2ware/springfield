package com.u2ware.springfield.sample.part1.step2;

import java.util.List;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Page;
import org.springframework.test.context.transaction.TransactionConfiguration;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.domain.EntityPageRequest;
import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.sample.ApplicationContextTestRoot;
import com.u2ware.springfield.sample.part1.FindByIdAndPasswordOrderByNameDesc;
import com.u2ware.springfield.sample.part1.MyQuery;

@TransactionConfiguration(transactionManager="transactionManager",defaultRollback=true)
@Transactional
public class JpaBeanRepository extends ApplicationContextTestRoot{

	@Autowired @Qualifier("jpaBeanRepository")
	private EntityRepository<JpaBean,String> jpaBeanRepository;

	@Before
	public void init() throws Exception{
		for(int i = 1 ; i < 10 ; i++){
			jpaBeanRepository.createOrUpdate(new JpaBean("id"+i , "pwd"+i, "name"+i, i));
		}
	}
	
	
	@Test
	public void testFind() throws Exception{
		
		EntityPageRequest pageable = new EntityPageRequest();
		pageable.addSortOrder("age" , 1);
		
		JpaBean query = new JpaBean();
		//param.setId(7);
		
		long count = jpaBeanRepository.count(query);
		logger.debug(count);
		
		Page<JpaBean> page = jpaBeanRepository.findAll(query, pageable);
		logger.debug(page.getContent().size());
		
		Assert.assertEquals(9 , page.getContent().size());
		Assert.assertEquals("id1", page.getContent().get(0).getId());
	}
	
	///////////////////////////////////////////
	//
	//////////////////////////////////////////
	@Test
	public void testFindByEntityQueryObject1() throws Exception{
		
		FindByIdAndPasswordOrderByNameDesc query 
			= new FindByIdAndPasswordOrderByNameDesc();
		query.setPassword("pwd7");
		
		List<?> result = jpaBeanRepository.findAll(query);
		Assert.assertEquals(1, result.size());
	}
	
	@Test
	public void testFindByEntityQueryObject2() throws Exception{
		
		MyQuery query = new MyQuery();
		query.setName("name5");
		
		List<?> result = jpaBeanRepository.findAll(query);
		Assert.assertEquals(1, result.size());
	}
	
	@Test
	public void testDeleteAll() throws Exception{
		FindByIdAndPasswordOrderByNameDesc query2 
		= new FindByIdAndPasswordOrderByNameDesc();
		query2.setPassword("pwd7");
		jpaBeanRepository.deleteAll(query2);

		MyQuery query3 = new MyQuery();
		query3.setName("name5");
		jpaBeanRepository.deleteAll(query3);
		
		jpaBeanRepository.deleteAll();
	}
}
