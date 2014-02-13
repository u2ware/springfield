package com.u2ware.springfield.sample.part1.step1;

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
public class MybatisBeanRepositoryTest extends ApplicationContextTestRoot{

	@Autowired @Qualifier("mybatisBeanRepository")
	private EntityRepository<MybatisBean,String> mybatisBeanRepository;
	
	@Before
	public void init() throws Exception{
		for(int i = 1 ; i < 10 ; i++){
			mybatisBeanRepository.createOrUpdate(new MybatisBean("id"+i , "pwd"+i, "name"+i, i));
		}
	}

	@Test
	public void testFind() throws Exception{

		
		EntityPageRequest pageable = new EntityPageRequest();
		pageable.addSortOrder("age" , 1);

		MybatisBean query = new MybatisBean();
		//param.setId(7);
	
		long count = mybatisBeanRepository.count(query);
		logger.debug(""+count);
				
		Page<MybatisBean> page = mybatisBeanRepository.findAll(query, pageable);
		logger.debug(""+page.getContent().size());
		
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
		
		List<?> result = mybatisBeanRepository.findAll(query);
		Assert.assertEquals(1, result.size());
	}
	
	@Test
	public void testFindByEntityQueryObject2() throws Exception{
		
		MyQuery query = new MyQuery();
		query.setName("name5");
		
		List<?> result = mybatisBeanRepository.findAll(query);
		Assert.assertEquals(1, result.size());
	}
	
	
	@Test
	public void testDeleteAll() throws Exception{

		MybatisBean query1 = new MybatisBean();
		query1.setName("name5");
		mybatisBeanRepository.deleteAll(query1);
		
		FindByIdAndPasswordOrderByNameDesc query2 
		= new FindByIdAndPasswordOrderByNameDesc();
		query2.setPassword("pwd7");
		mybatisBeanRepository.deleteAll(query2);

		MyQuery query3 = new MyQuery();
		query3.setName("name5");
		mybatisBeanRepository.deleteAll(query3);
		
		mybatisBeanRepository.deleteAll();
	}
}
