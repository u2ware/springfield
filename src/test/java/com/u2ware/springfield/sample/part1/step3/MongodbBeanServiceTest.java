package com.u2ware.springfield.sample.part1.step3;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import com.u2ware.springfield.domain.EntityPage;
import com.u2ware.springfield.domain.EntityPageRequest;
import com.u2ware.springfield.sample.ApplicationContextTestRoot;
import com.u2ware.springfield.service.EntityService;

public class MongodbBeanServiceTest extends ApplicationContextTestRoot{

	@Autowired @Qualifier("mongodbBeanService")
	private EntityService<MongodbBean,MongodbBean> mongodbBeanService;

	@Before
	public void before() throws Exception{
		for(int i = 1 ; i < 10 ; i++){
			if( mongodbBeanService.read(new MongodbBean("id"+i)) == null){
				mongodbBeanService.create(new MongodbBean("id"+i, "pwd"+i, "name"+i, i));
			}
		}
	}

	@Test
	@SuppressWarnings("unchecked")
	public void testFind() throws Exception{

		EntityPageRequest pageable = new EntityPageRequest();
		pageable.addSortOrder("age", 1);
		
		MongodbBean query = new MongodbBean();
		
		EntityPage<MongodbBean> entityPage = (EntityPage<MongodbBean>)mongodbBeanService.find(query, pageable);
		logger.debug(entityPage.getTotalElements());
		logger.debug(entityPage.getTotalPages());
		logger.debug(entityPage.getContent().size());
		logger.debug(entityPage.getContent());
		
		Assert.assertEquals(9 , entityPage.getTotalElements());
		Assert.assertEquals("id1", entityPage.getContent().get(0).getId());
	
	}
	
	
	
	


	
}
