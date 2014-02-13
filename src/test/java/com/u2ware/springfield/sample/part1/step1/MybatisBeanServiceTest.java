package com.u2ware.springfield.sample.part1.step1;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import com.u2ware.springfield.domain.EntityPage;
import com.u2ware.springfield.domain.EntityPageRequest;
import com.u2ware.springfield.sample.ApplicationContextTestRoot;
import com.u2ware.springfield.service.EntityService;

public class MybatisBeanServiceTest extends ApplicationContextTestRoot{

	@Autowired @Qualifier("mybatisBeanService")
	private EntityService<MybatisBean,MybatisBean> mybatisBeanService;

	@Before
	public void before() throws Exception{
		for(int i = 1 ; i < 10 ; i++){
			if(mybatisBeanService.read(new MybatisBean("id"+i)) == null){
				mybatisBeanService.create(new MybatisBean("id"+i, "pwd"+i, "name"+i, i));
			}
		}
	}
	
	@Test
	@SuppressWarnings("unchecked")
	public void testFind() throws Exception{

		EntityPageRequest pageable = new EntityPageRequest();
		pageable.addSortOrder("age", 1);
		
		MybatisBean query = new MybatisBean();

		EntityPage<MybatisBean> entityPage = (EntityPage<MybatisBean>)mybatisBeanService.find(query, pageable);
		logger.debug(""+entityPage.getTotalElements());
		logger.debug(""+entityPage.getTotalPages());
		logger.debug(""+entityPage.getContent().size());
		logger.debug(""+entityPage.getContent());
		
		Assert.assertEquals(9 , entityPage.getTotalElements());
		Assert.assertEquals("id1", entityPage.getContent().get(0).getId());
	}
	
}
