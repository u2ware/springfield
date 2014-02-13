package com.u2ware.springfield.sample.part3.step3;


import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import com.u2ware.springfield.domain.EntityPage;
import com.u2ware.springfield.domain.EntityPageRequest;
import com.u2ware.springfield.sample.ApplicationContextTestRoot;
import com.u2ware.springfield.service.EntityService;

public class FormBeanServiceTest extends ApplicationContextTestRoot{

	@Autowired @Qualifier("formBeanService")
	private EntityService<FormBean,FormBean> formService;
	
	@Test
	public void testCreate() throws Exception{

		FormBean entity = new FormBean();
		entity.setId("12");
		entity.setAge(12);

		Object newEntity = formService.create(entity);
		logger.debug(newEntity.toString());
	}
	
	@Test
	public void testFind() throws Exception{

		EntityPageRequest pageable = new EntityPageRequest();
		pageable.addSortOrder("age", 1);
		
		FormBean request = new FormBean();
		
		EntityPage<?> entityPage = (EntityPage<?>)formService.find(request, pageable);
		logger.debug(""+entityPage.getTotalElements());
		logger.debug(""+entityPage.getTotalPages());
		logger.debug(""+entityPage.getContent().size());
		logger.debug(""+entityPage.getContent());
	}
	
}
