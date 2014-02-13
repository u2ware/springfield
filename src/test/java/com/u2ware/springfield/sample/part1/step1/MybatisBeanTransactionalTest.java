package com.u2ware.springfield.sample.part1.step1;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import com.u2ware.springfield.sample.ApplicationContextTestRoot;
import com.u2ware.springfield.service.EntityService;

public class MybatisBeanTransactionalTest extends ApplicationContextTestRoot{

	@Autowired @Qualifier("mybatisBeanService")
	private EntityService<MybatisBean,MybatisBean> mybatisBeanService;

	@Test
	public void testTx() throws Exception{
		try{
			mybatisBeanService.delete(null);
		}catch(Exception e){
			e.printStackTrace();
		}
	}

}
