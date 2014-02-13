package com.u2ware.springfield.repository.sqlsession.test2;

import java.util.List;

import junit.framework.Assert;
import lombok.Getter;
import lombok.Setter;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.AbstractContextTestRoot;
import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.repository.QueryMethod;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class SpringfieldQueryTest extends AbstractContextTestRoot{

	@Autowired @Qualifier("springfieldQueryRepository")
	private EntityRepository<SpringfieldQuery,String> springfieldQueryRepository;


	private SpringfieldQuery[] createEntities(){
		SpringfieldQuery[] r = new SpringfieldQuery[9];
		for(int i = 1 ; i < 10 ; i++){
			r[i-1] = new SpringfieldQuery(
					"id"+i , 
					i
					);
		}		
		return r;
	}
	
	
	@Test
	@Transactional
	public void testAll() throws Exception{
		
		springfieldQueryRepository.deleteAll();
		SpringfieldQuery[] entities = createEntities();
		for(SpringfieldQuery e : entities){
			springfieldQueryRepository.save(e);
		}
		
		
		MyQuery queryMethod = new MyQuery();
		queryMethod.setName("id2");
		queryMethod.setAge(2);
		
		
		List<SpringfieldQuery> result = springfieldQueryRepository.findAll(queryMethod);
		Assert.assertEquals(result.size(), 1);
		
	}
	
	@QueryMethod("findByNameAndAge")
	public static class MyQuery{
		private @Getter @Setter String name;
		private @Getter @Setter Integer age;
	}
	
}