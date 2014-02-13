package com.u2ware.springfield.repository.jpa.test3;

import java.util.List;

import javax.persistence.EntityManager;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import com.mysema.query.alias.Alias;
import com.mysema.query.jpa.impl.JPAQuery;
import com.mysema.query.types.EntityPath;
import com.mysema.query.types.path.NumberPath;
import com.mysema.query.types.path.StringPath;
import com.u2ware.springfield.AbstractContextTestRoot;
import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.repository.TemplateCallback;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class SpringfieldQueryTemplateTest extends AbstractContextTestRoot{

	@Autowired
	@Qualifier("springfieldQueryTemplateRepository")
	private EntityRepository<SpringfieldQueryTemplate, String> springfieldQueryTemplateRepository;

	@Test
	@Transactional
	public void testQuery() throws Exception {
	
		springfieldQueryTemplateRepository.save(new SpringfieldQueryTemplate("a", 1));
		springfieldQueryTemplateRepository.save(new SpringfieldQueryTemplate("b", 2));
		springfieldQueryTemplateRepository.save(new SpringfieldQueryTemplate("c", 3));
		
		List<SpringfieldQueryTemplate> result = springfieldQueryTemplateRepository.execute(
			new TemplateCallback<List<SpringfieldQueryTemplate>, EntityManager>() {
				public List<SpringfieldQueryTemplate> doInTemplate(EntityManager em) {
	
					//Using QueryDsl
					SpringfieldQueryTemplate alias = Alias.alias(SpringfieldQueryTemplate.class, "foo");
					EntityPath<SpringfieldQueryTemplate> foo = Alias.$(alias);
					StringPath fooName = Alias.$(alias.getName());
					NumberPath<Integer> fooAge = Alias.$(alias.getAge());
					
					JPAQuery query = new JPAQuery(em);
					query.from(foo);
					query.where(fooName.eq("a"));
					query.where(fooAge.eq(1));
	
					return query.list(foo);
				}
		});
		Assert.assertEquals(result.size(), 1);
	}
}
