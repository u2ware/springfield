package com.u2ware.springfield.repository.hibernate.test3;

import java.util.List;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Restrictions;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

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
				new TemplateCallback<List<SpringfieldQueryTemplate>, Session>() {

					@Override @SuppressWarnings("unchecked")
					public List<SpringfieldQueryTemplate> doInTemplate(Session session) {
		
						//Using Hibernate Criteria
						Criteria criteria = session.createCriteria(SpringfieldQueryTemplate.class);
						criteria.add(Restrictions.eq("name", "a"));
						criteria.add(Restrictions.eq("age", 1));
						return criteria.list();
					}
			});
		Assert.assertEquals(result.size(), 1);
	}
}
