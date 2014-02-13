package com.u2ware.springfield.repository.sqlsession.test3;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mybatis.spring.SqlSessionTemplate;
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
				new TemplateCallback<List<SpringfieldQueryTemplate>, SqlSessionTemplate>() {
					public List<SpringfieldQueryTemplate> doInTemplate(SqlSessionTemplate template) {
		
						SpringfieldQueryTemplate query = new SpringfieldQueryTemplate("a", 1);
						
						Map<String,Object> parameter = new HashMap<String,Object>();
						parameter.put("query", query);
						
						String statement = SpringfieldQueryTemplate.class.getName()+".customQuery";
						return template.selectList(statement, parameter);
					}
			});

		Assert.assertEquals(result.size(), 1);
	}
}
