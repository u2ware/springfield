package example.u2ware.springfield.part1.step2;

import java.util.List;

import junit.framework.Assert;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Page;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.domain.EntityPageRequest;
import com.u2ware.springfield.repository.EntityRepository;

import example.u2ware.springfield.part1.FindByIdAndPasswordAndNameAndAddressOrderByNameDesc;
import example.u2ware.springfield.part1.MyQuery;

@RunWith(SpringJUnit4ClassRunner.class)
@WebAppConfiguration
@ContextConfiguration(locations="../../application-context.xml")
public class JpaBeanRepositoryTest {

	protected final Log logger = LogFactory.getLog(getClass());

	@Autowired @Qualifier("jpaBeanRepository")
	private EntityRepository<JpaBean,Integer> jpaBeanRepository;

	@Before
	@Transactional
	public void before() throws Exception{
		for(int i = 1 ; i < 10 ; i++){
			if(! jpaBeanRepository.exists(new JpaBean(i), false)){
				jpaBeanRepository.create(new JpaBean("pwd"+i, "name"+i, "addr-"+(10-i)));
			}
		}
	}
	
	@Test
	@Transactional
	public void testWhere() throws Exception{
		
		EntityPageRequest pageable = new EntityPageRequest();
		pageable.addSortOrder("address" , 1);
		
		JpaBean query = new JpaBean();
		//param.setId(7);
		
		long count = jpaBeanRepository.count(query);
		logger.debug(count);
		
		Page<JpaBean> page = jpaBeanRepository.findAll(query, pageable);
		logger.debug(page.getContent().size());
		
		Assert.assertEquals(9 , page.getContent().size());
		Assert.assertEquals(new Integer(9), page.getContent().get(0).getId());
	}
	
	///////////////////////////////////////////
	//
	//////////////////////////////////////////
	@Test
	@Transactional
	public void testWhereByQueryObject1() throws Exception{
		FindByIdAndPasswordAndNameAndAddressOrderByNameDesc query = new FindByIdAndPasswordAndNameAndAddressOrderByNameDesc();
		query.setPassword("pwd7");
		
		List<?> result = jpaBeanRepository.findAll(query);
		Assert.assertEquals(1, result.size());
	}
	
	@Test
	@Transactional
	public void testWhereByQueryObject2() throws Exception{
		FindByIdAndPasswordAndNameAndAddressOrderByNameDesc query = new FindByIdAndPasswordAndNameAndAddressOrderByNameDesc();
		
		List<?> result = jpaBeanRepository.findAll(query);
		Assert.assertEquals(9, result.size());
	}

	@Test
	@Transactional
	public void testWhereByQueryObject3() throws Exception{
		MyQuery query = new MyQuery();
		query.setAddress("addr-9");
		
		List<?> result = jpaBeanRepository.findAll(query);
		Assert.assertEquals(1, result.size());
	}
	
	@Test
	@Transactional
	public void testWhereByQueryObject4() throws Exception{
		MyQuery query = new MyQuery();
		
		List<?> result = jpaBeanRepository.findAll(query);
		Assert.assertEquals(9, result.size());
	}
}
