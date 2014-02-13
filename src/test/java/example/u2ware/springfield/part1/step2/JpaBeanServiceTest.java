package example.u2ware.springfield.part1.step2;


import junit.framework.Assert;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;

import com.u2ware.springfield.domain.EntityPage;
import com.u2ware.springfield.domain.EntityPageRequest;
import com.u2ware.springfield.service.EntityService;


@RunWith(SpringJUnit4ClassRunner.class)
@WebAppConfiguration
@ContextConfiguration(locations="../../application-context.xml")
public class JpaBeanServiceTest {

	protected final Log logger = LogFactory.getLog(getClass());

	@Autowired @Qualifier("jpaBeanService")
	private EntityService<JpaBean,JpaBean> jpaBeanService;
	
	
	@Before
	public void before() throws Exception{
		for(int i = 1 ; i < 10 ; i++){
			if(jpaBeanService.read(new JpaBean(i)) == null){
				jpaBeanService.create(new JpaBean("pwd"+i, "name"+i, "addr-"+(10-i)));
			}
		}
	}

	@Test
	public void testOrdring() throws Exception{
		
		EntityPageRequest pageable = new EntityPageRequest();
		pageable.addSortOrder("address", 1);
		
		JpaBean query = new JpaBean();
		
		EntityPage<JpaBean> entityPage = (EntityPage<JpaBean>)jpaBeanService.find(query, pageable);
		logger.debug(entityPage.getTotalElements());
		logger.debug(entityPage.getTotalPages());
		logger.debug(entityPage.getContent().size());
		logger.debug(entityPage.getContent());
		
		Assert.assertEquals(9 , entityPage.getTotalElements());
		Assert.assertEquals(new Integer(9), entityPage.getContent().get(0).getId());
	
	}
	
}
