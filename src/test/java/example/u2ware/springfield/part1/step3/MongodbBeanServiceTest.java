package example.u2ware.springfield.part1.step3;


import junit.framework.Assert;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;

import com.u2ware.springfield.domain.EntityPage;
import com.u2ware.springfield.domain.EntityPageRequest;
import com.u2ware.springfield.service.EntityService;


@RunWith(SpringJUnit4ClassRunner.class)
@WebAppConfiguration
@ContextConfiguration(locations="../../application-context.xml")
public class MongodbBeanServiceTest {

	protected final Log logger = LogFactory.getLog(getClass());

	@Autowired
	private MongoOperations mongoOperations;
	
	@Before
	public void before() throws Exception{
		for(int i = 1 ; i < 10 ; i++){
			if( mongodbBeanService.read(new MongodbBean(i)) == null){
				mongodbBeanService.create(new MongodbBean(i, "pwd"+i, "name"+i, "addr-"+(10-i)));
			}
		}
	}
	@After
	public void afterMongoOperations() throws Exception {
		mongoOperations.dropCollection(MongodbBean.class);
	}
	
	////////////////////////////////////////////////////
	//
	///////////////////////////////////////////////////
	@Autowired @Qualifier("mongodbBeanService")
	private EntityService<MongodbBean,MongodbBean> mongodbBeanService;

	
	@Test
	public void testOrdring() throws Exception{

		EntityPageRequest pageable = new EntityPageRequest();
		pageable.addSortOrder("address", 1);
		
		MongodbBean query = new MongodbBean();
		
		EntityPage<MongodbBean> entityPage = (EntityPage<MongodbBean>)mongodbBeanService.find(query, pageable);
		logger.debug(entityPage.getTotalElements());
		logger.debug(entityPage.getTotalPages());
		logger.debug(entityPage.getContent().size());
		logger.debug(entityPage.getContent());
		
		Assert.assertEquals(9 , entityPage.getTotalElements());
		Assert.assertEquals(new Integer(9), entityPage.getContent().get(0).getId());
	
	}
	
	
	
	


	
}
