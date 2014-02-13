package example.u2ware.springfield.part3.step3;


import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
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
public class FormServiceTest {

	protected final Log logger = LogFactory.getLog(getClass());


	@Autowired @Qualifier("formService")
	private EntityService<Form,Form> formService;
	

	@Test
	public void testCreate() throws Exception{

		Form entity = new Form();
		entity.setPassword("password");
		entity.setName("name");
		entity.setAddress("address");

		Form newEntity = formService.create(entity);
		logger.debug(newEntity);
		
		
	}
	
	@Test
	public void testList() throws Exception{

		EntityPageRequest pageable = new EntityPageRequest();
		pageable.addSortOrder("address", 1);
		
		Form request = new Form();
		
		EntityPage<Form> entityPage = (EntityPage<Form>)formService.findForm(request, pageable);
		logger.debug(entityPage.getTotalElements());
		logger.debug(entityPage.getTotalPages());
		logger.debug(entityPage.getContent().size());
		logger.debug(entityPage.getContent());
		
		
	}
	
}
