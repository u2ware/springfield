package example.u2ware.springfield;


/*
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;
*/
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.web.context.WebApplicationContext;



@RunWith(SpringJUnit4ClassRunner.class)
@WebAppConfiguration
@ContextConfiguration(locations="application-context.xml")
public class ApplicationContextTest {

	protected final Log logger = LogFactory.getLog(getClass());
	
	@Autowired
	protected WebApplicationContext applicationContext;
	
	@Test
	public void print() throws Exception{
		logger.info("======================================================================ApplicationContext");
		if(applicationContext != null){
			for(String name : applicationContext.getBeanDefinitionNames()){
				//logger.info(name+"="+applicationContext.getType(name));
			}
		}
		logger.info("======================================================================ApplicationContext");
		
	}
	
}
