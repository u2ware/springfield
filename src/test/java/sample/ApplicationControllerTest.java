package sample;

import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.TestRestTemplate;
import org.springframework.boot.test.WebIntegrationTest;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.context.WebApplicationContext;


@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(classes = Application.class)
@WebIntegrationTest
public class ApplicationControllerTest {
    protected Log logger = LogFactory.getLog(getClass());
	
	@Autowired
	private WebApplicationContext applicationContext;

    @Before
    public void setup() throws Exception {
        logger.warn("===================================================");
//        String[] beanNames = applicationContext.getBeanDefinitionNames();
//        Arrays.sort(beanNames, 0, beanNames.length);
//        for(String name : beanNames){
//            logger.warn(name+"="+applicationContext.getBean(name).getClass());
//        }
        logger.warn("===================================================");
    }
	
	@SuppressWarnings("unchecked")
	@Test
    public void testSpringDataRest() throws Exception{
		RestTemplate template = new TestRestTemplate();
		String result = null;
		
        logger.warn("===================================================");
		result = template.getForObject("http://localhost:8080/alps", String.class);
		logger.debug(result);
        logger.warn("===================================================");

		result = template.getForObject("http://localhost:8080/desks", String.class);
		logger.debug(result);
        logger.warn("===================================================");
		
		result = template.getForObject("http://localhost:8080/sample/application/consumer.xml", String.class);
		logger.debug(result);
        logger.warn("===================================================");

		result = template.getForObject("http://localhost:8080/a/b/c.xml", String.class);
		logger.debug(result);
        logger.warn("===================================================");

		result = template.getForObject("http://localhost:8080/sample/application/house.xml", String.class);
		logger.debug(result);
        logger.warn("===================================================");

		result = template.getForObject("http://localhost:8080/sample/application/phonebook.xml", String.class);
		logger.debug(result);

		logger.warn("===================================================");
        MultiValueMap<String, Object> params = new LinkedMultiValueMap<String, Object> ();
        params.add("multipartFile", new FileSystemResource("pom.xml"));

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.MULTIPART_FORM_DATA);
        
        HttpEntity<MultiValueMap<String, Object>> entity = new HttpEntity<MultiValueMap<String, Object>>(params, headers);

        Map<String,Object> upload = template.postForObject("http://localhost:8080/sample/application/consumer.upload", entity, Map.class);
		logger.debug(upload);
        logger.warn("===================================================");
                
        //ByteArrayResource fff = template.getForObject("http://localhost:8080/sample/application/consumer/multipart/"+upload.get("uploadFilename")+".download", ByteArrayResource.class);
		//logger.debug(fff.getByteArray().length);
        String fff = template.getForObject("http://localhost:8080/sample/application/consumer/"+upload.get("uploadFilename")+".download", String.class);
		logger.debug(fff);
        logger.warn("===================================================");
	}	
}
