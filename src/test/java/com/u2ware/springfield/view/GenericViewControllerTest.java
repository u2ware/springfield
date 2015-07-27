package com.u2ware.springfield.view;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.fileUpload;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.servlet.ViewResolver;

import com.fasterxml.jackson.databind.ObjectMapper;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes=GenericViewControllerTestConfig.class)
@WebAppConfiguration
public class GenericViewControllerTest {
	
    protected Log logger = LogFactory.getLog(getClass());

    @Autowired
    protected WebApplicationContext applicationContext;
    private MockMvc mockMvc;
    
    @Autowired
    private ViewResolver[] viewResolvers;

    @Before
    public void setup() throws Exception {
        logger.warn("===================================================");
//        String[] beanNames = applicationContext.getBeanDefinitionNames();
//        Arrays.sort(beanNames, 0, beanNames.length);
//        for(String name : beanNames){
//            logger.warn(name+"="+applicationContext.getBean(name).getClass());
//        }
        logger.warn("===================================================");
        this.mockMvc = MockMvcBuilders.webAppContextSetup(applicationContext).build();
    }

    @Test
    public void testExtension() throws Exception{
    	
    	String content = null;
        logger.warn("===================================================");
        content = this.mockMvc.perform(
        		get("/consumer.html")
    			)
        		.andExpect(status().isOk())
        		.andReturn().getResponse().getContentAsString();
        logger.debug(content);
        
        
        logger.warn("===================================================");
        content = this.mockMvc.perform(
        		get("/consumer.json")
    			)
        		.andExpect(status().isOk())
        		.andReturn().getResponse().getContentAsString();
        logger.debug(content);

        logger.warn("===================================================");
        content = this.mockMvc.perform(
        		get("/consumer.xml")
    			)
        		.andExpect(status().isOk())
        		.andReturn().getResponse().getContentAsString();
        logger.debug(content);
        
        logger.warn("===================================================");
        content = this.mockMvc.perform(
        		get("/consumer.xls")
    			)
        		.andExpect(status().isOk())
        		.andReturn().getResponse().getContentAsString();
        logger.debug(content);

        logger.warn("===================================================");
        content = this.mockMvc.perform(
        		fileUpload("/consumer.upload")
        		.file(newMultipartFile("README.md"))
    			)
        		.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
        logger.debug(content);

        logger.warn("===================================================");
        content = this.mockMvc.perform(
        		get("/consumer/"+getUploadFilename(content)+".download")
    			)
        		.andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
        logger.debug(content);
    }    
    
	public static MockMultipartFile newMultipartFile(String name) throws Exception{
    	Path path = Paths.get(name);
    	File file = path.toFile();
        MockMultipartFile multipartFile = new MockMultipartFile(
        		"multipartFile", 
        		file.getName(),
        		java.nio.file.Files.probeContentType(path),
        		com.google.common.io.Files.toByteArray(file));
        return multipartFile;
	}
	
	@SuppressWarnings("unchecked")
	public static String getUploadFilename(String jsonString) throws Exception{
		Map<String,Object> json = new ObjectMapper().readValue(jsonString, Map.class);
		return (String)json.get("uploadFilename");
	}
    
}


