package example.u2ware.springfield.part1.step3;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import example.u2ware.springfield.part1.step3.MongodbBean;


@RunWith(SpringJUnit4ClassRunner.class)
@WebAppConfiguration
@ContextConfiguration(locations="../../application-context.xml")
public class MongodbBeanControllerTest {

	protected final Log logger = LogFactory.getLog(getClass());

	@Autowired
	protected WebApplicationContext applicationContext;
	protected MockMvc mockMvc;

	@Before
	public void setup() throws Exception {
		mongoOperations.dropCollection(MongodbBean.class);
		logger.debug(mongoOperations.findAll(MongodbBean.class).size());
		this.mockMvc = MockMvcBuilders.webAppContextSetup(applicationContext).build();
		for(int i = 1 ; i < 10 ; i++){
			this.mockMvc.perform(
					post("/part1/step3/new")
					.param("id", ""+i)
					.param("password", "pwd"+i)
					.param("contry", "name"+i)
					.param("address", "addr-"+(10-i)))
				.andExpect(status().isOk());
		}
	}
	
	@Test
	public void testPagingAndOrdring() throws Exception{

		this.mockMvc.perform(get("/part1/step3")
				//.param("id", "id5")
				.param("model_query_pageable.pageNumber" , "0")
				.param("model_query_pageable.pageSize" , "2")
				.param("model_query_pageable.sortOrders[0].property" , "password")
				.param("model_query_pageable.sortOrders[0].direction" , "1")
				.param("model_query_pageable.sortOrders[1].property" , "address")
				.param("model_query_pageable.sortOrders[1].direction" , "-1"))
			.andExpect(status().isOk())
			.andExpect(model().attributeExists("model_query_result"));
		
	}	
	
	@After
	public void afterMongoOperations() throws Exception {
		mongoOperations.dropCollection(MongodbBean.class);
	}
	
	@Autowired
	private MongoOperations mongoOperations;


}
