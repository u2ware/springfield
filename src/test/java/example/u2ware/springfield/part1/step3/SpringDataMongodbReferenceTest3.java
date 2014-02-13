package example.u2ware.springfield.part1.step3;


import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.BasicQuery;

import example.u2ware.springfield.part1.step3.MongodbBean;


//@RunWith(SpringJUnit4ClassRunner.class)
//@WebAppConfiguration
//@ContextConfiguration(locations="../../application-context.xml")
public class SpringDataMongodbReferenceTest3 {

	protected final Log logger = LogFactory.getLog(getClass());


	@Autowired
	private MongoOperations mongoOperations;

	//@Before
	public void before() throws Exception{
		mongoOperations.createCollection(MongodbBean.class);
		for(int i = 0 ; i < 10 ; i++){
			mongoOperations.insert(new MongodbBean(i , "pwd"+i, "korea", "addr-"+(10-i)));
		}
	}
	
	//@Test
	public void testWhere() throws Exception{

		
		List<MongodbBean> list1 = mongoOperations.find(new BasicQuery("{id : 1}"), MongodbBean.class);
		logger.debug(list1);

		List<MongodbBean> list4 = mongoOperations.find(new BasicQuery("{id : { $gt : 2}}"), MongodbBean.class);
		for(MongodbBean e : list4){
			logger.debug(e);
		}
		
		List<MongodbBean> list5 = mongoOperations.find(new BasicQuery("{id : { $lt : 6}}"), MongodbBean.class);
		for(MongodbBean e : list5){
			logger.debug(e);
		}
		
		List<MongodbBean> list6 = mongoOperations.find(new BasicQuery("{id : { $gt : 2 , $lt : 6 }}"), MongodbBean.class);
		for(MongodbBean e : list6){
			logger.debug(e);
		}

		List<MongodbBean> list2 = mongoOperations.find(new BasicQuery("{$or : [ {id : { $gt : 4}} , {id : {$lt : 2} } ] }"), MongodbBean.class);
		for(MongodbBean e : list2){
			logger.debug(e);
		}
		
	}
	
	//@After
	public void afterMongoOperations() throws Exception {
		mongoOperations.dropCollection(MongodbBean.class);
	}
	


}
