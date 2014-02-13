package example.u2ware.springfield.part1.step3;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Test;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.BasicQuery;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;

import com.mongodb.Mongo;

//@RunWith(SpringJUnit4ClassRunner.class)
//@WebAppConfiguration
//@ContextConfiguration(locations="application-context.xml")
public class SpringDataMongodbReferenceTest1 {

	
	protected final Log logger = LogFactory.getLog(getClass());
	
	@NoArgsConstructor @AllArgsConstructor @ToString 
	public static class Person {

		private @Getter @Setter Integer id;
		private @Getter @Setter String name;
		private @Getter @Setter String title;

	}
	
	//@Test
	public void testMongodb1() throws Exception{

		logger.debug("print1print1print1print1");
		
		
		MongoOperations mongoOps = new MongoTemplate(new Mongo(), "database");
		
		if (mongoOps.collectionExists(Person.class)) {
			mongoOps.dropCollection(Person.class);
		}
		mongoOps.createCollection(Person.class);

		mongoOps.insert(new Person(34, "Joe", "t1"));
		mongoOps.insert(new Person(21, "Lee", "t2"));

		List<Person> persons = mongoOps.findAll( Person.class);
		for(Person person : persons){
			logger.debug(person);
		}

		logger.debug("------------------------------------------------------");
		Person person1 = mongoOps.findOne(new Query(Criteria.where("name").is("Joe")), Person.class);
		logger.debug(person1);
		
		
		logger.debug("------------------------------------------------------");
		List<Person> person2 = mongoOps.find(new BasicQuery("{'name' : 'Joe'}"), Person.class);
		logger.debug(person2);
		
		
		logger.debug("------------------------------------------------------");
		BasicQuery q = new BasicQuery("{}" , "{'title' : -1}");
		q.with(new Sort(Direction.DESC, "title"));
		List<Person> person3 = mongoOps.find(q, Person.class);
		logger.debug(person3);
		
		mongoOps.dropCollection(Person.class);
	}
	
	@Test
	public void testMongodb2() throws Exception{

		logger.debug("print1print1print1print1");
		
		
		MongoOperations mongoOps = new MongoTemplate(new Mongo(), "database");
		
		if (mongoOps.collectionExists("myperson")) {
			mongoOps.dropCollection("myperson");
		}
		mongoOps.createCollection("myperson");

		
		Map<String,Object> p1 = new HashMap<String,Object>();
		p1.put("id", 34);
		p1.put("name", "Joe");
		p1.put("title", "t1");
		p1.put("code", "c1");

		Map<String,Object> p2 = new HashMap<String,Object>();
		p2.put("id", 21);
		p2.put("name", "Lee");
		p2.put("title", "t2");
		p2.put("code", "c2");

		mongoOps.insert(p1 , "myperson");
		mongoOps.insert(p2, "myperson");

		List<Map> persons = mongoOps.findAll( Map.class, "myperson");
		for(Map person : persons){
			logger.debug(person);
		}
		logger.debug("------------------------------------------------------");

		Map person1 = mongoOps.findOne(new Query(Criteria.where("name").is("Joe")), Map.class, "myperson");
		logger.debug(person1);
		
		logger.debug("------------------------------------------------------");
		List<Map> person2 = mongoOps.find(new BasicQuery("{'name' : 'Joe'}"), Map.class, "myperson");
		logger.debug(person2);
		
		logger.debug("------------------------------------------------------");
		BasicQuery q3 = new BasicQuery("{}" , "{'title' : 1}");
		q3.with(new Sort(Direction.DESC, "title"));
		List<Map> person3 = mongoOps.find(q3, Map.class, "myperson");
		logger.debug(person3);

		logger.debug("------------------------------------------------------");
		//BasicQuery q4 = new BasicQuery("{ {'name' : 'Joe'} ,  {'title' : 't1'} }");
		//BasicQuery q4 = new BasicQuery("{ 'name' : 'Joe' ,  'title' : 't1' }");
		BasicQuery q4 = new BasicQuery("{ $or : [ {'name' : 'Joe'},  {'title' : 't2'} ] }");
		//BasicQuery q4 = new BasicQuery("{ $and : [ {'name' : 'Joe'} ,  {'title' : 't1'} ] }");
		//q.with(new Sort(Direction.DESC, "title"));

		
		List<Map> person4 = mongoOps.find(q4, Map.class, "myperson");
		logger.debug(person4);
		
		mongoOps.dropCollection("myperson");
	}
	
	
//50ee14ed58b0ef93a84c56b4
//50ee14ed58b0ef93a84c56b4
}
