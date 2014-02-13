package example.u2ware.springfield.part1.step3;

import static org.springframework.data.mongodb.core.query.Criteria.where;
import static org.springframework.data.mongodb.core.query.Query.query;
import static org.springframework.data.mongodb.core.query.Update.update;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;



//@RunWith(SpringJUnit4ClassRunner.class)
//@ContextConfiguration(locations="application-context.xml")
public class SpringDataMongodbReferenceTest2 {

	
	protected final Log logger = LogFactory.getLog(getClass());

	@Autowired
	protected ApplicationContext applicationContext;
	
	//@Test
	public void print() throws Exception{
		logger.info("======================================================================ApplicationContext");
		if(applicationContext != null){
			for(String name : applicationContext.getBeanDefinitionNames()){
				logger.info(name+"="+applicationContext.getType(name));
			}
		}
		logger.info("======================================================================ApplicationContext");
	}

	@Autowired
	protected MongoOperations mongoOps;
	
	//@Test
	public void print2() throws Exception{
		Person p = new Person("Joe", 34);
	    
	    // Insert is used to initially store the object into the database.
	    mongoOps.insert(p);
	    logger.info("Insert: " + p);
	    
	    // Find
	    p = mongoOps.findById(p.getAbcd(), Person.class);    
	    logger.info("Found: " + p);
	 
	    
	  
	    
	    
	    // Update
	    mongoOps.updateFirst(query(where("name").is("Joe")), update("age", 35), Person.class);    
	    p = mongoOps.findOne(query(where("name").is("Joe")), Person.class);
	    logger.info("Updated: " + p);
	    
	    // Delete
	    mongoOps.remove(p);
	    
	    // Check that deletion worked
	    List<Person> people =  mongoOps.findAll(Person.class);
	    logger.info("Number of people = : " + people.size());

	    mongoOps.dropCollection(Person.class);
	}
	
	@Document
	public static @ToString class Person {

		public Person() {
		}
		public Person(String name, int age) {
			this.name = name;
			this.age = age;
		}
		
		@Id
		private @Getter @Setter String abcd;
		
		
		private @Getter @Setter String name;
		private @Getter @Setter int age;

	}
	
}
