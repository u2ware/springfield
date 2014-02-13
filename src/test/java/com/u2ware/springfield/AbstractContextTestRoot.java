package com.u2ware.springfield;

import java.util.Arrays;

import org.junit.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;

public abstract class AbstractContextTestRoot {

	protected final Logger logger = LoggerFactory.getLogger(getClass());
	
	@Autowired
	protected ApplicationContext applicationContext;
	
	@Before
	public void setup() throws Exception {
		logger.warn("===================================================");
		String[] beanNames = applicationContext.getBeanDefinitionNames();
		Arrays.sort(beanNames, 0, beanNames.length);
		for(String name : beanNames){
			logger.warn(name+"="+applicationContext.getBean(name).getClass());
		}
		logger.warn("===================================================");
	}
}
