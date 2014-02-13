package com.u2ware.springfield.support.environment.test1;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.u2ware.springfield.AbstractContextTestRoot;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class SpringfieldEnvironmentTest extends AbstractContextTestRoot{

	@Autowired
	protected Environment environment;
	
	@Test
	public void testEnv() throws Exception {
		logger.warn(environment.getProperty("a.b.c"));
		logger.warn(environment.getProperty("e.d.f"));
	}	
}
