package com.u2ware.springfield.support.messages.test3;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.support.MessageSourceAccessor;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.u2ware.springfield.AbstractContextTestRoot;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class SpringfieldMessageTest extends AbstractContextTestRoot{

	@Autowired 
	protected MessageSourceAccessor messageSourceAccessor;
	
	@Test
	public void testMessage() throws Exception {
		logger.warn(messageSourceAccessor.getMessage("application.title"));
		logger.warn(messageSourceAccessor.getMessage("foo.title"));
		logger.warn(messageSourceAccessor.getMessage("bar.title"));
	}


	
}
