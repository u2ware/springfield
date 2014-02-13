package com.u2ware.springfield.support.messages.test2;

import java.util.Map;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.context.support.MessageSourceAccessor;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.u2ware.springfield.AbstractContextTestRoot;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class SpringfieldMessageTest extends AbstractContextTestRoot{

	@Test
	public void test() throws Exception {
		Map<String, MessageSourceAccessor> beans = applicationContext.getBeansOfType(MessageSourceAccessor.class);
		
		for(String name : beans.keySet()){
			
			MessageSourceAccessor bean = applicationContext.getBean(name, MessageSourceAccessor.class);
			
			try{
				logger.warn(name+"[foo.title] -->"+bean.getMessage("foo.title"));
			}catch(Exception e){
				logger.warn(name+"[foo.title] -->");
			}
			try{
				logger.warn(name+"[bar.title] -->"+bean.getMessage("bar.title"));
			}catch(Exception e){
				logger.warn(name+"[bar.title] -->");
			}
		}
	}
}
