package com.u2ware.springfield.config;

import org.springframework.beans.factory.xml.NamespaceHandlerSupport;

public class SpringfieldNameSpaceHandler extends NamespaceHandlerSupport{

	public void init() {
		registerBeanDefinitionParser("generic-mvc", new GenericMvcBeanDefinitionParser());
	}
}	
