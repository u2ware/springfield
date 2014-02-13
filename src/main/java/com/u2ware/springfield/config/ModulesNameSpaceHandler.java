package com.u2ware.springfield.config;

import org.springframework.beans.factory.xml.NamespaceHandlerSupport;

public class ModulesNameSpaceHandler extends NamespaceHandlerSupport{

	public void init() {
		
		
		registerBeanDefinitionParser("modules", new ModulesConfigDefinitionParser());
	}
}
//JpaRepositoryNameSpaceHandler