package com.u2ware.springfield.config;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.xml.NamespaceHandlerSupport;

public class ModulesNameSpaceHandler extends NamespaceHandlerSupport{

	protected final Log logger = LogFactory.getLog(getClass());

	public void init() {
		registerBeanDefinitionParser("modules", new ModulesConfigDefinitionParser());
	}
}
//JpaRepositoryNameSpaceHandler