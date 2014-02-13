package com.u2ware.springfield.config;

import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.core.io.ResourceLoader;
import org.w3c.dom.Element;

public class ModulesConfig  {

	public static final String BASE_PACKAGE = "base-package";

	public static final String PROPERTIES = "properties-ref";
	
	public static final String DEFAULT_STRATEGY = "default-strategy";
	
	public static final String DATA_SOURCE_REF = "data-source-ref";
	
	public static final String ENTITY_MANAGER_FACTORY_REF = "entity-manager-factory-ref";

	public static final String SESSION_FACTORY_REF = "session-factory-ref";

	public static final String SQL_SESSION_FACTORY_REF = "sql-session-factory-ref";

	//public static final String MONGO_TEMPLATE_REF = "mongo-template-ref";


	
	private Element element;
	private ParserContext parser;

	public ModulesConfig(Element element, ParserContext parser) throws ClassNotFoundException, LinkageError{
		this.element = element;
		this.parser = parser;
	}
	public Object getSource() {
		return element;
	}
	public ResourceLoader getResourceLoader() {
		return parser.getReaderContext().getResourceLoader();
	}
	public String getBasePackage() {
		return element.getAttribute(BASE_PACKAGE);
	}
	public String getPropertiesRef() {
		return element.getAttribute(PROPERTIES);
	}
	public String getDataSourceRef() {
		return element.getAttribute(DATA_SOURCE_REF);
	}
	public String getEntityManagerFactoryRef() {
		return element.getAttribute(ENTITY_MANAGER_FACTORY_REF);
	}
	public String getSessionFactoryRef() {
		return element.getAttribute(SESSION_FACTORY_REF);
	}
	public String getSqlSessionFactoryRef() {
		return element.getAttribute(SQL_SESSION_FACTORY_REF);
	}
	public String getDefaultStrategy() {
		return element.getAttribute(DEFAULT_STRATEGY);
	}
	
	
}