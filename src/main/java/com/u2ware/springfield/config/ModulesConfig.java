package com.u2ware.springfield.config;

import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.core.io.ResourceLoader;
import org.w3c.dom.Element;

public class ModulesConfig  {

	public static final String BASE_PACKAGE = "base-package";

	public static final String TRANSACTION_MANAGER_REF = "transaction-manager-ref";
	public static final String ENTITY_MANAGER_FACTORY_REF = "entity-manager-factory-ref";
	public static final String DEFAULT_STRATEGY = "default-strategy";
	
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
	public String getEntityManagerFactoryRef() {
		return element.getAttribute(ENTITY_MANAGER_FACTORY_REF);
	}
	public String getTransactionManagerRef() {
		return element.getAttribute(TRANSACTION_MANAGER_REF);
	}
	public String getDefaultStrategy() {
		return element.getAttribute(DEFAULT_STRATEGY);
	}

//	public static final String MESSAGE_SOURCE_REF = "message-source-ref";
//	public static final String CONVERSION_SERVICE_REF = "conversion-service-ref";
//	public static final String VALIDATOR_REF = "validator-ref";
//	public static final String CONTENT_NEGOTIATING_VIEW_RESOLVER_REF = "content-negotiating-view-resolver-ref";
	
//	public String getMessageSourceRef() {
//		return element.getAttribute(MESSAGE_SOURCE_REF);
//	}
//	public String getConversionServiceRef() {
//		return element.getAttribute(CONVERSION_SERVICE_REF);
//	}
//	
//	public String getValidatorRef() {
//		return element.getAttribute(VALIDATOR_REF);
//	}
//	public String getContentNegotiatingViewResolverRef() {
//		return element.getAttribute(CONTENT_NEGOTIATING_VIEW_RESOLVER_REF);
//	}

}