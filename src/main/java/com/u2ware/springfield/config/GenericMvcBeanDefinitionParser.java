package com.u2ware.springfield.config;

import java.util.Arrays;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.parsing.ReaderContext;
import org.springframework.beans.factory.xml.BeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.core.env.Environment;
import org.springframework.core.io.ResourceLoader;
import org.springframework.util.StringUtils;
import org.w3c.dom.Element;

public class GenericMvcBeanDefinitionParser implements BeanDefinitionParser{

	protected Log logger = LogFactory.getLog(getClass());

	private Iterable<String> basePackages;
	private ResourceLoader resourceLoader;
	private Environment environment;
	private Object source;
	private String entityManagerFactoryRef;
	private String transactionManagerRef;
	
	
	@Override
	public BeanDefinition parse(Element element, ParserContext parser) {
		
		try{
			this.basePackages = Arrays.asList(StringUtils.delimitedListToStringArray(element.getAttribute("base-package"), ",", " "));
			this.resourceLoader = parser.getReaderContext().getResourceLoader();
			this.source = parser.getReaderContext().extractSource(element);
			this.environment = parser.getReaderContext().getEnvironment();

			this.entityManagerFactoryRef = element.getAttribute("entity-manager-factory-ref");
			this.transactionManagerRef = element.getAttribute("transaction-manager-ref");
			
			logger.debug("source : "+source);
			logger.debug("basePackages : "+basePackages);
			logger.debug("entityManagerFactoryRef : "+entityManagerFactoryRef);
			logger.debug("transactionManagerRef : "+transactionManagerRef);
			logger.debug("resourceLoader : "+resourceLoader);
			logger.debug("environment : "+environment);

			GenericMvcBeanDefinitionConfigurationSource configSource = new GenericMvcBeanDefinitionConfigurationSource();
			configSource.setBasePackages(this.basePackages);
			configSource.setResourceLoader(this.resourceLoader);
			configSource.setSource(this.source);
			configSource.setEnvironment(this.environment);
			configSource.setEntityManagerFactoryRef(this.entityManagerFactoryRef);
			configSource.setTransactionManagerRef(this.transactionManagerRef);
			
			new GenericMvcBeanDefinitionConfiguration().registerBeanDefinitions(parser.getRegistry(), configSource);
			
		}catch(Exception e){
			e.printStackTrace();
			handleError(e, element, parser.getReaderContext());
		}
		return null;
	}

	private void handleError(Exception e, Element source, ReaderContext reader) {
		reader.error(e.getMessage(), reader.extractSource(source), e);
	}
}
