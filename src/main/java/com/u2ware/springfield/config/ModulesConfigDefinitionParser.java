package com.u2ware.springfield.config;

import java.lang.annotation.Annotation;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.parsing.ReaderContext;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.xml.BeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.type.filter.AnnotationTypeFilter;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;
import org.w3c.dom.Element;

import com.u2ware.springfield.config.Springfield.Strategy;

public class ModulesConfigDefinitionParser implements BeanDefinitionParser{

	private static final Logger logger = LoggerFactory.getLogger(ModulesConfigDefinitionParser.class);


	private void handleError(Exception e, Element source, ParserContext parser) {
		ReaderContext reader = parser.getReaderContext();
		reader.error(e.getMessage(), reader.extractSource(source), e.getCause());
	}
	
	private Set<BeanDefinition> findCandidateComponents(ModulesConfig modulesConfig, ParserContext parser, Class<? extends Annotation> annotationType){
		ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(false);
		scanner.addIncludeFilter(new AnnotationTypeFilter(annotationType));
		scanner.setResourceLoader(parser.getReaderContext().getResourceLoader());
		return scanner.findCandidateComponents(modulesConfig.getBasePackage());
	}


	
	public BeanDefinition parse(Element element, ParserContext parser) {
	
		try {
			BeanDefinitionRegistry registry = parser.getRegistry();
			ModulesConfig modulesConfig = new ModulesConfig(element, parser);
			
			logger.warn("@Springfield");
			logger.warn("\thttp://u2waremanager.github.io/springfield");
			logger.warn("\t"+modulesConfig.getBasePackage()+" scaning...");

			
			Set<BeanDefinition> springfieldDefs  = findCandidateComponents(modulesConfig, parser, Springfield.class);
			
			for (BeanDefinition springfieldDef : springfieldDefs) {

				String targetClassName = springfieldDef.getBeanClassName();
				Class<?> targetClass = ClassUtils.forName(targetClassName, getClass().getClassLoader());

				Springfield targetSpringfield = AnnotationUtils.findAnnotation(targetClass, Springfield.class);
				Class<?> entityClass = targetSpringfield.entity();

				if(! Class.class.equals(entityClass)){
					logger.warn("@Springfield Mvc   : "+targetClassName);
					addConfiguration(registry, modulesConfig, entityClass, targetClass, targetSpringfield);
				}
			}
			
			for (BeanDefinition springfieldDef : springfieldDefs) {

				String targetClassName = springfieldDef.getBeanClassName();
				Class<?> targetClass = ClassUtils.forName(targetClassName, getClass().getClassLoader());

				Springfield targetSpringfield = AnnotationUtils.findAnnotation(targetClass, Springfield.class);
				Class<?> entityClass = targetSpringfield.entity();
				
				if(Class.class.equals(entityClass)){
					logger.warn("@Springfield Mvc   : "+targetClassName);
					addConfiguration(registry, modulesConfig, targetClass, targetClass, targetSpringfield);
				}
			}
			
			////////////////
			//
			////////////////
			
			
		} catch (Exception e) {
			logger.warn(e.getMessage(), e);
			handleError(e, element, parser);
		} 
		return null;
	}
	
	
	///////////////////////////////////////
	//
	///////////////////////////////////////
	private void registerBeanDefinition(BeanDefinitionRegistry registry, String beanName, BeanDefinition beanDefinition){
		registry.registerBeanDefinition(beanName, beanDefinition);
		logger.warn(beanName+" = "+beanDefinition.getBeanClassName());
	}
	
	private void registerBeanDefinition(BeanDefinitionRegistry registry, String beanName){
		logger.warn(beanName+" = <<bean name used in registry >>");
	}
	
	
	///////////////////////////////////////
	//
	///////////////////////////////////////
	private void addConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) throws Exception{

		Strategy strategy = webapp.strategy();
		if(Strategy.DEFAULT_STRATEGY.equals(strategy)){
			strategy = Strategy.valueOf(modulesConfig.getDefaultStrategy());
		}
		
		if(Strategy.HIBERNATE_REPOSITORY_ONLY.equals(strategy)){
			addHibernateRepositoryConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			
		}else if(Strategy.JPA_REPOSITORY_ONLY.equals(strategy)){
			addJpaRepositoryConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);

		}else if(Strategy.MONGODB_REPOSITORY_ONLY.equals(strategy)){
			addMangodbRepositoryConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
	
		}else if(Strategy.SQLSESSION_REPOSITORY_ONLY.equals(strategy)){
			addSqlSessionRepositoryConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
		
		}else if(Strategy.HIBERNATE.equals(strategy)){
			addEntityInformationConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addHibernateRepositoryConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityServiceConfiguration(registry, modulesConfig, entityClass, queryClass, webapp, false);
			addEntityValidatorConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityControllerConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			
		}else if(Strategy.JPA.equals(strategy)){
			addEntityInformationConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addJpaRepositoryConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityServiceConfiguration(registry, modulesConfig, entityClass, queryClass, webapp, false);
			addEntityValidatorConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityControllerConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);

		}else if(Strategy.MONGODB.equals(strategy)){
			addEntityInformationConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addMangodbRepositoryConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityServiceConfiguration(registry, modulesConfig, entityClass, queryClass, webapp, false);
			addEntityValidatorConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityControllerConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);

		}else if(Strategy.SQLSESSION.equals(strategy)){
			addEntityInformationConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addSqlSessionRepositoryConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityServiceConfiguration(registry, modulesConfig, entityClass, queryClass, webapp, false);
			addEntityValidatorConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityControllerConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			
		}else if(Strategy.DTO.equals(strategy)){
			addEntityInformationConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityServiceConfiguration(registry, modulesConfig, entityClass, queryClass, webapp, true);
			addEntityValidatorConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityControllerConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
		}	
	}
	

	///////////////////////////////////////
	//
	///////////////////////////////////////
	private void addEntityInformationConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) throws Exception{

		String beanName = ClassUtils.getShortNameAsProperty(queryClass)+"Information";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}
		
		String topLevelMapping = webapp != null ? webapp.topLevelMapping() : "";
		if(! StringUtils.hasText(topLevelMapping)){
			String root = ClassUtils.convertClassNameToResourcePath(modulesConfig.getBasePackage());
			topLevelMapping = ClassUtils.classPackageAsResourcePath(queryClass).replaceAll(root, "");
		}
		
		String[] methodLevelMapping = webapp.methodLevelMapping();
		String[] identity = webapp.identity();
		String attributesCSV = webapp.attributesCSV();

		//logger.info(beanName+ " "+attributesCSV);
		
		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.domain.EntityInformation")
				.addConstructorArgValue(modulesConfig.getBasePackage())
				.addConstructorArgValue(entityClass)
				.addConstructorArgValue(queryClass)
				.addConstructorArgValue(topLevelMapping)
				.addConstructorArgValue(methodLevelMapping)
				.addConstructorArgValue(identity)
				.addConstructorArgValue(attributesCSV)

				.getBeanDefinition();

		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}

	///////////////////////////////////////
	//
	///////////////////////////////////////
	private void addHibernateRepositoryConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) {

		String beanName = ClassUtils.getShortNameAsProperty(entityClass)+"Repository";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}

		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.repository.hibernate.EntityHibernateRepository")
				.addConstructorArgValue(entityClass)
				.addConstructorArgValue(null)
				.getBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
		
	}

	private void addJpaRepositoryConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) throws Exception{

		String beanName = ClassUtils.getShortNameAsProperty(entityClass)+"Repository";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}
		
		//logger.info(beanName);

		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.repository.jpa.EntityJpaRepository")
				.addConstructorArgValue(entityClass)
				.addConstructorArgValue(null)
				.getBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	
	private void addSqlSessionRepositoryConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) throws Exception{

		String beanName = ClassUtils.getShortNameAsProperty(entityClass)+"Repository";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}

		//logger.info(beanName);

		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.repository.sqlsession.EntitySqlSessionRepository")
				.addConstructorArgValue(entityClass)
				.addConstructorArgValue(null)
				.getBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	
	private void addMangodbRepositoryConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) throws Exception{
		String beanName = ClassUtils.getShortNameAsProperty(entityClass)+"Repository";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}

		//logger.info(beanName);

		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.repository.mongodb.EntityMongodbRepository")
				.addConstructorArgValue(entityClass)
				.addConstructorArgValue(null)
				.getBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	
	/////////////////////////////////////////////////
	//
	/////////////////////////////////////////////////
	private void addEntityServiceConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp, boolean isDummy) throws Exception{
		String beanName = ClassUtils.getShortNameAsProperty(queryClass)+"Service";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}
		

		BeanDefinition beanDefinition = null;
		if(isDummy){
			beanDefinition = BeanDefinitionBuilder
					.rootBeanDefinition("com.u2ware.springfield.service.EntityServiceImpl")
					.getBeanDefinition();
		}else{
			String repositoryName = ClassUtils.getShortNameAsProperty(entityClass)+"Repository";
			
			//logger.info(beanName);
			beanDefinition = BeanDefinitionBuilder
					.rootBeanDefinition("com.u2ware.springfield.service.EntityServiceImpl")
					.addConstructorArgReference(repositoryName)
					.getBeanDefinition();
		}
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	
	///////////////////////////////////////
	//
	///////////////////////////////////////
	private void addEntityValidatorConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) throws Exception{

		String beanName = ClassUtils.getShortNameAsProperty(queryClass)+"Validator";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}
		
		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.validation.EntityValidatorImpl")
				.getBeanDefinition();

		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}

	
	///////////////////////////////////////
	//
	///////////////////////////////////////
	private void addEntityControllerConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) throws Exception{

		String beanName = ClassUtils.getShortNameAsProperty(queryClass)+"Controller";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}

		//logger.info(beanName);
		String informationName = ClassUtils.getShortNameAsProperty(queryClass)+"Information";
		String serviceName = ClassUtils.getShortNameAsProperty(queryClass)+"Service";
		String validatorName = ClassUtils.getShortNameAsProperty(queryClass)+"Validator";
		
		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.controller.EntityControllerImpl")
				.addConstructorArgReference(informationName)
				.addConstructorArgReference(serviceName)
				.addConstructorArgReference(validatorName)
				.getRawBeanDefinition();
				
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	
	
}

/*

			////////////////////////////////////////////////
			//
			////////////////////////////////////////////////
			Set<BeanDefinition> mongodbDefs = findCandidateComponents(modulesConfig, parser, Document.class);
			for (BeanDefinition mongodbDef : mongodbDefs) {

				String entityClassName = mongodbDef.getBeanClassName();
				Class<?> entityClass = ClassUtils.forName(entityClassName, getClass().getClassLoader());
				
				addMangodbRepositoryConfiguration(registry, modulesConfig, entityClass);
				addServiceConfiguration(registry, modulesConfig, entityClass);	
			}

			////////////////////////////////////////////////
			//
			////////////////////////////////////////////////
			Set<BeanDefinition> entityDefs = findCandidateComponents(modulesConfig, parser, Entity.class);
			for (BeanDefinition entityDef : entityDefs) {

				String entityClassName = entityDef.getBeanClassName();
				Class<?> entityClass = ClassUtils.forName(entityClassName, getClass().getClassLoader());
				
				addJpaRepositoryConfiguration(registry, modulesConfig, entityClass);
				addServiceConfiguration(registry, modulesConfig, entityClass);	
			}
			
			////////////////////////////////////////////////
			//
			////////////////////////////////////////////////
			Set<BeanDefinition> sqlSessionDefs = findCandidateComponents(modulesConfig, parser, Persistent.class);
			for (BeanDefinition sqlSessionDef : sqlSessionDefs) {

				String entityClassName = sqlSessionDef.getBeanClassName();
				Class<?> entityClass = ClassUtils.forName(entityClassName, getClass().getClassLoader());
				
				addSqlSessionRepositoryConfiguration(registry, modulesConfig, entityClass);
				addServiceConfiguration(registry, modulesConfig, entityClass);	
			}
			
			//////////////////////////////////
			//
			/////////////////////////////////////////
			Set<BeanDefinition> springfieldDefs  = findCandidateComponents(modulesConfig, parser, Springfield.class);
			
			//int controllerCount = 0;
			for (BeanDefinition springfieldDef : springfieldDefs) {
				
				String queryClassName = springfieldDef.getBeanClassName();
				Class<?> queryClass = ClassUtils.forName(queryClassName, getClass().getClassLoader());
				Springfield springfield = AnnotationUtils.findAnnotation(queryClass, Springfield.class);
				
				Class<?> entityClass = springfield.entity();
				if(Class.class.equals(entityClass))
					entityClass = queryClass;

				//logger.debug("..............."+webappClass);
				//logger.debug("..............."+webappEntityClass);
				//logger.debug("..............."+StringUtils.arrayToCommaDelimitedString(webapp.extensions()));
				
				addHandlerMetamodelConfiguration(registry, modulesConfig, entityClass, queryClass, springfield);
				addHandlerConfiguration(registry, modulesConfig, entityClass, queryClass);
				//controllerCount++;
			}



private BeanDefinition getEntityManagerBeanDefinition(ModulesConfig modulesConfig) {
	BeanDefinitionBuilder builder = BeanDefinitionBuilder
			.rootBeanDefinition("org.springframework.orm.jpa.SharedEntityManagerCreator")
			.setFactoryMethod("createSharedEntityManager")
			.addConstructorArgReference(modulesConfig.getEntityManagerFactoryRef());
	
	AbstractBeanDefinition bean = builder.getRawBeanDefinition();
	bean.setSource(modulesConfig.getSource());
	return bean;
}

	BeanDefinition beanDefinition = BeanDefinitionBuilder
			.rootBeanDefinition("com.u2ware.springfield.repository.jpa.EntityRepositoryImpl")
			.addConstructorArgValue(entityClass)
			.addConstructorArgValue(this.getEntityManagerBeanDefinition(modulesConfig))
			.getBeanDefinition();
			
	BeanDefinition beanDefinition = BeanDefinitionBuilder
			.rootBeanDefinition("com.u2ware.springfield.service.EntityServiceImpl")
			.addConstructorArgReference(ClassUtils.getShortNameAsProperty(entityClass)+"Repository")
			.addConstructorArgReference(modulesConfig.getTransactionManagerRef())
			.getBeanDefinition();
			
			//.addConstructorArgValue(this.getEntityManagerBeanDefinition(modulesConfig))
			
*/
/*

/////////////////////////////////
//
/////////////////////////////////	
private void addEventListenerConfigurerConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {
	
	String beanName = "com.u2ware.springfield.repository.HibernateEventListenerConfigurer";
	if(registry.isBeanNameInUse(beanName)) return;

	BeanDefinition beanDefinition = BeanDefinitionBuilder
			.rootBeanDefinition("com.u2ware.springfield.repository.HibernateEventListenerConfigurer")
			.addConstructorArgValue(this.getEntityManagerBeanDefinition(modulesConfig))
			.getBeanDefinition();
	
	this.registerBeanDefinition(registry, beanName, beanDefinition);
}

///////////////////////////////////////
//
///////////////////////////////////////
private void addHandlerViewResolverConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {

	if(StringUtils.hasText(modulesConfig.getContentNegotiatingViewResolverRef())) return;
	
	String beanName = "contentNegotiatingViewResolverBySpringfield";
	if(registry.isBeanNameInUse(beanName)) return;

	Map<String,String> mediaTypes = new HashMap<String,String>();
	ManagedList<BeanDefinition> viewResolvers = new ManagedList<BeanDefinition>();



		String defaultContentType = "text/html";
		viewResolvers.add(
			BeanDefinitionBuilder
			.rootBeanDefinition("com.u2ware.springfield.view.thymeleaf.ThymeleafViewResolver")
			.addPropertyValue("providePrefix", "classpath:com/u2ware/springfield/view/thymeleaf/TwitterBootstrapBasedSample1")
			.addPropertyValue("provideSuffix", ".html")
			.getBeanDefinition()				
		);

		mediaTypes.put("html", "text/html");
		viewResolvers.add(
			BeanDefinitionBuilder
			.rootBeanDefinition("com.u2ware.springfield.view.thymeleaf.ThymeleafViewResolver")
			.addPropertyValue("viewNames", "*.html")
			.addPropertyValue("providePrefix", "classpath:com/u2ware/springfield/view/thymeleaf/JQueryMobileBasedSample1")
			.addPropertyValue("provideSuffix", ".html")
			.getBeanDefinition()				
		);

		mediaTypes.put("thymeleaf", "text/html");
		viewResolvers.add(
			BeanDefinitionBuilder
			.rootBeanDefinition("com.u2ware.springfield.view.thymeleaf.ThymeleafViewResolver")
			.addPropertyValue("viewNames", "*.thymeleaf")
			.getBeanDefinition()
		);

		mediaTypes.put("json", "application/json");
		viewResolvers.add(
			BeanDefinitionBuilder
			.rootBeanDefinition("com.u2ware.springfield.view.jackson.JsonViewResolver")
			.addPropertyValue("viewNames", "*.json")
			.getBeanDefinition()				
		);

		mediaTypes.put("xjson", "application/json");
		viewResolvers.add(
			BeanDefinitionBuilder
			.rootBeanDefinition("com.u2ware.springfield.view.xstream.JsonViewResolver")
			.addPropertyValue("viewNames", "*.xjson")
			.getBeanDefinition()				
		);

		mediaTypes.put("xml", "application/xml");
		viewResolvers.add(
			BeanDefinitionBuilder
			.rootBeanDefinition("com.u2ware.springfield.view.xstream.XmlViewResolver")
			.addPropertyValue("viewNames", "*.xml")
			.getBeanDefinition()
		);
	
		mediaTypes.put("xls", "application/vnd.ms-excel");
		viewResolvers.add(
			BeanDefinitionBuilder
			.rootBeanDefinition("com.u2ware.springfield.view.jexcel.ExcelViewResolver")
			.addPropertyValue("viewNames", "*.xls")
			.getBeanDefinition()
		);

	Resource location = new ClassPathResource(ClassUtils.convertClassNameToResourcePath(modulesConfig.getBasePackage())+"/tiles-definitions.xml");
	logger.debug(location);
	logger.debug(location.exists());
	//logger.debug(location.exists());
	
	if(location.exists()){
		mediaTypes.put("tiles", "text/html");
		
		viewResolvers.add(
			BeanDefinitionBuilder
			.rootBeanDefinition("com.u2ware.springfield.view.tiles.TilesViewResolver")
			.addPropertyValue("viewNames", "*.tiles")
			.addPropertyValue("definition", location)
			.getBeanDefinition()
		);
	}
	
	
		
	BeanDefinition beanDefinition = BeanDefinitionBuilder
			.rootBeanDefinition("org.springframework.web.servlet.view.ContentNegotiatingViewResolver")
			.addPropertyValue("defaultContentType", defaultContentType)
			.addPropertyValue("mediaTypes", mediaTypes)
			.addPropertyValue("viewResolvers", viewResolvers)
			.getBeanDefinition();

	
	
	this.registerBeanDefinition(registry, beanName, beanDefinition);
	
}

///////////////////////////////////////
//
///////////////////////////////////////
private void addMessageSourceConfiguration(BeanDefinitionRegistry registry,ModulesConfig modulesConfig) {
	if(StringUtils.hasText(modulesConfig.getMessageSourceRef())) return;

	String beanName = "messageSource";
	if(registry.isBeanNameInUse(beanName)) return;
	
	BeanDefinition beanDefinition = BeanDefinitionBuilder
			.rootBeanDefinition("com.u2ware.springfield.support.message.ExtendedReloadableResourceBundleMessageSource")
			.addPropertyValue("basenamePackages", modulesConfig.getBasePackage())
			.addPropertyValue("defaultEncoding", "UTF-8")
			.getBeanDefinition();
	
	this.registerBeanDefinition(registry, beanName, beanDefinition);
}

private void addConversionServiceConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {
	if(StringUtils.hasText(modulesConfig.getConversionServiceRef())) return;

	String beanName = "conversionService";
	if(registry.isBeanNameInUse(beanName)) return;
	
	//Set formatter = new HashSet();
	//formatter.add(new StringTrimFormatter());
	
	BeanDefinition beanDefinition = BeanDefinitionBuilder
			.rootBeanDefinition("org.springframework.format.support.FormattingConversionServiceFactoryBean")
			//.addPropertyReference("formatter", "com.u2ware.springfield.support.conversion.StringTrimFormatter")
			.getBeanDefinition();
	
	this.registerBeanDefinition(registry, beanName, beanDefinition);
}


private void addValidatorConfiguration(BeanDefinitionRegistry registry,ModulesConfig modulesConfig) {
	if(StringUtils.hasText(modulesConfig.getValidatorRef())) return;
	
	String beanName = "validator";
	if(registry.isBeanNameInUse(beanName)) return;
	
	BeanDefinition messageInterpolator = BeanDefinitionBuilder
			.rootBeanDefinition("com.u2ware.springfield.support.validation.ValidationMessageInterpolator")
			.getBeanDefinition();
	
	BeanDefinition beanDefinition = BeanDefinitionBuilder
			.rootBeanDefinition("org.springframework.validation.beanvalidation.LocalValidatorFactoryBean")
			.addPropertyValue("messageInterpolator", messageInterpolator)
			.getBeanDefinition();
	
	this.registerBeanDefinition(registry, beanName, beanDefinition);
}

*/




