package com.u2ware.springfield.config;

import java.lang.annotation.Annotation;
import java.util.Set;

import org.hibernate.ejb.HibernatePersistence;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.parsing.ReaderContext;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
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

	protected final Logger logger = LoggerFactory.getLogger(getClass());


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
			
			//BeanFactoryUtils.beanNamesForTypeIncludingAncestors((ListableBeanFactory)registry, DataSource.class);
			
			ModulesConfig modulesConfig = new ModulesConfig(element, parser);
			
			logger.warn("@Springfield");
			logger.warn("\thttp://u2ware.github.io/springfield");
			logger.warn("\t"+modulesConfig.getBasePackage()+" scanning...");
			logger.warn("\t");
			//////////////////////////////
			//
			//////////////////////////////
			addSpringfieldBaseConfiguration(registry, modulesConfig);
			addSpringfieldBaseMultipartConfiguration(registry, modulesConfig);
			addSpringfieldBaseMessageConfiguration(registry, modulesConfig);

			
			//////////////////////////////
			//
			//////////////////////////////
			int webmvcCount = 0;
			Set<BeanDefinition> springfieldDefs  = findCandidateComponents(modulesConfig, parser, Springfield.class);
			for (BeanDefinition springfieldDef : springfieldDefs) {

				String targetClassName = springfieldDef.getBeanClassName();
				Class<?> targetClass = ClassUtils.forName(targetClassName, getClass().getClassLoader());

				Springfield targetSpringfield = AnnotationUtils.findAnnotation(targetClass, Springfield.class);
				Class<?> entityClass = targetSpringfield.entity();

				if(! Class.class.equals(entityClass)){
					logger.warn("\t");
					logger.warn("@Springfield Mvc   : "+targetClassName);
					webmvcCount += addConfiguration(registry, modulesConfig, entityClass, targetClass, targetSpringfield);
					logger.warn("\t");
				}
			}
			
			for (BeanDefinition springfieldDef : springfieldDefs) {

				String targetClassName = springfieldDef.getBeanClassName();
				Class<?> targetClass = ClassUtils.forName(targetClassName, getClass().getClassLoader());

				Springfield targetSpringfield = AnnotationUtils.findAnnotation(targetClass, Springfield.class);
				Class<?> entityClass = targetSpringfield.entity();
				
				if(Class.class.equals(entityClass)){
					logger.warn("\t");
					logger.warn("@Springfield Mvc   : "+targetClassName);
					webmvcCount += addConfiguration(registry, modulesConfig, targetClass, targetClass, targetSpringfield);
					logger.warn("\t");
				}
			}
			
			//////////////////////////////
			//
			//////////////////////////////
			if(webmvcCount > 0){
				addSpringfieldWebmvcConfiguration(registry, modulesConfig);
				addSpringfieldWebmvcRenderConfiguration(registry, modulesConfig);
				addSpringfieldWebmvcSecurityConfiguration(registry, modulesConfig);
			}
			logger.warn("\t");
		
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
		logger.debug(beanName+" = "+beanDefinition.getBeanClassName());
	}
	
	private void registerBeanDefinition(BeanDefinitionRegistry registry, String beanName){
		logger.debug(beanName+" = <<bean name used in registry >>");
	}
	
	
	
	
	///////////////////////////////////////
	//
	///////////////////////////////////////
	private int addConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) throws Exception{

		Strategy strategy = webapp.strategy();
		if(Strategy.NULL.equals(strategy)){
			if(StringUtils.hasText(modulesConfig.getDefaultStrategy())){
				strategy = Strategy.valueOf(modulesConfig.getDefaultStrategy());
			}else{
				strategy = Strategy.JPA;
			}
		}
		
		if(Strategy.HIBERNATE_REPOSITORY_ONLY.equals(strategy)){
			addHibernateRepositoryConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			return 0;
			
		}else if(Strategy.JPA_REPOSITORY_ONLY.equals(strategy)){
			addJpaRepositoryConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			return 0;

		}else if(Strategy.JDBC_REPOSITORY_ONLY.equals(strategy)){
			addJdbcRepositoryConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			return 0;
	
		}else if(Strategy.SQLSESSION_REPOSITORY_ONLY.equals(strategy)){
			addSqlSessionRepositoryConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			return 0;
		
		}else if(Strategy.HIBERNATE.equals(strategy)){
			addHibernateRepositoryConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addHibernateServiceConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityInformationConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityValidatorConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityControllerConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			return 1;
			
		}else if(Strategy.JPA.equals(strategy)){
			addJpaRepositoryConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addJpaServiceConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityInformationConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityValidatorConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityControllerConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			return 1;

		}else if(Strategy.JDBC.equals(strategy)){
			addJdbcRepositoryConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addJdbcServiceConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityInformationConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityValidatorConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityControllerConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			return 1;

		}else if(Strategy.SQLSESSION.equals(strategy)){
			addSqlSessionRepositoryConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addSqlSessionServiceConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityInformationConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityValidatorConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityControllerConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			return 1;
			
		}else if(Strategy.DTO.equals(strategy)){
			addDummyServiceConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityInformationConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityValidatorConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			addEntityControllerConfiguration(registry, modulesConfig, entityClass, queryClass, webapp);
			return 1;
		}	
		return 0;
	}
	

	///////////////////////////////////////
	//
	///////////////////////////////////////
	private BeanDefinition getHibernatePropertiesFor(String dataSourceBeanName, Object source) {

		BeanDefinitionBuilder builder = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.repository.hibernate.support.HibernateDialectType");
		builder.setFactoryMethod("hibernateProperties");
		builder.addConstructorArgReference(dataSourceBeanName);
		AbstractBeanDefinition bean = builder.getRawBeanDefinition();
		bean.setSource(source);
		return bean;
	}
	private BeanDefinition getSQLTemplatesFor(String dataSourceBeanName, Object source) {
		
		BeanDefinitionBuilder builder = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.repository.jdbc.support.SQLTemplatesType");
		builder.setFactoryMethod("sqlTemplates");
		builder.addConstructorArgReference(dataSourceBeanName);
		AbstractBeanDefinition bean = builder.getRawBeanDefinition();
		bean.setSource(source);
		return bean;
	}

	private BeanDefinition getEntityManagerBeanDefinitionFor(String entityManagerFactoryBeanName, Object source) {

		BeanDefinitionBuilder builder = BeanDefinitionBuilder
				.rootBeanDefinition("org.springframework.orm.jpa.SharedEntityManagerCreator");
		builder.setFactoryMethod("createSharedEntityManager");
		builder.addConstructorArgReference(entityManagerFactoryBeanName);
		AbstractBeanDefinition bean = builder.getRawBeanDefinition();
		bean.setSource(source);
		return bean;
	}


	///////////////////////////////////////
	//
	///////////////////////////////////////
	private String getDataSourceRef(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {		
		String dataSourceRef = modulesConfig.getDataSourceRef();
		if(StringUtils.isEmpty(dataSourceRef)) throw new RuntimeException("dataSourceRef is null") ;

		
		String jdbcTemplateRef = dataSourceRef+"JdbcTemplate";
		if(registry.isBeanNameInUse(jdbcTemplateRef)) {registerBeanDefinition(registry, jdbcTemplateRef); return dataSourceRef;}
		
		BeanDefinition jdbcTemplateDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("org.springframework.jdbc.core.JdbcTemplate")
				.addConstructorArgReference(dataSourceRef)
				.getBeanDefinition();
		this.registerBeanDefinition(registry, jdbcTemplateRef, jdbcTemplateDefinition);
		
		return dataSourceRef;
	}
	
	private String getDataSourceTxRef(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {		
	
		String dataSourceRef = getDataSourceRef(registry, modulesConfig);
		String dataSourceTxRef = dataSourceRef+"TransactionManager";
		if(registry.isBeanNameInUse(dataSourceTxRef)) {registerBeanDefinition(registry, dataSourceTxRef); return dataSourceTxRef;}
		
		BeanDefinition dataSourceTxDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("org.springframework.jdbc.datasource.DataSourceTransactionManager")
				.addPropertyReference("dataSource", dataSourceRef)
				.getBeanDefinition();
		this.registerBeanDefinition(registry, dataSourceTxRef, dataSourceTxDefinition);
		
		addTransactionManagementConfiguration(registry, modulesConfig, dataSourceTxRef);
		
		return dataSourceTxRef;
	}
	
	private String getDataSourceTxTemplateRef(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {		

		String dataSourceRef = getDataSourceRef(registry, modulesConfig);
		String dataSourceTxRef = getDataSourceTxRef(registry, modulesConfig);
		String dataSourceTxTemplateRef = dataSourceRef+"TransactionTemplate";
		if(registry.isBeanNameInUse(dataSourceTxTemplateRef)) {registerBeanDefinition(registry, dataSourceTxTemplateRef); return dataSourceTxTemplateRef;}

		BeanDefinition dataSourceTxTemplateRefDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("org.springframework.transaction.support.TransactionTemplate")
				.addConstructorArgReference(dataSourceTxRef)
				.getBeanDefinition();
		this.registerBeanDefinition(registry, dataSourceTxTemplateRef, dataSourceTxTemplateRefDefinition);
		
		return dataSourceTxTemplateRef;
	}

	
	
	///////////////////////////////////////
	//
	///////////////////////////////////////
	private String getSqlSessionFactoryRef(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) throws Exception{

		if(StringUtils.hasText(modulesConfig.getSqlSessionFactoryRef())){
			return modulesConfig.getSqlSessionFactoryRef();
			
		}else {
			String dataSourceRef = getDataSourceRef(registry, modulesConfig);
			String sqlSessionFactoryRef = dataSourceRef+"Mybatis";
			
			////////////////////////////////
			String sqlSessionFactoryMapperLocationsRef = sqlSessionFactoryRef+"MapperLocations";
			String[] sqlSessionFactoryMapperLocationsRefPackagesToScan = new String[]{modulesConfig.getBasePackage()};
			if(registry.isBeanNameInUse(sqlSessionFactoryMapperLocationsRef)) {
				BeanDefinition savedBean = registry.getBeanDefinition(sqlSessionFactoryMapperLocationsRef);
				String[] savedValue = (String[]) savedBean.getPropertyValues().getPropertyValue("packagesToScan").getValue();
				
				if(savedValue != null){
					
					String[] newValue = new String[savedValue.length+sqlSessionFactoryMapperLocationsRefPackagesToScan.length];
					for(int i = 0; i < savedValue.length; i++){
						newValue[i] = savedValue[i];
					}
					for(int i = 0; i < sqlSessionFactoryMapperLocationsRefPackagesToScan.length; i++){
						newValue[i + savedValue.length] = sqlSessionFactoryMapperLocationsRefPackagesToScan[i];
					}
					sqlSessionFactoryMapperLocationsRefPackagesToScan = newValue;

				}
				registry.removeBeanDefinition(sqlSessionFactoryMapperLocationsRef);
			}

			BeanDefinition sqlSessionFactoryMapperLocationsRefBeanDefinition = BeanDefinitionBuilder
					.rootBeanDefinition("com.u2ware.springfield.support.resource.ResourcePatternResolverBean")
					.addPropertyValue("packagesToScan", sqlSessionFactoryMapperLocationsRefPackagesToScan)
					.addPropertyValue("resourcePatterns", new String[]{"/**/*.sqlsession.xml"})
					.getRawBeanDefinition();
			this.registerBeanDefinition(registry, sqlSessionFactoryMapperLocationsRef, sqlSessionFactoryMapperLocationsRefBeanDefinition);


			////////////////////////////////
			String sqlSessionFactoryConfigLocationRef = sqlSessionFactoryRef+"ConfigLocation";
			String[] sqlSessionFactoryConfigLocationRefPackagesToScan = new String[]{modulesConfig.getBasePackage()};
			if(registry.isBeanNameInUse(sqlSessionFactoryConfigLocationRef)) {
				BeanDefinition savedBean = registry.getBeanDefinition(sqlSessionFactoryConfigLocationRef);
				String[] savedValue = (String[]) savedBean.getPropertyValues().getPropertyValue("packagesToScan").getValue();
				if(savedValue != null){
					
					String[] newValue = new String[savedValue.length+sqlSessionFactoryConfigLocationRefPackagesToScan.length];
					for(int i = 0; i < savedValue.length; i++){
						newValue[i] = savedValue[i];
					}
					for(int i = 0; i < sqlSessionFactoryConfigLocationRefPackagesToScan.length; i++){
						newValue[i + savedValue.length] = sqlSessionFactoryConfigLocationRefPackagesToScan[i];
					}
					sqlSessionFactoryConfigLocationRefPackagesToScan = newValue;
				}
				registry.removeBeanDefinition(sqlSessionFactoryConfigLocationRef);
			}
			BeanDefinition sqlSessionFactoryConfigLocationRefBeanDefinition = BeanDefinitionBuilder
					.rootBeanDefinition("com.u2ware.springfield.support.resource.ResourcePatternResolverBean")
					.addPropertyValue("packagesToScan", sqlSessionFactoryConfigLocationRefPackagesToScan)
					.addPropertyValue("resourcePatterns", new String[]{"/**/*.sqlconfig.xml"})
					.getRawBeanDefinition();
			this.registerBeanDefinition(registry, sqlSessionFactoryConfigLocationRef, sqlSessionFactoryConfigLocationRefBeanDefinition);
			
			
			////////////////////////////////
			if(registry.isBeanNameInUse(sqlSessionFactoryRef)) { return sqlSessionFactoryRef;}
			BeanDefinition beanDefinition = BeanDefinitionBuilder
					.rootBeanDefinition("org.mybatis.spring.SqlSessionFactoryBean")
					.addPropertyReference("dataSource", dataSourceRef)
					.addPropertyValue("mapperLocations", "#{"+sqlSessionFactoryMapperLocationsRef+".resources}")
					.addPropertyValue("configLocation", "#{"+sqlSessionFactoryConfigLocationRef+".resources}")
					.getBeanDefinition();

			this.registerBeanDefinition(registry, sqlSessionFactoryRef, beanDefinition);
			return sqlSessionFactoryRef;
		}
	}
	
	
	///////////////////////////////////////
	//
	///////////////////////////////////////
	private String getSessionFactoryRef(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {
		
		if(StringUtils.hasText(modulesConfig.getSessionFactoryRef())){
			return modulesConfig.getSessionFactoryRef();

		}else{
			String dataSourceRef =  getDataSourceRef(registry, modulesConfig);
			String sessionFactoryRef = dataSourceRef+"Hibernate";
			
			String[] packages = new String[]{modulesConfig.getBasePackage()};
			//logger.trace("add packages : "+StringUtils.arrayToCommaDelimitedString(packages));
			
			if(registry.isBeanNameInUse(sessionFactoryRef)) {

				BeanDefinition savedBean = registry.getBeanDefinition(sessionFactoryRef);

				String[] savedPackages = (String[])
						savedBean.getPropertyValues().getPropertyValue("packagesToScan").getValue();
				//logger.trace("saved packages : "+StringUtils.arrayToCommaDelimitedString(savedPackages));

				if(savedPackages != null){
					
					String[] newPackage = new String[packages.length+savedPackages.length];
					
					for(int i = 0; i < savedPackages.length; i++){
						newPackage[i] = savedPackages[i];
					}
					for(int i = 0; i < packages.length; i++){
						newPackage[i + savedPackages.length] = packages[i];
					}
					packages = newPackage;
				}
				registry.removeBeanDefinition(sessionFactoryRef);
			}
			//logger.trace("new packages : "+StringUtils.arrayToCommaDelimitedString(packages));

			BeanDefinition beanDefinition = BeanDefinitionBuilder
					.rootBeanDefinition("org.springframework.orm.hibernate3.annotation.AnnotationSessionFactoryBean")
					.addPropertyReference("dataSource", dataSourceRef)
					.addPropertyValue("packagesToScan", packages)
					.addPropertyValue("annotatedPackages", packages)
					.addPropertyValue("hibernateProperties", getHibernatePropertiesFor(dataSourceRef, modulesConfig.getSource()))
					.getBeanDefinition();
			
			this.registerBeanDefinition(registry, sessionFactoryRef, beanDefinition);
			return sessionFactoryRef;
		}
	}
	
	private String getSessionFactoryTxRef(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {

		String sessionFactoryRef = getSessionFactoryRef(registry, modulesConfig);
		String sessionFactoryTxRef = sessionFactoryRef+"TransactionManager";
		if(registry.isBeanNameInUse(sessionFactoryTxRef)) {registerBeanDefinition(registry, sessionFactoryTxRef); return sessionFactoryTxRef;}
		
		BeanDefinition sessionFactoryTxDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("org.springframework.orm.hibernate3.HibernateTransactionManager")
				.addPropertyReference("sessionFactory", sessionFactoryRef)
				.getBeanDefinition();
		
		this.registerBeanDefinition(registry, sessionFactoryTxRef, sessionFactoryTxDefinition);
		
		addTransactionManagementConfiguration(registry, modulesConfig, sessionFactoryTxRef);

		return sessionFactoryTxRef;
	}
	
	private String getSessionFactoryTxTemplateRef(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {

		String sessionFactoryRef = getSessionFactoryRef(registry, modulesConfig);
		String sessionFactoryTxRef = getSessionFactoryTxRef(registry, modulesConfig);
		String sessionFactoryTxTemplateRef = sessionFactoryRef+"TransactionTemplate";
		if(registry.isBeanNameInUse(sessionFactoryTxTemplateRef)) {registerBeanDefinition(registry, sessionFactoryTxTemplateRef); return sessionFactoryTxTemplateRef;}

		BeanDefinition sessionFactoryTxTemplateDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("org.springframework.transaction.support.TransactionTemplate")
				.addConstructorArgReference(sessionFactoryTxRef)
				.getBeanDefinition();
		this.registerBeanDefinition(registry, sessionFactoryTxTemplateRef, sessionFactoryTxTemplateDefinition);
		
		return sessionFactoryTxTemplateRef;
	}
	
	
	///////////////////////////////////////
	//
	///////////////////////////////////////
	private String getEntityManagerFactoryRef(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) throws Exception{
		
		if(StringUtils.hasText(modulesConfig.getEntityManagerFactoryRef())){
			return modulesConfig.getEntityManagerFactoryRef();
		}else{
			String dataSourceRef =  getDataSourceRef(registry, modulesConfig);
			String entityManagerFactoryRef = dataSourceRef+"Jpa";


			String[] packages = new String[]{modulesConfig.getBasePackage()};
			//logger.trace("add packages : "+StringUtils.arrayToCommaDelimitedString(packages));
			
			if(registry.isBeanNameInUse(entityManagerFactoryRef)) {
		
				BeanDefinition savedBean = registry.getBeanDefinition(entityManagerFactoryRef);
				String[] savedPackages = (String[])
						(savedBean.getPropertyValues().getPropertyValue("packagesToScan").getValue());
				//logger.trace("saved packages : "+StringUtils.arrayToCommaDelimitedString(savedPackages));

				if(savedPackages != null){
					
					String[] newPackage = new String[packages.length+savedPackages.length];
					
					for(int i = 0; i < savedPackages.length; i++){
						newPackage[i] = savedPackages[i];
					}
					for(int i = 0; i < packages.length; i++){
						newPackage[i + savedPackages.length] = packages[i];
					}
					packages = newPackage;
				}
				registry.removeBeanDefinition(entityManagerFactoryRef);
			}
			//logger.trace("new packages : "+StringUtils.arrayToCommaDelimitedString(packages));
			
			BeanDefinition beanDefinition = BeanDefinitionBuilder
					.rootBeanDefinition("org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean")
					.addPropertyReference("dataSource", dataSourceRef)
					.addPropertyValue("persistenceUnitName", entityManagerFactoryRef)
					.addPropertyValue("persistenceProviderClass", HibernatePersistence.class)
					.addPropertyValue("packagesToScan", packages)
					.addPropertyValue("jpaProperties", getHibernatePropertiesFor(dataSourceRef, modulesConfig.getSource()))
					.getBeanDefinition();
			
			this.registerBeanDefinition(registry, entityManagerFactoryRef, beanDefinition);
			return entityManagerFactoryRef;
		}
	}
	
	private String getEntityManagerFactoryTxRef(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) throws Exception{
		
		String emfRef = getEntityManagerFactoryRef(registry, modulesConfig);
		String emfTxRef = emfRef+"TransactionManager";
		if(registry.isBeanNameInUse(emfTxRef)) {registerBeanDefinition(registry, emfTxRef); return emfTxRef;}
	
		BeanDefinition emfTxDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("org.springframework.orm.jpa.JpaTransactionManager")
				.addPropertyReference("entityManagerFactory", emfRef)
				.getBeanDefinition();
		
		this.registerBeanDefinition(registry, emfTxRef, emfTxDefinition);

		addTransactionManagementConfiguration(registry, modulesConfig, emfTxRef);
		
		return emfTxRef;
	}
	
	
	private String getEntityManagerFactoryTxTemplateRef(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) throws Exception{
	
		String emfRef = getEntityManagerFactoryRef(registry, modulesConfig);
		String emfTxRef = getEntityManagerFactoryTxRef(registry, modulesConfig);
		String emfTxTemplateRef = emfRef+"TransactionTemplate";
		if(registry.isBeanNameInUse(emfTxTemplateRef)) {registerBeanDefinition(registry, emfTxTemplateRef); return emfTxTemplateRef;}

		
		BeanDefinition emfTxTemplateDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("org.springframework.transaction.support.TransactionTemplate")
				.addConstructorArgReference(emfTxRef)
				.getBeanDefinition();
		this.registerBeanDefinition(registry, emfTxTemplateRef, emfTxTemplateDefinition);
		
		return emfTxTemplateRef;
	}
	
	
	///////////////////////////////////////////////
	//
	///////////////////////////////////////////////
	private void addTransactionManagementConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, String annotationDrivenTransactionManagerRef) {
		String beanName = "transactionManagementConfiguration";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return;}

		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.config.support.TransactionManagementConfiguration")
				.addPropertyReference("annotationDrivenTransactionManager", annotationDrivenTransactionManagerRef)
				.getRawBeanDefinition();
				
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	
	///////////////////////////////////////////////
	//
	///////////////////////////////////////////////
	private void addJdbcRepositoryConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) {
		
		getDataSourceTxTemplateRef(registry, modulesConfig);

		String beanName = ClassUtils.getShortNameAsProperty(entityClass)+"Repository";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}
		
		String dataSourceRef = modulesConfig.getDataSourceRef();
		String jdbcTemplateRef = dataSourceRef+"JdbcTemplate";
		
		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.repository.jdbc.JdbcRepository")
				.addConstructorArgValue(entityClass)
				.addConstructorArgReference(jdbcTemplateRef)
				.addPropertyValue("dialect", getSQLTemplatesFor(dataSourceRef, modulesConfig.getSource()))
				.getBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	

	private void addSqlSessionRepositoryConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) throws Exception{

		getDataSourceTxTemplateRef(registry, modulesConfig);

		String beanName = ClassUtils.getShortNameAsProperty(entityClass)+"Repository";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}
	
		String sqlSessionFactoryRef = getSqlSessionFactoryRef(registry, modulesConfig);
		
		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.repository.sqlsession.SqlSessionRepository")
				.addConstructorArgValue(entityClass)
				.addConstructorArgReference(sqlSessionFactoryRef)
				.getBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}


	private void addHibernateRepositoryConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) {

		getSessionFactoryTxTemplateRef(registry, modulesConfig);
		
		String beanName = ClassUtils.getShortNameAsProperty(entityClass)+"Repository";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}

		String sessionFactoryRef = getSessionFactoryRef(registry, modulesConfig);
		
		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.repository.hibernate.HibernateRepository")
				.addConstructorArgValue(entityClass)
				.addConstructorArgReference(sessionFactoryRef)
				.getBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	
	private void addJpaRepositoryConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) throws Exception{

		getEntityManagerFactoryTxTemplateRef(registry, modulesConfig);

		String beanName = ClassUtils.getShortNameAsProperty(entityClass)+"Repository";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}

		getEntityManagerFactoryTxRef(registry, modulesConfig);
		String entityManagerFactoryRef = getEntityManagerFactoryRef(registry, modulesConfig);
		BeanDefinition entityManagerFactoryValue = getEntityManagerBeanDefinitionFor(entityManagerFactoryRef, modulesConfig.getSource());
		
		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.repository.jpa.JpaRepository")
				.addConstructorArgValue(entityClass)
				.addConstructorArgValue(entityManagerFactoryValue)
				.getBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	
	
	////////////////////////////////////////////////////////////////////////
	//
	////////////////////////////////////////////////////////////////////////
	private void addDummyServiceConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) throws Exception{
		String beanName = ClassUtils.getShortNameAsProperty(queryClass)+"Service";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}
		
		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.service.EntityServiceImpl")
				.getBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	
	private void addJdbcServiceConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) throws Exception{

		String beanName = ClassUtils.getShortNameAsProperty(queryClass)+"Service";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}

		String repositoryRef = ClassUtils.getShortNameAsProperty(entityClass)+"Repository";
		String txTemplateRef = getDataSourceTxTemplateRef(registry, modulesConfig);
		
		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.service.EntityServiceImpl")
				.addConstructorArgReference(txTemplateRef)
				.addConstructorArgReference(repositoryRef)
				.getBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	
	private void addSqlSessionServiceConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) throws Exception{
		
		String beanName = ClassUtils.getShortNameAsProperty(queryClass)+"Service";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}

		String repositoryRef = ClassUtils.getShortNameAsProperty(entityClass)+"Repository";
		String txTemplateRef = getDataSourceTxTemplateRef(registry, modulesConfig);
		
		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.service.EntityServiceImpl")
				.addConstructorArgReference(txTemplateRef)
				.addConstructorArgReference(repositoryRef)
				.getBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}

	private void addHibernateServiceConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) throws Exception{

		String beanName = ClassUtils.getShortNameAsProperty(queryClass)+"Service";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}

		String repositoryRef = ClassUtils.getShortNameAsProperty(entityClass)+"Repository";
		String txTemplateRef = getSessionFactoryTxTemplateRef(registry, modulesConfig);
		
		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.service.EntityServiceImpl")
				.addConstructorArgReference(txTemplateRef)
				.addConstructorArgReference(repositoryRef)
				.getBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}


	private void addJpaServiceConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig, Class<?> entityClass, Class<?> queryClass, Springfield webapp) throws Exception{


		String beanName = ClassUtils.getShortNameAsProperty(queryClass)+"Service";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return ;}

		String repositoryRef = ClassUtils.getShortNameAsProperty(entityClass)+"Repository";
		String txTemplateRef = getEntityManagerFactoryTxTemplateRef(registry, modulesConfig);
		
		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.service.EntityServiceImpl")
				.addConstructorArgReference(txTemplateRef)
				.addConstructorArgReference(repositoryRef)
				.getBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
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
	
	
	private void addSpringfieldBaseConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {
		String beanName = "springfieldBaseConfiguration";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return;}
		
		BeanDefinitionBuilder builder = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.config.support.BaseConfiguration");
		
		if(StringUtils.hasText(modulesConfig.getPropertiesRef())){
			builder.addPropertyReference("properties", modulesConfig.getPropertiesRef());
		}
		
		
		BeanDefinition beanDefinition = builder.getRawBeanDefinition();
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	private void addSpringfieldBaseMultipartConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {
		
		String beanName = "springfieldBaseMultipartConfiguration";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return;}
		
		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.config.support.BaseMultipartConfiguration")
				.getRawBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	
	private void addSpringfieldBaseMessageConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {
		
		String beanName = "springfieldBaseMessageConfiguration";
		String[] packagesToScan = new String[]{modulesConfig.getBasePackage()};

		if(registry.isBeanNameInUse(beanName)) {
			BeanDefinition savedBean = registry.getBeanDefinition(beanName);
			String[] savedValue = (String[])savedBean.getPropertyValues().getPropertyValue("packagesToScan").getValue();

			if(savedValue != null){
				String[] newValue = new String[savedValue.length+packagesToScan.length];

				for(int i = 0; i < savedValue.length; i++){
					newValue[i] = savedValue[i];
				}
				for(int i = 0; i < packagesToScan.length; i++){
					newValue[i + savedValue.length] = packagesToScan[i];
				}
				
				packagesToScan = newValue;
			}
			
			
			registry.removeBeanDefinition(beanName);
		}

		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.config.support.BaseMessageConfiguration")
				.addPropertyValue("packagesToScan", packagesToScan)
				.getRawBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	
	///////////////////////////////////////
	//
	///////////////////////////////////////
	private void addSpringfieldWebmvcConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {
		
		String beanName = "springfieldWebmvcConfiguration";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return;}
		
		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.config.support.WebmvcConfiguration")
				.getRawBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	private void addSpringfieldWebmvcSecurityConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {
		
		String beanName = "springfieldWebmvcSecurityConfiguration";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return;}
		
		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.config.support.WebmvcSecurityConfiguration")
				.addPropertyValue("basePackage", modulesConfig.getBasePackage())
				.getRawBeanDefinition();
		
		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
	private void addSpringfieldWebmvcRenderConfiguration(BeanDefinitionRegistry registry, ModulesConfig modulesConfig) {
		String beanName = "springfieldWebmvcRenderConfiguration";
		if(registry.isBeanNameInUse(beanName)) {registerBeanDefinition(registry, beanName); return;}
				
		BeanDefinition beanDefinition = BeanDefinitionBuilder
				.rootBeanDefinition("com.u2ware.springfield.config.support.WebmvcRenderConfiguration")
				.addPropertyValue("basePackage", modulesConfig.getBasePackage())
				.getRawBeanDefinition();

		this.registerBeanDefinition(registry, beanName, beanDefinition);
	}
}
	



