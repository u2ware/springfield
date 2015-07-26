package com.u2ware.springfield.config;

import java.util.HashSet;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.env.Environment;
import org.springframework.core.io.ResourceLoader;
import org.springframework.core.type.filter.AnnotationTypeFilter;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;

import com.u2ware.springfield.controller.GenericControllerImpl;
import com.u2ware.springfield.repository.GenericRepositoryImpl;
import com.u2ware.springfield.service.GenericServiceImpl;

public class GenericMvcBeanDefinitionConfiguration {

	protected final Log logger = LogFactory.getLog(getClass());
	
	public void registerBeanDefinitions(BeanDefinitionRegistry r, GenericMvcBeanDefinitionConfigurationSource s) throws Exception{

		
		String name = "com_u2ware_springfield_config";
		BeanDefinitionBuilder bean = BeanDefinitionBuilder
				.rootBeanDefinition(GenericMvcConfiguration.class)
				.addConstructorArgValue(s.getBasePackages())
				.addConstructorArgReference(s.getEntityManagerFactoryRef())
				.addConstructorArgReference(s.getTransactionManagerRef());
		
		r.registerBeanDefinition(name, bean.getBeanDefinition());
		
		////////////////////////////
		//
		////////////////////////////
		HashSet<BeanDefinition> mvcBeans = new HashSet<BeanDefinition>();

		ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(false);
		scanner.setResourceLoader(s.getResourceLoader());
		scanner.addIncludeFilter(new AnnotationTypeFilter(GenericMvc.class));
		scanner.addIncludeFilter(new AnnotationTypeFilter(GenericMvc.Controller.class));
		scanner.addIncludeFilter(new AnnotationTypeFilter(GenericMvc.Service.class));
		scanner.addIncludeFilter(new AnnotationTypeFilter(GenericMvc.Repository.class));
		//scanner.addIncludeFilter(new AssignableTypeFilter(type));
		for(String basePackage : s.getBasePackages()){
			mvcBeans.addAll(scanner.findCandidateComponents(basePackage));
		}		
		
		@SuppressWarnings("unused")
		RuntimeBeanReference repository, service, controller = null;
		for(BeanDefinition mvcBean : mvcBeans){
			
			Class<?> mvcClass = Class.forName(mvcBean.getBeanClassName());
			logger.debug(mvcClass);
			
			GenericMvcConfig c = new GenericMvcConfig(s, mvcClass);
			repository = registerRepository(r, c);
			   service =    registerService(r, c, repository);
			controller = registerController(r, c, service);

			logger.debug("");
		}
	}
	
	protected RuntimeBeanReference registerRepository(BeanDefinitionRegistry r, GenericMvcConfig c) throws Exception{
		
		if(! c.hasRepositoryComponent()) return null;

		String name = "com_u2ware_springfield_repository_"+ClassUtils.getShortNameAsProperty(c.getEntityClass())+"Repository";
		BeanDefinitionBuilder bean = BeanDefinitionBuilder
				.rootBeanDefinition(GenericRepositoryImpl.class)
				.addConstructorArgValue(c.getEntityClass())
				.addConstructorArgValue(getEntityManagerBeanDefinitionFor(c));

		r.registerBeanDefinition(name, bean.getBeanDefinition());
		logger.debug(name);
		
		return new RuntimeBeanReference(name);
	}
	
	
	protected RuntimeBeanReference registerService(BeanDefinitionRegistry r, GenericMvcConfig c, RuntimeBeanReference repository) throws Exception{

		if(! c.hasServiceComponent()) return null;

		String name = "com_u2ware_springfield_service_"+ClassUtils.getShortNameAsProperty(c.getDomainClass())+"Service";
		BeanDefinitionBuilder bean = BeanDefinitionBuilder
				.rootBeanDefinition(GenericServiceImpl.class)
				.addPropertyValue("domainClass", c.getDomainClass())
				.addPropertyValue("repository", repository)
				.addPropertyReference("modelMapper", "com_u2ware_springfield_config_ModelMapper");
		
		r.registerBeanDefinition(name, bean.getBeanDefinition());
		
		logger.debug(name);

		return new RuntimeBeanReference(name);
	}
	
	protected RuntimeBeanReference registerController(BeanDefinitionRegistry r, GenericMvcConfig c, RuntimeBeanReference service) throws Exception{

		if(! c.hasControllerComponent()) return null;

		String rootExpression  = "#{<bean>.getRequestMappingRootPatternValue(T(<class>),'<value>')}";
		rootExpression = StringUtils.replace(rootExpression, "<bean>", "com_u2ware_springfield_config_RequestMappingPatternResolverBean");
		rootExpression = StringUtils.replace(rootExpression, "<class>", c.getDomainClass().getName());
		rootExpression = StringUtils.replace(rootExpression, "<value>", c.getRequestMappingRootPatternValue());

		String uniqueExpression  = "#{<bean>.getRequestMappingUniquePatternValue(T(<class>),'<value>')}";
		uniqueExpression = StringUtils.replace(uniqueExpression, "<bean>", "com_u2ware_springfield_config_RequestMappingPatternResolverBean");
		uniqueExpression = StringUtils.replace(uniqueExpression, "<class>", c.getEntityClass().getName());
		uniqueExpression = StringUtils.replace(uniqueExpression, "<value>", c.getRequestMappingUniquePatternValue());
		
		
		String name = "com_u2ware_springfield_controller_"+ClassUtils.getShortNameAsProperty(c.getDomainClass())+"Controller";
		BeanDefinitionBuilder bean = BeanDefinitionBuilder
				.rootBeanDefinition(GenericControllerImpl.class)
				.addPropertyValue("domainClass", c.getDomainClass())
				.addPropertyValue("service", service)
				.addPropertyValue("requestMappingRootPatternValue", rootExpression)
				.addPropertyValue("requestMappingUniquePatternValue", uniqueExpression)
				.addPropertyReference("modelMapper", "com_u2ware_springfield_config_ModelMapper");
				
		r.registerBeanDefinition(name, bean.getBeanDefinition());

		//logger.debug(name+"="+GenericControllerFactoryBean.class);

		return new RuntimeBeanReference(name);
	}

	private AbstractBeanDefinition getEntityManagerBeanDefinitionFor(GenericMvcBeanDefinitionConfigurationSource c) {
		BeanDefinitionBuilder builder = BeanDefinitionBuilder
				.rootBeanDefinition("org.springframework.orm.jpa.SharedEntityManagerCreator");
		builder.setFactoryMethod("createSharedEntityManager");
		builder.addConstructorArgReference(c.getEntityManagerFactoryRef());

		AbstractBeanDefinition bean = builder.getRawBeanDefinition();
		bean.setSource(c.getSource());
		return bean;
	}
	

	/////////////////////////
	//
	/////////////////////////
	private static class GenericMvcConfig extends GenericMvcBeanDefinitionConfigurationSource{

		private GenericMvcBeanDefinitionConfigurationSource source;
		private Class<?> domainClass;
		private Class<?> entityClass;

		private String requestMappingRootPatternValue;
		private String requestMappingUniquePatternValue;
		
		private boolean hasRepositoryComponent;
		private boolean hasServiceComponent;
		private boolean hasControllerComponent;
		
		private GenericMvcConfig(GenericMvcBeanDefinitionConfigurationSource source, Class<?> domainClass){
			this.source = source;
			this.domainClass = domainClass;
			
			GenericMvc.Repository repository = AnnotationUtils.getAnnotation(domainClass, GenericMvc.Repository.class);
			if(repository != null){
				this.entityClass = repository.value().equals(Class.class) ? domainClass : repository.value();
				this.hasRepositoryComponent = true;
				this.hasServiceComponent = false;
				this.hasControllerComponent = false;
			}

			GenericMvc.Service service = AnnotationUtils.getAnnotation(domainClass, GenericMvc.Service.class);
			if(service != null){
				this.entityClass = service.value().equals(Class.class) ? domainClass : service.value();
				this.hasRepositoryComponent = true;
				this.hasServiceComponent = true;
				this.hasControllerComponent = false;
			}

			GenericMvc.Controller controller = AnnotationUtils.getAnnotation(domainClass, GenericMvc.Controller.class);
			if(controller != null){
				this.entityClass = controller.value().equals(Class.class) ? domainClass : controller.value();
				this.hasRepositoryComponent = true;
				this.hasServiceComponent = false;
				this.hasControllerComponent = true;
				this.requestMappingRootPatternValue = controller.requestMappingRootPatternValue();
				this.requestMappingUniquePatternValue = controller.requestMappingUniquePatternValue();
			}
			
			GenericMvc mvc = AnnotationUtils.getAnnotation(domainClass, GenericMvc.class);
			if(mvc != null){
				this.entityClass = mvc.value().equals(Class.class) ? domainClass : mvc.value();
				this.hasRepositoryComponent = true;
				this.hasServiceComponent = true;
				this.hasControllerComponent = true;
				this.requestMappingRootPatternValue = mvc.requestMappingRootPatternValue();
				this.requestMappingUniquePatternValue = mvc.requestMappingUniquePatternValue();
			}
		}

		public Class<?> getDomainClass() {
			return domainClass;
		}
		public Class<?> getEntityClass() {
			return entityClass;
		}
		
		public String getRequestMappingRootPatternValue(){
			return requestMappingRootPatternValue;
		}
		public String getRequestMappingUniquePatternValue(){
			return requestMappingUniquePatternValue;
		}
		
		@Override
		public Iterable<String> getBasePackages() {
			return source.getBasePackages();
		}

		@Override
		public ResourceLoader getResourceLoader() {
			return source.getResourceLoader();
		}

		@Override
		public Environment getEnvironment() {
			return source.getEnvironment();
		}
		@Override
		public Object getSource() {
			return source.getSource();
		}

		@Override
		public String getEntityManagerFactoryRef() {
			return source.getEntityManagerFactoryRef();
		}

		@Override
		public String getTransactionManagerRef() {
			return source.getTransactionManagerRef();
		}
		
		public boolean hasRepositoryComponent(){
			return hasRepositoryComponent;
		}
		public boolean hasServiceComponent(){
			return hasServiceComponent;
		}
		public boolean hasControllerComponent(){
			return hasControllerComponent;
		}	
	}
}
