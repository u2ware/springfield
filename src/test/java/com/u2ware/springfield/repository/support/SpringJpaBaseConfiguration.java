package com.u2ware.springfield.repository.support;

import java.util.Properties;

import javax.persistence.EntityManagerFactory;
import javax.sql.DataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabase;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.persistenceunit.DefaultPersistenceUnitManager;
import org.springframework.orm.jpa.persistenceunit.PersistenceUnitManager;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;


@Configuration
@EnableTransactionManagement
public abstract class SpringJpaBaseConfiguration {

	protected Log logger = LogFactory.getLog(getClass());

	//public static final String REPOSITORY_CONFIGURATION_CLASS = RepositoryConfiguration.class.getName();//"com.u2ware.springfield.config.RepositoryConfiguration";
	//public static final String CLASS_NAME = "com.u2ware.springfield.config.support.SpringDataJpaConfiguration";
	//public static final String CLASS_NAME = BaseConfiguration.class.getName();
	//public static final String DATA_SOURCE              = CLASS_NAME + "#dataSource";
	//public static final String PERSISTENCE_UNIT_MANAGER = CLASS_NAME + "#persistenceUnitManager";
	//public static final String ENTITY_MANAGER_FACTORY   = CLASS_NAME + "#entityManagerFactory";
	//public static final String TRANSACTION_MANAGER      = CLASS_NAME + "#transactionManager";
	//public static final String SQLSESSION_FACTORY       = CLASS_NAME + "#sqlSessionFactory";
	//public static final String SQLSESSION_TEMPLATE      = CLASS_NAME + "#sqlSessionTemplate";

	protected String[] packagesToScans;
	protected DataSource defaultDataSource;
	protected Properties jpaProperties;

	public SpringJpaBaseConfiguration(Class<?> basePackageClass){
		this(ClassUtils.getPackageName(basePackageClass));
	}
	public SpringJpaBaseConfiguration(String basePackage){
		this(StringUtils.delimitedListToStringArray(basePackage, ",", " "));
	}
	public SpringJpaBaseConfiguration(String[] packagesToScans){
		this.packagesToScans = packagesToScans;
	}
	
	public void setDefaultDataSource(DataSource defaultDataSource) {
		this.defaultDataSource = defaultDataSource;
	}
	public void setJpaProperties(Properties jpaProperties) {
		this.jpaProperties = jpaProperties;
	}

	protected DataSource getDefaultDataSource(){
		if(defaultDataSource != null){
			return defaultDataSource;
		}else{
			return dataSource();
		}
	}
	
	@Bean
	public DataSource dataSource(){		
		DataSource ds = new EmbeddedDatabaseBuilder().setType(EmbeddedDatabaseType.HSQL).build();
		return ds;
	}

	@Bean
	public PersistenceUnitManager persistenceUnitManager(){
		
		DefaultPersistenceUnitManager pum = new DefaultPersistenceUnitManager();
		pum.setPackagesToScan(packagesToScans);
		pum.setDefaultDataSource(getDefaultDataSource());
		return pum;
	}

	@Bean
	public EntityManagerFactory entityManagerFactory(){
		
		HibernateJpaVendorAdapter adaptor = new HibernateJpaVendorAdapter();
		if(getDefaultDataSource() instanceof EmbeddedDatabase){
			adaptor.setGenerateDdl(true);
			adaptor.setShowSql(true);
		}
		
		LocalContainerEntityManagerFactoryBean emf = new LocalContainerEntityManagerFactoryBean();
		emf.setPersistenceUnitManager(persistenceUnitManager());
		emf.setJpaVendorAdapter(adaptor);
		if(jpaProperties != null){
			emf.setJpaProperties(jpaProperties);
		}
		emf.afterPropertiesSet();
		
		return emf.getObject();
	}
	@Bean
	public PlatformTransactionManager transactionManager(EntityManagerFactory factory){
		return new JpaTransactionManager(factory);
	}

	
	//////////////////////////
	// SqlSession
	//////////////////////////
	/*
	@Bean(name=SQLSESSION_FACTORY)
	public SqlSessionFactoryBean sqlSessionFactory(){
		SqlSessionFactoryBean bean = new SqlSessionFactoryBean();
		bean.setDataSource(getDefaultDataSource());
		//bean.setConfigLocation(configLocation);
		//bean.setMapperLocations(mapperLocations);
		return bean;
	}
	@Bean(name=SQLSESSION_TEMPLATE)
	public SqlSessionTemplate sqlSessionTemplate(SqlSessionFactory sf){
		return new SqlSessionTemplate(sf);
	}
	*/
}
