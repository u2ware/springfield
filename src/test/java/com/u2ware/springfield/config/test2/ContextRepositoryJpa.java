package com.u2ware.springfield.config.test2;

import java.util.Properties;

import javax.sql.DataSource;

import org.hibernate.ejb.HibernatePersistence;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;

import com.u2ware.springfield.repository.hibernate.support.HibernateDialectType;


//@Configuration
//@EnableTransactionManagement
public class ContextRepositoryJpa {

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	protected String basePackage;
	protected DataSource dataSource;

	public void setDataSource(DataSource dataSource) {
		this.dataSource = dataSource;
	}
	public void setBasePackage(String basePackage) {
		this.basePackage = basePackage;
	}

	public LocalContainerEntityManagerFactoryBean springfieldEntityManagerFactory() throws Exception{
		logger.trace("create LocalContainerEntityManagerFactoryBean "+basePackage);
		String dialect = HibernateDialectType.fromMetaData(dataSource).getDialect(); 
		
		Properties p = new Properties();
		p.put("hibernate.cache.provider_class",  "org.hibernate.cache.HashtableCacheProvider");
		p.put("hibernate.dialect" , dialect);
		p.put("hibernate.show_sql", "true");
		p.put("hibernate.format_sql", "true");
		p.put("hibernate.hbm2ddl.auto", "update");
		
		LocalContainerEntityManagerFactoryBean b = new LocalContainerEntityManagerFactoryBean();
		b.setPersistenceUnitName("springfield");
		b.setPersistenceProviderClass(HibernatePersistence.class);
		b.setJpaProperties(p);

		b.setDataSource(dataSource);
		b.setPackagesToScan(basePackage);
		//b.afterPropertiesSet();
		
		return b;
	}
	
	public JpaTransactionManager springfieldEntityManagerFactoryTx() throws Exception{
		logger.trace("create JpaTransactionManager");
		JpaTransactionManager b = new JpaTransactionManager();
		b.setEntityManagerFactory(springfieldEntityManagerFactory().getObject());
		return b;
	}

}
