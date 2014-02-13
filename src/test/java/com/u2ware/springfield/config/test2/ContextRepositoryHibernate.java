package com.u2ware.springfield.config.test2;

import java.util.Properties;

import javax.sql.DataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.orm.hibernate3.HibernateTransactionManager;
import org.springframework.orm.hibernate3.annotation.AnnotationSessionFactoryBean;

import com.u2ware.springfield.repository.hibernate.support.HibernateDialectType;

//@Configuration
//@EnableTransactionManagement
public class ContextRepositoryHibernate {

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	protected String basePackage;
	protected DataSource dataSource;

	public void setDataSource(DataSource dataSource) {
		this.dataSource = dataSource;
	}
	public void setBasePackage(String basePackage) {
		this.basePackage = basePackage;
	}

	public AnnotationSessionFactoryBean springfieldSessionFactory()  throws Exception{
		logger.trace("SessionFactoryConfiguration");
		logger.info("create AnnotationSessionFactoryBean");
		
		String dialect = HibernateDialectType.fromMetaData(dataSource).getDialect(); 

		Properties p = new Properties();
		p.put("hibernate.cache.provider_class",  "org.hibernate.cache.HashtableCacheProvider");
		p.put("hibernate.dialect" ,  dialect);
		p.put("hibernate.show_sql", "true");
		p.put("hibernate.format_sql", "true");
		p.put("hibernate.hbm2ddl.auto", "update");
		p.put("hibernate.connection.release_mode", "after_transaction");
		//p.put("hibernate.current_session_context_class" value="thread");
		
		AnnotationSessionFactoryBean sessionFactory = new AnnotationSessionFactoryBean();
		sessionFactory.setDataSource(dataSource);
		sessionFactory.setPackagesToScan(new String[]{basePackage});
		sessionFactory.setAnnotatedPackages(new String[]{basePackage});
		sessionFactory.setHibernateProperties(p);
		sessionFactory.afterPropertiesSet();
		return sessionFactory;
	}
	
	public HibernateTransactionManager springfieldSessionFactoryTx() throws Exception{
		logger.info("create HibernateTransactionManager");

		HibernateTransactionManager tx = new HibernateTransactionManager();
		tx.setSessionFactory(springfieldSessionFactory().getObject());
		return tx;
	}

}
