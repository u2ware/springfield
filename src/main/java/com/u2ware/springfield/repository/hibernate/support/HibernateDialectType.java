package com.u2ware.springfield.repository.hibernate.support;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import javax.sql.DataSource;

import org.hibernate.dialect.DB2Dialect;
import org.hibernate.dialect.DerbyDialect;
import org.hibernate.dialect.H2Dialect;
import org.hibernate.dialect.HSQLDialect;
import org.hibernate.dialect.MySQL5Dialect;
import org.hibernate.dialect.Oracle10gDialect;
import org.hibernate.dialect.PostgreSQLDialect;
import org.hibernate.dialect.SQLServer2008Dialect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.support.JdbcUtils;
import org.springframework.jdbc.support.MetaDataAccessException;


public enum HibernateDialectType {

	DERBY("Apache Derby", DerbyDialect.class.getName()), 
	DB2("DB2", DB2Dialect.class.getName()), 
	DB2ZOS("DB2ZOS", DB2Dialect.class.getName()), 
	HSQL("HSQL Database Engine", HSQLDialect.class.getName()),
	SQLSERVER("Microsoft SQL Server", SQLServer2008Dialect.class.getName()),
	MYSQL("MySQL", MySQL5Dialect.class.getName()),
	ORACLE("Oracle", Oracle10gDialect.class.getName()),
	POSTGRES("PostgreSQL", PostgreSQLDialect.class.getName()),
	SYBASE("Sybase", Class.class.getName()), 
	H2("H2", H2Dialect.class.getName()),
	ALTIBASE("Altibase", AltibaseDialect.class.getName());

	private final String productName;
	private final String dialect;
	
	private HibernateDialectType(String productName, String dialect) {
		this.productName = productName;
		this.dialect = dialect;
	}
	
	public String getProductName() {
		return productName;
	}
	public String getDialect() {
		return dialect;
	}
	
	
	private static final Logger logger = LoggerFactory.getLogger(HibernateDialectType.class);
	private static final Map<String, HibernateDialectType> nameMap;
	
	static{
		nameMap = new HashMap<String, HibernateDialectType>();
		for(HibernateDialectType type: values()){
			nameMap.put(type.getProductName(), type);
		}
	}
	

	public static HibernateDialectType fromProductName(String productName){
		if(!nameMap.containsKey(productName)){
			Exception e = new IllegalArgumentException("DatabaseType not found for product name: [" + productName + "]");
			e.printStackTrace();
			return ALTIBASE;
		}
		else{
			return nameMap.get(productName);
		}
	}
	
	
	
	public static HibernateDialectType fromMetaData(DataSource dataSource) throws MetaDataAccessException {
		String databaseProductName = JdbcUtils.extractDatabaseMetaData(dataSource, "getDatabaseProductName").toString();
		String commonDatabaseName = JdbcUtils.commonDatabaseName(databaseProductName);
		logger.warn("Database  product name is ["+commonDatabaseName+"]");
		return fromProductName(commonDatabaseName);
	}
	
	
	public static Properties hibernateProperties(DataSource dataSource) throws MetaDataAccessException {
		Properties p = new Properties();
		p.put("hibernate.cache.provider_class",  "org.hibernate.cache.HashtableCacheProvider");
		p.put("hibernate.dialect" , fromMetaData(dataSource).getDialect());
		p.put("hibernate.show_sql", "false");
		p.put("hibernate.format_sql", "true");
		p.put("hibernate.hbm2ddl.auto", "update");
		//p.put("hibernate.connection.release_mode", "after_transaction");
		//p.put("hibernate.current_session_context_class", "thread");
		logger.warn("hibernateProperties is ["+p+"]");
		return p;
	}

	
}
