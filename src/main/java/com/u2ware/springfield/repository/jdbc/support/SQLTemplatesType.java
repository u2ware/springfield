package com.u2ware.springfield.repository.jdbc.support;

import java.util.HashMap;
import java.util.Map;

import javax.sql.DataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.support.JdbcUtils;
import org.springframework.jdbc.support.MetaDataAccessException;

import com.mysema.query.sql.DerbyTemplates;
import com.mysema.query.sql.H2Templates;
import com.mysema.query.sql.HSQLDBTemplates;
import com.mysema.query.sql.MySQLTemplates;
import com.mysema.query.sql.OracleTemplates;
import com.mysema.query.sql.PostgresTemplates;
import com.mysema.query.sql.SQLServerTemplates;
import com.mysema.query.sql.SQLTemplates;

public enum SQLTemplatesType {


	DERBY("Apache Derby", new DerbyTemplates()), 
	DB2("DB2", null), 
	DB2ZOS("DB2ZOS", null), 
	HSQL("HSQL Database Engine", new HSQLDBTemplates()),
	SQLSERVER("Microsoft SQL Server", new SQLServerTemplates()),
	MYSQL("MySQL", new MySQLTemplates()),
	ORACLE("Oracle", new OracleTemplates()),
	POSTGRES("PostgreSQL", new PostgresTemplates()),
	SYBASE("Sybase", null), 
	H2("H2", new H2Templates()),
	ALTIBASE("Altibase", new OracleTemplates());

	private final String productName;
	private final SQLTemplates dialect;
	
	private SQLTemplatesType(String productName, SQLTemplates dialect) {
		this.productName = productName;
		this.dialect = dialect;
	}
	
	public String getProductName() {
		return productName;
	}
	public SQLTemplates getDialect() {
		return dialect;
	}
	
	private static final Logger logger = LoggerFactory.getLogger(SQLTemplatesType.class);
	private static final Map<String, SQLTemplatesType> nameMap;
	
	static{
		nameMap = new HashMap<String, SQLTemplatesType>();
		for(SQLTemplatesType type: values()){
			nameMap.put(type.getProductName(), type);
		}
	}

	public static SQLTemplatesType fromProductName(String productName){
		if(!nameMap.containsKey(productName)){
			Exception e = new IllegalArgumentException("DatabaseType not found for product name: [" + productName + "]");
			e.printStackTrace();
			return ALTIBASE;
		}
		else{
			return nameMap.get(productName);
		}
	}
	
	public static SQLTemplatesType fromMetaData(DataSource dataSource) throws MetaDataAccessException{
		String databaseProductName = JdbcUtils.extractDatabaseMetaData(dataSource, "getDatabaseProductName").toString();
		String commonDatabaseName = JdbcUtils.commonDatabaseName(databaseProductName);
		logger.warn("Database product name is ["+commonDatabaseName+"]");
		return fromProductName(commonDatabaseName);
	}
	
	
	public static SQLTemplates sqlTemplates(DataSource dataSource)throws MetaDataAccessException{
		SQLTemplates result = fromMetaData(dataSource).getDialect();
		logger.warn("SQLTemplates is ["+result.getClass()+"]");
		return result;
	}
	
	
	
}

