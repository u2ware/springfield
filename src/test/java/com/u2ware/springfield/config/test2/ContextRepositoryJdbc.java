package com.u2ware.springfield.config.test2;

import javax.sql.DataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;

//@Configuration
//@EnableTransactionManagement
public class ContextRepositoryJdbc {

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	protected String basePackage;
	protected DataSource dataSource;

	public void setDataSource(DataSource dataSource) {
		this.dataSource = dataSource;
	}
	public void setBasePackage(String basePackage) {
		this.basePackage = basePackage;
	}

	public DataSourceTransactionManager springfieldDataSourceTx() throws Exception{
		logger.trace("create DataSourceTransactionManager");
		DataSourceTransactionManager b = new DataSourceTransactionManager();
		b.setDataSource(dataSource);
		return b;
	}
	
}
