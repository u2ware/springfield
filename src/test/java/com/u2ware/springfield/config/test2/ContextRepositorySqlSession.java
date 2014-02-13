package com.u2ware.springfield.config.test2;

import javax.sql.DataSource;

import org.mybatis.spring.SqlSessionFactoryBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ResourceLoaderAware;
import org.springframework.core.io.ResourceLoader;

//@Configuration
//@EnableTransactionManagement
public class ContextRepositorySqlSession implements ResourceLoaderAware{

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	protected ResourceLoader resourceLoader;
	protected String basePackage;
	protected DataSource dataSource;

	public void setResourceLoader(ResourceLoader resourceLoader) {
		this.resourceLoader = resourceLoader;
	} 
	public void setDataSource(DataSource dataSource) {
		this.dataSource = dataSource;
	}
	public void setBasePackage(String basePackage) {
		this.basePackage = basePackage;
	}
	
	public SqlSessionFactoryBean springfieldSqlSessionFactory() throws Exception{
		logger.trace("create SqlSessionFactoryBean");
		
		SqlSessionFactoryBean b = new SqlSessionFactoryBean();
		b.setDataSource(dataSource);
		//b.setMapperLocations(r.getResources());
		return b;
	}

	
}
