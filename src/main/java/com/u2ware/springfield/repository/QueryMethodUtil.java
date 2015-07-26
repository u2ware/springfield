package com.u2ware.springfield.repository;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.data.querydsl.EntityPathResolver;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;

import com.mysema.query.types.EntityPath;
import com.mysema.query.types.path.PathBuilderFactory;

public abstract class QueryMethodUtil {
	
	private static final Logger logger = LoggerFactory.getLogger(QueryMethodUtil.class);

	public static String methodName(Object query){
		Class<?> beanClass = query.getClass();
		String queryMethodName = ClassUtils.getShortNameAsProperty(beanClass);
		QueryMethod queryMethod = AnnotationUtils.findAnnotation(beanClass, QueryMethod.class);

		if(queryMethod != null && StringUtils.hasText(queryMethod.value())){
			queryMethodName = queryMethod.value();
		}

		if(! queryMethodName.toLowerCase().startsWith("find")){
			queryMethodName = "findBy";
		}
		logger.debug(" quessQueryMethodName is "+queryMethodName);
		return queryMethodName;
	}

	public static EntityPathResolver ENTITY_PATH_RESOLVER = new CustomEntityPathResolver();
	
	private static class CustomEntityPathResolver implements EntityPathResolver{

		@Override
		public <T> EntityPath<T> createPath(Class<T> domainClass) {
			return  new PathBuilderFactory().create(domainClass);
		}
	}
	
	
}
