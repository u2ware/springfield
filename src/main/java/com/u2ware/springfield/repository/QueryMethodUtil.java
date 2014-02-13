package com.u2ware.springfield.repository;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;

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
}
