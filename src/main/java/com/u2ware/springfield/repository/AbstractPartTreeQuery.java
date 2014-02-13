package com.u2ware.springfield.repository;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeanWrapper;
import org.springframework.beans.PropertyAccessorFactory;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.data.domain.Sort;
import org.springframework.data.repository.query.parser.Part;
import org.springframework.data.repository.query.parser.PartTree;
import org.springframework.data.repository.query.parser.PartTree.OrPart;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;

public abstract class AbstractPartTreeQuery<S> {

	protected final Log logger = LogFactory.getLog(getClass());


	private PartTree partTree ;
	private BeanWrapper paramWrapper ;
	
	public AbstractPartTreeQuery(Class<?> entityClass, Object param){
		this.partTree = new PartTree(quessQueryMethodName(param), entityClass);
		this.paramWrapper = PropertyAccessorFactory.forBeanPropertyAccess(param);
	}
	
	public Sort createSort() {
		return partTree.getSort();
	}
	
	protected S createCriteria() {
		return createCriteria(partTree, paramWrapper);
	}
	
	//Jpa
	protected String quessQueryMethodName(Object query){

		Class<?> beanClass = query.getClass();
		String queryMethodName = ClassUtils.getShortNameAsProperty(beanClass);
		QueryMethod queryMethod = AnnotationUtils.findAnnotation(beanClass, QueryMethod.class);

		if(queryMethod != null && StringUtils.hasText(queryMethod.value())){
			queryMethodName = queryMethod.value();
		}

		if(! queryMethodName.toLowerCase().startsWith("findby")){
			queryMethodName = "findBy";
		}

		logger.info(query.getClass() +" quessQueryMethodName is "+queryMethodName);
		return queryMethodName;
	}
	
	protected S createCriteria(PartTree tree, BeanWrapper paramWrapper) {
		
		S base = null;
		for (OrPart node : tree) {
			//logger.debug("OrPart : "+node.getClass());
			
			S criteria = null;
			for (Part part : node) {
				//logger.debug("Part "+part.getClass());
				
				if(part.getProperty() != null){
					S newCriteria = create(part, paramWrapper);
					if(newCriteria != null){
						//logger.debug("ok....");
						criteria = criteria == null ? newCriteria : and(criteria, newCriteria, paramWrapper);
					}
				}
			}
			base = base == null ? criteria : or(base, criteria, paramWrapper);
		}
		//logger.debug("base "+base);
		
		return base;
	}
	
	protected abstract S create(Part part, BeanWrapper paramWrapper);

	protected abstract S and(S base, S criteria, BeanWrapper paramWrapper);

	protected abstract S or(S base, S criteria, BeanWrapper paramWrapper);
	
	

}

/*
		switch(type){
			case AFTER:
			case GREATER_THAN:
			case GREATER_THAN_EQUAL:
			case BEFORE:
			case LESS_THAN:
			case LESS_THAN_EQUAL:
			case BETWEEN:
			case IS_NOT_NULL:
			case IS_NULL:
			case NOT_IN:
			case IN:
			case LIKE:
			case STARTING_WITH:
			case ENDING_WITH:
			case CONTAINING:
			case REGEX:
			case EXISTS:
			case TRUE:
			case FALSE:
			case NEAR:
			case WITHIN:
			case SIMPLE_PROPERTY:
			case NEGATING_SIMPLE_PROPERTY:
		}
		throw new IllegalArgumentException("Unsupported keyword!");
		
		
		
		
		if(Type.AFTER.equals(type)){
			return null;
		}else if(Type.GREATER_THAN.equals(type)){
			return null;
		}else if(Type.GREATER_THAN_EQUAL.equals(type)){
			return null;
		}else if(Type.BEFORE.equals(type)){
			return null;
		}else if(Type.LESS_THAN.equals(type)){
			return null;
		}else if(Type.LESS_THAN_EQUAL.equals(type)){
			return null;
		}else if(Type.BETWEEN.equals(type)){
			return null;
		}else if(Type.IS_NOT_NULL.equals(type)){
			return null;
		}else if(Type.IS_NULL.equals(type)){
			return null;
		}else if(Type.NOT_IN.equals(type)){
			return null;
		}else if(Type.IN.equals(type)){
			return null;
		}else if(Type.LIKE.equals(type)){
			return null;
		}else if(Type.STARTING_WITH.equals(type)){
			return null;
		}else if(Type.ENDING_WITH.equals(type)){
			return null;
		}else if(Type.CONTAINING.equals(type)){
			return null;
		}else if(Type.REGEX.equals(type)){
			return null;
		}else if(Type.EXISTS.equals(type)){
			return null;
		}else if(Type.TRUE.equals(type)){
			return null;
		}else if(Type.FALSE.equals(type)){
			return null;
		}else if(Type.NEAR.equals(type)){
			return null;
		}else if(Type.WITHIN.equals(type)){
			return null;
		}else if(Type.SIMPLE_PROPERTY.equals(type)){
			return null;
		}else if(Type.NEGATING_SIMPLE_PROPERTY.equals(type)){
			return null;
		}else{
			throw new IllegalArgumentException("Unsupported keyword!");
		}

*/
