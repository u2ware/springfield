package com.u2ware.springfield.repository;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanWrapper;
import org.springframework.beans.PropertyAccessorFactory;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.data.domain.Sort;
import org.springframework.data.repository.query.parser.Part;
import org.springframework.data.repository.query.parser.Part.Type;
import org.springframework.data.repository.query.parser.PartTree;
import org.springframework.data.repository.query.parser.PartTree.OrPart;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;

import com.mysema.query.types.Predicate;
import com.mysema.query.types.expr.BooleanExpression;
import com.mysema.query.types.path.BooleanPath;
import com.mysema.query.types.path.ComparablePath;
import com.mysema.query.types.path.PathBuilder;
import com.mysema.query.types.path.StringPath;

public class QueryMethodPredicate {

	private static final Logger logger = LoggerFactory.getLogger(QueryMethodPredicate.class);

	protected final Predicate predicate;
	protected final Sort sort;
	
	public QueryMethodPredicate(Class<?> entityClass, PathBuilder<?> builder, Object param){
		String queryMethodName = quessQueryMethodName(param);
		PartTree partTree = new PartTree(queryMethodName, entityClass);
		BeanWrapper paramWrapper = PropertyAccessorFactory.forBeanPropertyAccess(param);
		
		this.predicate = createPredicate(partTree, builder, paramWrapper);
		this.sort = partTree.getSort();
	}

	public Predicate getPredicate(){
		return predicate;
	}
	public Sort getSort(){
		return sort;
	}
	public Sort getSort(Sort... sorts){
		Sort result = sort;
		for(Sort s : sorts){
			result = result != null ? result.and(s) : s;
		}
		return result;
	}
	
	
	protected String quessQueryMethodName(Object query){
		Class<?> beanClass = query.getClass();
		String queryMethodName = ClassUtils.getShortNameAsProperty(beanClass);
		QueryMethod queryMethod = AnnotationUtils.findAnnotation(beanClass, QueryMethod.class);

		if(queryMethod != null && StringUtils.hasText(queryMethod.value())){
			queryMethodName = queryMethod.value();
		}

		if(! queryMethodName.toLowerCase().startsWith("find")){
			queryMethodName = "findBy";
		}
		logger.info(query.getClass() +" quessQueryMethodName is "+queryMethodName);
		return queryMethodName;
	}
	
	
	
	
	
	/////////////////////////////////////////////////////
	//
	/////////////////////////////////////////////////////
	private BooleanExpression createPredicate(PartTree tree, PathBuilder<?> builder, BeanWrapper paramWrapper) {
		
		BooleanExpression base = null;
		for (OrPart node : tree) {
			//logger.debug("OrPart : "+node.getClass());
			
			BooleanExpression criteria = null;
			for (Part part : node) {
				//logger.debug("Part "+part.getClass());
				
				if(part.getProperty() != null){
					BooleanExpression newCriteria = create(part, builder, paramWrapper);
					if(newCriteria != null){
						//logger.debug("ok....");
						criteria = criteria == null ? newCriteria : and(criteria, newCriteria, builder, paramWrapper);
					}
				}
			}
			base = base == null ? criteria : or(base, criteria, builder, paramWrapper);
		}
		//logger.debug("base "+base);
		
		return base;
	}
	private BooleanExpression and(BooleanExpression base, BooleanExpression criteria, PathBuilder<?> builder, BeanWrapper paramWrapper) {
		return base.and(criteria);
	}
	private BooleanExpression or(BooleanExpression base, BooleanExpression criteria, PathBuilder<?> builder, BeanWrapper paramWrapper) {
		return base.or(criteria);
	}
	
	
	@SuppressWarnings("unchecked")
	private BooleanExpression create(Part part, PathBuilder<?> builder, BeanWrapper paramWrapper){
		
		
		Type type = part.getType();
		String name = part.getProperty().getSegment();
		
		
		if(Type.BETWEEN.equals(type)){
			Comparable<?> min = (Comparable<?>)paramWrapper.getPropertyValue(name+"Min");
			Comparable<?> max = (Comparable<?>)paramWrapper.getPropertyValue(name+"Max");

			if(min != null && max != null){
				logger.info("set query parameter : "+name+"Min");
				logger.info("set query parameter : "+name+"Max");
				ComparablePath<Comparable<?>> property = builder.getComparable(name, paramWrapper.getPropertyType(name+"Min"));
				return property.between(min, max);
			}			

		}else if(Type.AFTER.equals(type)){
			
			
		}else if(Type.GREATER_THAN.equals(type)){
			Comparable<?> greaterThan = (Comparable<?>)paramWrapper.getPropertyValue(name);
			if(greaterThan != null){
				logger.info("set query parameter : "+name);
				ComparablePath<Comparable<?>> property = builder.getComparable(name, paramWrapper.getPropertyType(name));
				return property.gt(greaterThan);
			}

		}else if(Type.GREATER_THAN_EQUAL.equals(type)){
			Comparable<?> greaterThan = (Comparable<?>)paramWrapper.getPropertyValue(name);
			if(greaterThan != null){
				logger.info("set query parameter : "+name);
				ComparablePath<Comparable<?>> property = builder.getComparable(name, paramWrapper.getPropertyType(name));
				return property.goe(greaterThan);
			}

		}else if(Type.BEFORE.equals(type)){
			
			
		}else if(Type.LESS_THAN.equals(type)){
			Comparable<?> lessThan = (Comparable<?>)paramWrapper.getPropertyValue(name);
			if(lessThan != null){
				logger.info("set query parameter : "+name);
				ComparablePath<Comparable<?>> property = builder.getComparable(name, paramWrapper.getPropertyType(name));
				return property.lt(lessThan);
			}

		}else if(Type.LESS_THAN_EQUAL.equals(type)){
			Comparable<?> lessThan = (Comparable<?>)paramWrapper.getPropertyValue(name);
			if(lessThan != null){
				logger.info("set query parameter : "+name);
				ComparablePath<Comparable<?>> property = builder.getComparable(name, paramWrapper.getPropertyType(name));
				return property.loe(lessThan);
			}

		}else if(Type.IS_NULL.equals(type)){
			PathBuilder<?> property = builder.get(name, paramWrapper.getPropertyType(name));
			return property.isNull();

		}else if(Type.IS_NOT_NULL.equals(type)){
			PathBuilder<?> property = builder.get(name, paramWrapper.getPropertyType(name));
			return property.isNotNull();
			
		}else if(Type.NOT_IN.equals(type)){

			
		}else if(Type.IN.equals(type)){
		

			
		}else if(Type.STARTING_WITH.equals(type)){

			Object likeValue = paramWrapper.getPropertyValue(name);
			if(likeValue != null){
				logger.info("set query parameter : "+name);
				StringPath property = builder.getString(name);
				return property.like(likeValue.toString()+"%");
			}
			
		}else if(Type.ENDING_WITH.equals(type)){
			Object likeValue = paramWrapper.getPropertyValue(name);
			if(likeValue != null){
				logger.info("set query parameter : "+name);
				StringPath property = builder.getString(name);
				return property.like("%"+likeValue.toString());
			}

		}else if(Type.CONTAINING.equals(type)){
			Object likeValue = paramWrapper.getPropertyValue(name);
			if(likeValue != null){
				logger.info("set query parameter : "+name);
				StringPath property = builder.getString(name);
				return property.like("%"+likeValue.toString()+"%");
			}

		}else if(Type.LIKE.equals(type)){
			Object likeValue = paramWrapper.getPropertyValue(name);
			if(likeValue != null){
				logger.info("set query parameter : "+name);
				StringPath property = builder.getString(name);
				return property.like(likeValue.toString());
			}

		}else if(Type.NOT_LIKE.equals(type)){
			
			Object likeValue = paramWrapper.getPropertyValue(name);
			if(likeValue != null){
				logger.info("set query parameter : "+name);
				StringPath property = builder.getString(name);
				return property.notLike(likeValue.toString());
			}
			
			
		}else if(Type.TRUE.equals(type)){

			Object trueValue = paramWrapper.getPropertyValue(name);
			if(trueValue != null){
				logger.info("set query parameter : "+name);
				BooleanPath property = builder.getBoolean(name);
				return property.isTrue();
			}

		}else if(Type.FALSE.equals(type)){

			Object trueValue = paramWrapper.getPropertyValue(name);
			if(trueValue != null){
				logger.info("set query parameter : "+name);
				BooleanPath property = builder.getBoolean(name);
				return property.isFalse();
			}
			
		}else if(Type.SIMPLE_PROPERTY.equals(type)){
			Object equalValue = paramWrapper.getPropertyValue(name);
			if(equalValue != null){
				PathBuilder<Object> property = builder.get(name, paramWrapper.getPropertyType(name));
				logger.info("set query parameter : "+name);
				return property.eq(equalValue);
			}
			

		}else if(Type.NEGATING_SIMPLE_PROPERTY.equals(type)){
			Object equalValue = paramWrapper.getPropertyValue(name);
			if(equalValue != null){
				PathBuilder<Object> property = builder.get(name, paramWrapper.getPropertyType(name));
				logger.info("set query parameter : "+name);
				return property.eq(equalValue).not();
			}
			
		}else{
			throw new IllegalArgumentException("Unsupported keyword " + part.getType());
		}
		return null;
	}

	
}
