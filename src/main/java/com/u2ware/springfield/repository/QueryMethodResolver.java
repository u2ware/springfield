package com.u2ware.springfield.repository;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanWrapper;
import org.springframework.beans.PropertyAccessorFactory;
import org.springframework.data.domain.Sort;
import org.springframework.data.repository.query.parser.Part;
import org.springframework.data.repository.query.parser.Part.Type;
import org.springframework.data.repository.query.parser.PartTree;
import org.springframework.data.repository.query.parser.PartTree.OrPart;

import com.mysema.query.types.Predicate;
import com.mysema.query.types.expr.BooleanExpression;
import com.mysema.query.types.path.BooleanPath;
import com.mysema.query.types.path.ComparablePath;
import com.mysema.query.types.path.PathBuilder;
import com.mysema.query.types.path.StringPath;



public class QueryMethodResolver<T> {

	protected final Logger logger = LoggerFactory.getLogger(getClass());
	
	
	private Predicate predicate;
	private Sort sort;
	
	public QueryMethodResolver(PathBuilder<T> pathBuilder, Object queryMethod){

		String queryMethodName = QueryMethodUtil.methodName(queryMethod);
		logger.warn(" QueryDsl: "+queryMethodName);
		
		PartTree tree = new PartTree(queryMethodName, pathBuilder.getType());
		BeanWrapper bean = PropertyAccessorFactory.forBeanPropertyAccess(queryMethod);
		
		this.predicate = createPredicate(pathBuilder, tree, bean);
		this.sort = tree.getSort();	
	}

	public Predicate predicate() {
		return predicate;
	}
	public Sort sort() {
		return sort;
	}
	
	
	
	private Predicate createPredicate(PathBuilder<?> builder, PartTree tree, BeanWrapper bean){
		
		//BeanWrapper paramWrapper = PropertyAccessorFactory.forBeanPropertyAccess(param);
		
		BooleanExpression base = null;
		for (OrPart node : tree) {
			//logger.debug("OrPart : "+node.getClass());
			
			BooleanExpression criteria = null;
			for (Part part : node) {
				//logger.debug("Part "+part.getClass());
				
				if(part.getProperty() != null){
					BooleanExpression newCriteria = create(builder, part, bean);
					if(newCriteria != null){
						//logger.debug("ok....");
						criteria = criteria == null ? newCriteria : and(criteria, newCriteria, bean);
					}
				}
			}
			base = base == null ? criteria : or(base, criteria, bean);
		}
		//logger.debug("base "+base);
		
		return base;
	}
	private BooleanExpression and(BooleanExpression base, BooleanExpression criteria, BeanWrapper bean) {
		return base.and(criteria);
	}
	private BooleanExpression or(BooleanExpression base, BooleanExpression criteria, BeanWrapper bean) {
		return base.or(criteria);
	}
	
	
	@SuppressWarnings("unchecked")
	private BooleanExpression create(PathBuilder<?> builder, Part part, BeanWrapper beanWrapper){
		
		
		Type treeType = part.getType();
		
		String name = part.getProperty().getSegment();
		Class<?> propertyType = beanWrapper.getPropertyType(name);
		Object propertyValue = null;
		if(propertyType == null){
			throw new RuntimeException("QueryDsl parameter: '"+name+"' is not found.");
		}else{
			propertyValue = beanWrapper.getPropertyValue(name);
		}
		
		if(Type.BETWEEN.equals(treeType)){
			if(propertyValue == null) return null;
			
			Comparable<?>[] between = (Comparable<?>[])propertyValue;
			Class<Comparable<?>> type = (Class<Comparable<?>>) between[0].getClass();
			
			ComparablePath<Comparable<?>> property = builder.getComparable(name, type);
			logger.warn("QueryDsl parameter: "+name+" between min["+between[0]+"] and max["+between[1]+"]");
			return property.between(between[0], between[1]);

		}else if(Type.AFTER.equals(treeType)){
			
			
		}else if(Type.GREATER_THAN.equals(treeType)){
			if(propertyValue == null) return null;
			
			Comparable<?> value = (Comparable<?>)propertyValue;
			Class<Comparable<?>> type = (Class<Comparable<?>>)propertyType;
			
			ComparablePath<Comparable<?>> property = builder.getComparable(name, type);
			logger.warn("QueryDsl parameter: "+name+" > "+value);
			return property.gt(value);

		}else if(Type.GREATER_THAN_EQUAL.equals(treeType)){
			if(propertyValue == null) return null;

			Comparable<?> value = (Comparable<?>)propertyValue;
			Class<Comparable<?>> type = (Class<Comparable<?>>)propertyType;

			ComparablePath<Comparable<?>> property = builder.getComparable(name, type);
			logger.warn("QueryDsl parameter: "+name+" >= "+value);
			return property.goe(value);

		}else if(Type.BEFORE.equals(treeType)){
			
			
		}else if(Type.LESS_THAN.equals(treeType)){
			if(propertyValue == null) return null;
			
			Comparable<?> value = (Comparable<?>)propertyValue;
			Class<Comparable<?>> type = (Class<Comparable<?>>)propertyType;
			
			ComparablePath<Comparable<?>> property = builder.getComparable(name, type);
			logger.warn("QueryDsl parameter: "+name+" < "+value);
			return property.lt(value);
			

		}else if(Type.LESS_THAN_EQUAL.equals(treeType)){
			if(propertyValue == null) return null;

			Comparable<?> value = (Comparable<?>)propertyValue;
			Class<Comparable<?>> type = (Class<Comparable<?>>)propertyType;

			ComparablePath<Comparable<?>> property = builder.getComparable(name, type);
			logger.warn("QueryDsl parameter: "+name+" <= "+value);
			return property.loe(value);

		}else if(Type.IS_NULL.equals(treeType)){

			//if(propertyValue != null) return null;

			Class<?> type = propertyType;
			
			PathBuilder<?> property = builder.get(name, type);
			logger.debug("QueryDsl parameter: "+name+" is null");
			return property.isNull();

		}else if(Type.IS_NOT_NULL.equals(treeType)){

			//if(propertyValue == null) return null;
			Class<?> type = propertyType;

			PathBuilder<?> property = builder.get(name, type);
			logger.warn("QueryDsl parameter: "+name+" is not null");
			return property.isNotNull();
			
		}else if(Type.NOT_IN.equals(treeType)){

			
		}else if(Type.IN.equals(treeType)){
		

			
		}else if(Type.STARTING_WITH.equals(treeType)){

			if(propertyValue == null) return null;

			String value = propertyValue.toString()+"%";
			
			StringPath property = builder.getString(name);
			logger.warn("QueryDsl parameter: "+name+" like "+value);
			return property.like(value);
			
		}else if(Type.ENDING_WITH.equals(treeType)){

			if(propertyValue == null) return null;
			String value = "%"+propertyValue.toString();
			
			StringPath property = builder.getString(name);
			logger.warn("QueryDsl parameter: "+name+" like "+value);
			return property.like(value);

		}else if(Type.CONTAINING.equals(treeType)){

			if(propertyValue == null) return null;
			String value = "%"+propertyValue.toString()+"%";
			
			StringPath property = builder.getString(name);
			logger.warn("QueryDsl parameter: "+name+" like "+value);
			return property.like(value);

		}else if(Type.LIKE.equals(treeType)){
	
			if(propertyValue == null) return null;
			String value = propertyValue.toString();
			
			StringPath property = builder.getString(name);
			logger.warn("QueryDsl parameter: "+name+" like "+value);
			return property.like(value);

		}else if(Type.NOT_LIKE.equals(treeType)){
			
			if(propertyValue == null) return null;
			String value = propertyValue.toString();
			
			StringPath property = builder.getString(name);
			logger.warn("QueryDsl parameter: "+name+" not like "+value);
			return property.notLike(value);
			
			
		}else if(Type.TRUE.equals(treeType)){

			//if(propertyValue == null) return null;
			//Class<Boolean> type = (Class<Boolean>)propertyType;
			//Boolean value = (Boolean)propertyValue;

			BooleanPath property = builder.getBoolean(name);
			logger.warn("QueryDsl parameter: "+name+" is true ");
			return property.isTrue();
			

		}else if(Type.FALSE.equals(treeType)){

			//if(propertyValue == null) return null;
			//Class<Boolean> type = (Class<Boolean>)propertyType;
			//Boolean value = (Boolean)propertyValue;
			BooleanPath property = builder.getBoolean(name);
			logger.warn("QueryDsl parameter: "+name+" is false ");
			return property.isFalse();
			
			
		}else if(Type.SIMPLE_PROPERTY.equals(treeType)){
	
			if(propertyValue == null) return null;
			Class<Object> type = (Class<Object>)propertyType;
			
			PathBuilder<Object> property = builder.get(name, type);
			logger.warn("QueryDsl parameter: "+name+" = "+propertyValue);
			return property.eq(propertyValue);

		}else if(Type.NEGATING_SIMPLE_PROPERTY.equals(treeType)){

			if(propertyValue == null) return null;
			Class<Object> type = (Class<Object>)propertyType;
			
			PathBuilder<Object> property = builder.get(name, type);
			logger.warn("QueryDsl parameter: "+name+" != "+propertyValue);
			return property.ne(propertyValue);
			
		}else{
			throw new IllegalArgumentException("Unsupported keyword " + part.getType());
		}
		return null;
	}
}









