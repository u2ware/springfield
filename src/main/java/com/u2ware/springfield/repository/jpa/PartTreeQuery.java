package com.u2ware.springfield.repository.jpa;

import java.util.Arrays;
import java.util.Collection;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.springframework.beans.BeanWrapper;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.mapping.PropertyPath;
import org.springframework.data.repository.query.parser.Part;
import org.springframework.data.repository.query.parser.Part.Type;
import org.springframework.util.ClassUtils;
import org.springframework.util.ObjectUtils;

import com.u2ware.springfield.repository.AbstractPartTreeQuery;

class PartTreeQuery<T> extends AbstractPartTreeQuery<Predicate> implements Specification<T>{

	//protected final Log logger = LogFactory.getLog(getClass());

	public PartTreeQuery(Class<?> entityClass, Object param) {
		super(entityClass, param);
	}

	protected Root<T> root;
	protected CriteriaQuery<?> query;
	protected CriteriaBuilder builder;

	
	////////////////////////////////////////
	//
	////////////////////////////////////////
	public Predicate toPredicate(Root<T> root, CriteriaQuery<?> query, CriteriaBuilder builder) {
		this.root = root;
		this.query = query;
		this.builder = builder;
		query.distinct(super.partTree.isDistinct());
		
		return super.createCriteria();
	}

	
	
	////////////////////////////////////////
	//
	////////////////////////////////////////
	@Override
	protected Predicate and(Predicate base, Predicate criteria, BeanWrapper paramWrapper) {
		return builder.and(base, criteria);
	}
	@Override
	protected Predicate or(Predicate base, Predicate criteria, BeanWrapper paramWrapper) {
		return builder.or(base, criteria);
	}
	@Override
	protected Predicate create(Part part, BeanWrapper paramWrapper) {
		return from(part, paramWrapper);
	}
	
	
	////////////////////////////////////////
	//
	////////////////////////////////////////
	@SuppressWarnings("unchecked")
	private Predicate from(Part part, BeanWrapper paramWrapper) {

		/*
		if(part.getProperty() == null){
			logger.debug("?"+part.getType());
			logger.debug("?"+part.getProperty());
			logger.debug("?"+part.getParameterRequired());
			logger.debug("?"+part.getNumberOfArguments());
			return null;
		}
		
		logger.debug("?"+part.getType());
		logger.debug("?"+part.getParameterRequired());
		logger.debug("?"+part.getNumberOfArguments());
		logger.debug("?"+part.getProperty().getSegment());
		*/
		Type type = part.getType();
		String name = part.getProperty().getSegment();
		
		
		
		if(Type.BETWEEN.equals(type)){
			Comparable<?> min = (Comparable<?>)paramWrapper.getPropertyValue(name+"Min");
			Comparable<?> max = (Comparable<?>)paramWrapper.getPropertyValue(name+"Max");

			if(min != null && max != null){
				logger.info("set query parameter : "+name+"Min");
				logger.info("set query parameter : "+name+"Max");
				return builder.between(getComparablePath(root, part), min, max);
			}else if(min != null && max == null){
				logger.info("set query parameter : "+name+"Min");
				return builder.greaterThanOrEqualTo(getComparablePath(root, part), min);
			}else if(min == null && max != null){
				logger.info("set query parameter : "+name+"Max");
				return builder.lessThanOrEqualTo(getComparablePath(root, part), max);
			}
			
		}else if(Type.AFTER.equals(type)){
			
			
		}else if(Type.GREATER_THAN.equals(type)){
			Comparable<?> greaterThan = (Comparable<?>)paramWrapper.getPropertyValue(name);
			if(greaterThan != null){
				logger.info("set query parameter : "+name);
				return builder.greaterThan(getComparablePath(root, part), greaterThan);
			}
		}else if(Type.GREATER_THAN_EQUAL.equals(type)){
			Comparable<?> greaterThanEqual = (Comparable<?>)paramWrapper.getPropertyValue(name);
			if(greaterThanEqual != null){
				logger.info("set query parameter : "+name);
				return builder.greaterThanOrEqualTo(getComparablePath(root, part), greaterThanEqual);
			}
		}else if(Type.BEFORE.equals(type)){
			
			
		}else if(Type.LESS_THAN.equals(type)){
			Comparable<?> lessThan = (Comparable<?>)paramWrapper.getPropertyValue(name);
			if(lessThan != null){
				logger.info("set query parameter : "+name);
				return builder.lessThan(getComparablePath(root, part), lessThan);
			}
		}else if(Type.LESS_THAN_EQUAL.equals(type)){
			Comparable<?> greaterThan = (Comparable<?>)paramWrapper.getPropertyValue(name);
			if(greaterThan != null){
				logger.info("set query parameter : "+name);
				return builder.lessThanOrEqualTo(getComparablePath(root, part), greaterThan);
			}
		}else if(Type.IS_NULL.equals(type)){
			Expression<?> isNullPath = getTypedPath(root, part);
			return isNullPath.isNull();

		}else if(Type.IS_NOT_NULL.equals(type)){
			Expression<?> isNullPath = getTypedPath(root, part);
			return isNullPath.isNotNull();
			
		}else if(Type.NOT_IN.equals(type)){
			Object inValue = paramWrapper.getPropertyValue(name);
			if(inValue != null){
				
				Expression<String> inPath = getTypedPath(root, part);
				if(ClassUtils.isAssignableValue(Collection.class, inValue)){
					return inPath.in((Collection<?>)inValue).not();
				}else if(ObjectUtils.isArray(inValue)){
					return inPath.in(Arrays.asList(inValue)).not();
				}
			}
			
		}else if(Type.IN.equals(type)){
		
			Object inValue = paramWrapper.getPropertyValue(name);
			if(inValue != null){
				
				Expression<String> inPath = getTypedPath(root, part);
				if(ClassUtils.isAssignableValue(Collection.class, inValue)){
					return inPath.in((Collection<?>)inValue);
				}else if(ObjectUtils.isArray(inValue)){
					return inPath.in(Arrays.asList(inValue)).not();
				}
			}
			
		}else if(Type.STARTING_WITH.equals(type)){
			Object likeValue = paramWrapper.getPropertyValue(name);
			if(likeValue != null){
				logger.info("set query parameter : "+name);
				
				Expression<String> likePath = getTypedPath(root, part);
				return builder.like(likePath, "%"+likeValue.toString());
			}
		}else if(Type.ENDING_WITH.equals(type)){
			Object likeValue = paramWrapper.getPropertyValue(name);
			if(likeValue != null){
				logger.info("set query parameter : "+name);
				Expression<String> likePath = getTypedPath(root, part);
				return builder.like(likePath, likeValue.toString()+"%");
			}
		}else if(Type.CONTAINING.equals(type)){
			Object likeValue = paramWrapper.getPropertyValue(name);
			if(likeValue != null){
				logger.info("set query parameter : "+name);
				Expression<String> likePath = getTypedPath(root, part);
				return builder.like(likePath, "%"+likeValue.toString()+"%");
			}
		}else if(Type.LIKE.equals(type)){
			Object likeValue = paramWrapper.getPropertyValue(name);
			if(likeValue != null){
				logger.info("set query parameter : "+name);
				Expression<String> likePath = getTypedPath(root, part);
				return builder.like(likePath, likeValue.toString());
			}
		}else if(Type.NOT_LIKE.equals(type)){
			
		}else if(Type.TRUE.equals(type)){

			Object trueValue = paramWrapper.getPropertyValue(name);
			if(trueValue != null){
				logger.info("set query parameter : "+name);
				Expression<Boolean> truePath = getTypedPath(root, part);
				return builder.isTrue(truePath);
			}

		}else if(Type.FALSE.equals(type)){

			Object falseValue = paramWrapper.getPropertyValue(name);
			if(falseValue != null){
				Expression<Boolean> falsePath = getTypedPath(root, part);
				return builder.isFalse(falsePath);
			}
		
		}else if(Type.SIMPLE_PROPERTY.equals(type)){
			Object equalValue = paramWrapper.getPropertyValue(name);
			if(equalValue != null){
				logger.info("set query parameter : "+name);

				Expression<Object> equalPath = getTypedPath(root, part);
				return builder.equal(equalPath , equalValue);
			}
		}else if(Type.NEGATING_SIMPLE_PROPERTY.equals(type)){
			
		}else{
			throw new IllegalArgumentException("Unsupported keyword " + part.getType());
		}
		return null;
	}
	
	@SuppressWarnings("rawtypes")
	private Expression<? extends Comparable> getComparablePath(Root<?> root, Part part) {
		return getTypedPath(root, part);
	}
	private <Y> Expression<Y> getTypedPath(Root<?> root, Part part) {
		return toExpressionRecursively(root, part.getProperty());
	}
	private Expression<Object> toExpressionRecursively(Path<Object> path, PropertyPath property) {
		Path<Object> result = path.get(property.getSegment());
		return property.hasNext() ? toExpressionRecursively(result, property.next()) : result;
	}
	@SuppressWarnings("unchecked")
	private <Y> Expression<Y> toExpressionRecursively(From<?, ?> from, PropertyPath property) {
		if (property.isCollection()) {
			Join<Object, Object> join = from.join(property.getSegment());
			return (Expression<Y>)(property.hasNext() ? toExpressionRecursively((From<?, ?>) join, property.next()) : join);
		} else {
			Path<Object> path = from.get(property.getSegment());
			return (Expression<Y>)(property.hasNext() ? toExpressionRecursively(path, property.next()) : path);
		}
	}

}
