package com.u2ware.springfield.repository.mongodb;

import org.springframework.beans.BeanWrapper;
import org.springframework.data.mapping.context.MappingContext;
import org.springframework.data.mapping.context.PersistentPropertyPath;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.mapping.MongoPersistentProperty;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.repository.query.parser.Part;
import org.springframework.data.repository.query.parser.Part.Type;

import com.u2ware.springfield.repository.AbstractPartTreeQuery;

class PartTreeQuery extends AbstractPartTreeQuery<Criteria> {

	public PartTreeQuery(Class<?> entityClass, Object param) {
		super(entityClass, param);
	}

	protected MappingContext<?, MongoPersistentProperty> context;
	
	public Criteria toPredicate(MongoOperations mongoOperations){
		this.context = mongoOperations.getConverter().getMappingContext();
		return super.createCriteria();
	}
	
	////////////////////////////////////////////////////
	//
	////////////////////////////////////////////////////
	@Override
	protected Criteria and(Criteria base, Criteria criteria, BeanWrapper paramWrapper) {
		Criteria result = new Criteria();
		return result.andOperator(base, criteria);
	}
	@Override
	protected Criteria or(Criteria base, Criteria criteria, BeanWrapper paramWrapper) {
		Criteria result = new Criteria();
		return result.orOperator(base, criteria);
	}
	@Override
	protected Criteria create(Part part, BeanWrapper paramWrapper) {
		return from(part, this.key(part), paramWrapper);
	}

	
	////////////////////////////////////////////////////
	//
	////////////////////////////////////////////////////
	private Criteria key(Part part) {
		PersistentPropertyPath<MongoPersistentProperty> path = context.getPersistentPropertyPath(part.getProperty());
		return Criteria.where(path.toDotPath(MongoPersistentProperty.PropertyToFieldNameConverter.INSTANCE));
	}
/*	private Criteria key(Part part, Criteria base) {
		PersistentPropertyPath<MongoPersistentProperty> path = context.getPersistentPropertyPath(part.getProperty());
		return base.and(path.toDotPath(MongoPersistentProperty.PropertyToFieldNameConverter.INSTANCE));
	}
*/	
	private Criteria from(Part part, Criteria criteria, BeanWrapper paramWrapper) {
		
		Type type = part.getType();
		String name = part.getProperty().getSegment();
		//logger.debug(type+"="+name);
		
		if(Type.AFTER.equals(type)){

		}else if(Type.GREATER_THAN.equals(type)){

		}else if(Type.GREATER_THAN_EQUAL.equals(type)){

		}else if(Type.BEFORE.equals(type)){

		}else if(Type.LESS_THAN.equals(type)){

		}else if(Type.LESS_THAN_EQUAL.equals(type)){

		}else if(Type.BETWEEN.equals(type)){

		}else if(Type.IS_NOT_NULL.equals(type)){

		}else if(Type.IS_NULL.equals(type)){

		}else if(Type.NOT_IN.equals(type)){

		}else if(Type.IN.equals(type)){

		}else if(Type.LIKE.equals(type)){

		}else if(Type.STARTING_WITH.equals(type)){

		}else if(Type.ENDING_WITH.equals(type)){

		}else if(Type.CONTAINING.equals(type)){

		}else if(Type.REGEX.equals(type)){

		}else if(Type.EXISTS.equals(type)){

		}else if(Type.TRUE.equals(type)){

		}else if(Type.FALSE.equals(type)){

		}else if(Type.NEAR.equals(type)){

		}else if(Type.WITHIN.equals(type)){

		}else if(Type.SIMPLE_PROPERTY.equals(type)){
			Object equalValue = paramWrapper.getPropertyValue(name);
			if(equalValue != null){ 
				logger.info("set query parameter : "+name);
				return criteria.is(equalValue); 
			}
		}else if(Type.NEGATING_SIMPLE_PROPERTY.equals(type)){
			return null;
		}else{
			throw new IllegalArgumentException("Unsupported keyword!");
		}

		return null;
	}
}
