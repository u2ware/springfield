package com.u2ware.springfield.domain;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import org.joda.time.DateTime;
import org.springframework.beans.BeanWrapper;
import org.springframework.beans.PropertyAccessorFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.expression.Expression;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.format.annotation.NumberFormat;
import org.springframework.util.ClassUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.ReflectionUtils.FieldCallback;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

public class EntityInformation<T,Q> implements InitializingBean{
	
	//protected final Logger logger = LoggerFactory.getLogger(getClass());

	private String basePackage;
	private Class<T> entityClass;
	private Class<Q> queryClass;
	private String[] identity;
	private String topLevelMapping;
	private String[] methodLevelMapping;
	private String attributesCSV;

	protected EntityInformation(){
	}
	
	public EntityInformation(String basePackage, Class<T> entityClass, Class<Q> queryClass,
			String topLevelMapping, String[] methodLevelMapping, 
			String[] identity, String attributesCSV) {
		super();
		this.basePackage = basePackage;
		this.entityClass = entityClass;
		this.queryClass = queryClass;
		this.identity = identity;
		this.topLevelMapping = topLevelMapping;
		this.methodLevelMapping = methodLevelMapping;
		this.attributesCSV = attributesCSV;
	}
	public String getBasePackage() {
		return basePackage;
	}
	public void setBasePackage(String basePackage) {
		this.basePackage = basePackage;
	}
	public Class<T> getEntityClass() {
		return entityClass;
	}
	public void setEntityClass(Class<T> entityClass) {
		this.entityClass = entityClass;
	}
	public Class<Q> getQueryClass() {
		return queryClass;
	}
	public void setQueryClass(Class<Q> queryClass) {
		this.queryClass = queryClass;
	}
	public String[] getIdentity() {
		return identity;
	}
	public void setIdentity(String[] identity) {
		this.identity = identity;
	}
	public String getTopLevelMapping() {
		return topLevelMapping;
	}
	public void setTopLevelMapping(String topLevelMapping) {
		this.topLevelMapping = topLevelMapping;
	}
	public String[] getMethodLevelMapping() {
		return methodLevelMapping;
	}
	public void setMethodLevelMapping(String[] methodLevelMapping) {
		this.methodLevelMapping = methodLevelMapping;
	}
	public String getAttributesCSV() {
		return attributesCSV;
	}
	public void setAttributesCSV(String attributesCSV) {
		this.attributesCSV = attributesCSV;
	}

	
	//////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////
	public String getEntityPath() {
		return entityPath;
	}
	public String getEntityPath(Object target) {

		BeanWrapper wrapper = PropertyAccessorFactory.forBeanPropertyAccess(target);
		
		
		int i= 0;
		StringBuilder result = new StringBuilder();
		for(String id : identity){

			Object value = wrapper.getPropertyValue(id);

			result.append(value);
			if( i < identity.length -1){result.append("/");}
			i++;
		}
		return result.toString();
	}
	public List<Attribute> getEntityAttributes() {
		return entityAttributes;
	}
	public List<Attribute> getQueryAttributes() {
		return queryAttributes;
	}
	
	
	
	public static class Attribute{
		
		private boolean id;
		private boolean editable;
		private boolean listable;
		private String name;
		private String renderingType;
		private Object renderingObject;

		private Attribute(){
		}			
		public String getName() {
			return name;
		}
		public void setName(String name) {
			this.name = name;
		}
		public boolean isId() {
			return id;
		}
		public void setId(boolean id) {
			this.id = id;
		}
		public boolean isEditable() {
			return editable;
		}
		public void setEditable(boolean editable) {
			this.editable = editable;
		}
		public boolean isListable() {
			return listable;
		}
		public void setListable(boolean listable) {
			this.listable = listable;
		}
		public String getRenderingType() {
			return renderingType;
		}
		public void setRenderingType(String renderingType) {
			this.renderingType = renderingType;
		}
		public Object getRenderingObject() {
			return renderingObject;
		}
		public void setRenderingObject(Object renderingObject) {
			this.renderingObject = renderingObject;
		}
		@Override
		public String toString() {
			return "Attribute [id=" + id + ", name=" + name + "]";
		}
	}

	//////////////////////////////////////////////////
	//
	//////////////////////////////////////////////////
	private List<Attribute> entityAttributes;
	private List<Attribute> queryAttributes;
	private String entityPath;
	
	@Override
	public void afterPropertiesSet() throws Exception {
		
		this.entityAttributes = new ArrayList<Attribute>();
		doWithFields(entityAttributes, entityClass, null, null);

		this.queryAttributes = new ArrayList<Attribute>();
		if(! ClassUtils.isAssignable(entityClass, queryClass)){
			doWithFields(queryAttributes, queryClass, null, null);
		}

		
		if(ObjectUtils.isEmpty(this.identity)){
			String[] array = {};
			for(Attribute attr : entityAttributes){
				if(attr.isId() && attr.isEditable()){
					array = StringUtils.addStringToArray(array, attr.getName());
				}
			}
			this.identity = array;
		}
		
		
		
		int i= 0;
		StringBuilder identityPath = new StringBuilder();
		for(String id : identity){
			identityPath.append("{"+id+"}");
			if( i < identity.length -1){identityPath.append("/");}
			i++;
		}
		this.entityPath = identityPath.toString();
	}
	
	
	private void doWithFields(final List<Attribute> attrs, final Class<?> type, final String parent, final Boolean parentId){

		ReflectionUtils.doWithFields(type, new FieldCallback(){

			public void doWith(Field field) throws IllegalArgumentException, IllegalAccessException {

				boolean isId = false;
				if(parentId == null){
					isId = isId(field, identity);
				}else{
					isId = parentId.booleanValue();
				}
				boolean keepGoing = canKeepGoing(field);
				
				if(keepGoing){
					Attribute attr = createAttribute(parent, field, isId, false, ! StringUtils.hasText(parent));
					attrs.add(attr);
					//logger.debug(""+attr);
					
					doWithFields(attrs, field.getType(), field.getName(), isId );
					
				}else{
					if(canStop(field)){
						Attribute attr = createAttribute(parent, field, isId, true, ! StringUtils.hasText(parent));
						attrs.add(attr);
						//logger.debug(""+attr);
					}
				}
			}});
	}
	


	private boolean isId(Field field, String[] identity){
		
		if(! ObjectUtils.isEmpty(this.identity)){
			boolean isId = ObjectUtils.containsElement(identity, field.getName());
			if(isId){
				return isId;
			}
		}
		
		javax.persistence.Id jpaId = field.getAnnotation(javax.persistence.Id.class);
		javax.persistence.EmbeddedId jpaEmbeddedId = field.getAnnotation(javax.persistence.EmbeddedId.class);
		org.springframework.data.annotation.Id springDataId = field.getAnnotation(org.springframework.data.annotation.Id.class);

		return jpaId != null || jpaEmbeddedId != null || springDataId != null;
	}
	
	private boolean canKeepGoing(Field field){
		String fieldPackage = ClassUtils.getPackageName(field.getType());
		return ! field.getType().isEnum() && fieldPackage.indexOf(basePackage) > -1;
	}
	
	private boolean canStop(Field field){
		return ! "serialVersionUID".equals(field.getName());
	}	
	
	
	private Attribute createAttribute(String parent, Field field, boolean id, boolean editable, boolean listable){
		
		String renderingType = null;
		Object renderingObject = null;
		
		if(field.getType().isEnum()){
			renderingType = "radio";
		
			String contents = "T("+field.getType().getName()+").values()";
			ExpressionParser parser = new SpelExpressionParser();
			Expression exp = parser.parseExpression(contents);
			renderingObject = exp.getValue();
			
			
		}else if(ClassUtils.isAssignable(MultipartFile.class, field.getType())){
			renderingType = "file";
		
		}else if(ClassUtils.isAssignable(Number.class, field.getType())){
			renderingType = "number";
			
		}else if(
				ClassUtils.isAssignable(DateTime.class, field.getType()) || 
				ClassUtils.isAssignable(java.util.Date.class, field.getType()) || 
				ClassUtils.isAssignable(java.sql.Date.class, field.getType()) || 
				ClassUtils.isAssignable(java.sql.Time.class, field.getType()) || 
				ClassUtils.isAssignable(java.sql.Timestamp.class, field.getType())  
				){
			renderingType = "date";

		}else if(field.isAnnotationPresent(DateTimeFormat.class)){
			renderingType = "date";
			DateTimeFormat dateTimeFormat = field.getAnnotation(DateTimeFormat.class);
			renderingObject = dateTimeFormat.pattern();

		}else if(field.isAnnotationPresent(NumberFormat.class)){
			renderingType = "number";
		
			NumberFormat numberFormat = field.getAnnotation(NumberFormat.class);
			renderingObject = numberFormat.pattern();
		}

		
		String name = StringUtils.hasText(parent) ? parent+"."+field.getName() : field.getName();
		Attribute a =  new Attribute();
		a.setName(name);
		a.setId(id);
		a.setEditable(editable);
		a.setListable(listable);
		a.setRenderingType(renderingType);
		a.setRenderingObject(renderingObject);
		
		return a;
	}
}

