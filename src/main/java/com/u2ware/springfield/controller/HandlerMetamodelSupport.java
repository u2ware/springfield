package com.u2ware.springfield.controller;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.expression.Expression;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.format.annotation.NumberFormat;
import org.springframework.util.ClassUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.ReflectionUtils.FieldCallback;
import org.springframework.web.multipart.MultipartFile;

public class HandlerMetamodelSupport {

	protected static final Log logger = LogFactory.getLog(HandlerMetamodelSupport.class);
	
	private static Map<Class<?> , List<Map<String,Object>>> attributesMap = new HashMap<Class<?> , List<Map<String,Object>>>();

	public static List<Map<String,Object>> retrieveClass(final Class<?> clazz, String[] identity) {
		
		if(attributesMap.containsKey(clazz)){
			return attributesMap.get(clazz);
		}else{
			final List<Map<String,Object>> attributes = new ArrayList<Map<String,Object>>();
			ReflectionUtils.doWithFields(clazz, new FieldCallback(){
				public void doWith(Field field) throws IllegalArgumentException, IllegalAccessException {
					if("serialVersionUID".equals(field.getName())) return;
					attributes.add( retrieveField(field));
				}
			});
			
			if(! ObjectUtils.isEmpty(identity)) {
				for(Map<String,Object> attr : attributes){
					String name = attr.get("name").toString();
					if(ObjectUtils.containsElement(identity, name)){
						attr.put("persistanceType", "Id");
					}
				}
			}
			
			
			attributesMap.put(clazz, attributes);
			return attributes;
		}
	}

	private static Map<String,Object> retrieveField(Field field) throws IllegalArgumentException, IllegalAccessException{

		String name = retrieveAttributeName(field);
		Class<?> type = retrieveAttributeType(field);
		String persistanceType = retrieveAttributePersistanceType(field);
		String displayType = retrieveAttributeDisplayType(field);
		Object displayOption = retrieveAttributeDisplayOption(field);
		List<?> relationship = retrieveAttributeRelationship(field);
		
		Map<String,Object> attr = new HashMap<String,Object>();
		attr.put("name", name);
		attr.put("type", type.toString());
		attr.put("persistanceType", persistanceType);
		attr.put("displayType", displayType);
		attr.put("displayOption", displayOption);
		attr.put("relationship", relationship);

		return attr;
	}
	private static String retrieveAttributeName(Field field) throws IllegalArgumentException, IllegalAccessException{
		return field.getName();
	}
	private static Class<?> retrieveAttributeType(Field field) throws IllegalArgumentException, IllegalAccessException{
		return field.getType();
	}

	private static String retrieveAttributePersistanceType(Field field) {
		javax.persistence.Id jpaId = field.getAnnotation(javax.persistence.Id.class);
		javax.persistence.EmbeddedId jpaEmbeddedId = field.getAnnotation(javax.persistence.EmbeddedId.class);
		javax.persistence.ManyToOne jpaManyToOne = field.getAnnotation(javax.persistence.ManyToOne.class);
		javax.persistence.OneToMany jpaOneToMany = field.getAnnotation(javax.persistence.OneToMany.class);

		org.springframework.data.annotation.Id springDataId = field.getAnnotation(org.springframework.data.annotation.Id.class);
		
		//SpringfieldId springfieldId = field.getAnnotation(SpringfieldId.class);
		//SpringfieldIds springfieldIds = field.getAnnotation(SpringfieldIds.class);

		
		if(jpaId != null) return "Id";
		if(jpaEmbeddedId != null) return "EmbeddedId";
		if(jpaManyToOne != null) return "ManyToOne";
		if(jpaOneToMany != null) return "OneToMany";
		if(springDataId != null) return "Id";
		//if(springfieldId != null) return "Id";
		//if(springfieldIds != null) return "EmbeddedId";
		
		return "Basic";
	}
	
	private static List<Map<String,Object>> retrieveAttributeRelationship(Field field) throws IllegalArgumentException, IllegalAccessException{
		
		javax.persistence.EmbeddedId embeddedId = field.getAnnotation(javax.persistence.EmbeddedId.class);
		javax.persistence.ManyToOne manyToOne = field.getAnnotation(javax.persistence.ManyToOne.class);
		//OneToMany oneToMany = field.getAnnotation(OneToMany.class);
		//SpringfieldIds springfieldIds = field.getAnnotation(SpringfieldIds.class);

		if(embeddedId != null) return retrieveClass(field.getType(), null);
		if(manyToOne != null) return retrieveClass(field.getType(), null);
		//if(oneToMany != null) findMetamodel(field.getType());
		//if(springfieldIds != null) return retrieveClass(field.getType());
		return null;
	}

	
	private static String retrieveAttributeDisplayType(Field field) throws IllegalArgumentException, IllegalAccessException{

		if(ClassUtils.isAssignable(Enum.class, field.getType())){
			return "Radio";
		}else if(ClassUtils.isAssignable(MultipartFile.class, field.getType())){
			return "File";
		}else{
		}

		DateTimeFormat dateTimeFormat = field.getAnnotation(DateTimeFormat.class);
		NumberFormat numberFormat = field.getAnnotation(NumberFormat.class);
		if(dateTimeFormat != null){
			return "Datetime";
		}else if(numberFormat != null){
			return "Number";
		}else{
		}
		return "Nomal";
	}
	private static Object retrieveAttributeDisplayOption(Field field) throws IllegalArgumentException, IllegalAccessException{
		if(ClassUtils.isAssignable(Enum.class, field.getType())){
			String contents = "T("+field.getType().getName()+").values()";
			ExpressionParser parser = new SpelExpressionParser();
			Expression exp = parser.parseExpression(contents);
			return exp.getValue();
		}else if(ClassUtils.isAssignable(MultipartFile.class, field.getType())){
		}else{
		}

		DateTimeFormat dateTimeFormat = field.getAnnotation(DateTimeFormat.class);
		NumberFormat numberFormat = field.getAnnotation(NumberFormat.class);
		if(dateTimeFormat != null){
			return dateTimeFormat.pattern();
		}else if(numberFormat != null){
			return numberFormat.pattern();
		}else{
		}

		return null;
	}
}
