package com.u2ware.springfield.controller;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.beans.BeanWrapper;
import org.springframework.beans.PropertyAccessorFactory;
import org.springframework.util.ObjectUtils;

public class HandlerMetamodel<T,Q> {

	protected Class<T> entityClass; 
	protected Class<Q> queryClass; 
	
	protected String topLevelMapping; 
	protected String[] methodLevelMapping;
	protected String[] identity;
	protected String attributesCSV;

	
	public HandlerMetamodel(Class<T> entityClass, Class<Q> queryClass,
			String topLevelMapping, String[] methodLevelMapping,
			String[] identity, String attributesCSV) {
		super();
		this.entityClass = entityClass;
		this.queryClass = queryClass;
		this.topLevelMapping = topLevelMapping;
		this.methodLevelMapping = methodLevelMapping;
		this.identity = identity;
		this.attributesCSV = attributesCSV;
	}
	public Class<T> getEntityClass() {
		return entityClass;
	}
	public Class<Q> getQueryClass() {
		return queryClass;
	}
	public String getTopLevelMapping() {
		return topLevelMapping;
	}
	public String[] getMethodLevelMapping() {
		return methodLevelMapping;
	}
	public String getAttributesCSV() {
		return attributesCSV;
	}
	
	
	public List<Map<String,Object>> getAttributes() {
		return HandlerMetamodelSupport.retrieveClass(entityClass, identity);
	}
	
	public String[] getIdentity(){
		if(! ObjectUtils.isEmpty(identity)) {
			return identity;
		}else{
			Set<String> ids = new HashSet<String>();
			for(Map<String,Object> attr : getAttributes()){
				if("Id".equals(attr.get("persistanceType"))){
					Object name = attr.get("name");
					ids.add(name.toString());
					
				}else if("EmbeddedId".equals(attr.get("persistanceType"))){

					if(attr.get("relationship") != null){
						@SuppressWarnings("unchecked")
						List<Map<String,Object>> childAttrs = (List<Map<String,Object>>)attr.get("relationship");
						for(Map<String,Object> childAttr : childAttrs){
							Object name = attr.get("name")+"."+childAttr.get("name");
							ids.add(name.toString());
						}
					}
				}
			}
			String[] result = new String[ids.size()];
			ids.toArray(result);
			return result;
		}
	}

	public String getIdentityUri()throws Exception{
		
		if(! ObjectUtils.isEmpty(identity)) {
			
			int i= 0;
			StringBuilder result = new StringBuilder();
			for(String id : identity){
				result.append("{").append(id).append("}");
				if( i < identity.length -1){result.append("/");}
			}
			return result.toString();
		}else{
			
			StringBuilder result = new StringBuilder();
			for(Map<String,Object> attr : getAttributes()){
				if("Id".equals(attr.get("persistanceType"))){
					
					Object name = attr.get("name");
					Object value = "{"+name+"}";
					result.append(value);
					
				}else if("EmbeddedId".equals(attr.get("persistanceType"))){

					if(attr.get("relationship") != null){
						@SuppressWarnings("unchecked")
						List<Map<String,Object>> childAttrs = (List<Map<String,Object>>)attr.get("relationship");
						int size = childAttrs.size();
						int i= 0;
						for(Map<String,Object> childAttr : childAttrs){

							Object name = attr.get("name")+"."+childAttr.get("name");
							Object value = "{"+name+"}";
							result.append(value);
							
							if( i < size -1){result.append("/");}
							i++;
						}
					}
				}
			}
			return result.toString();
		}
	}
	
	public String getIdentityUri(Object target) throws Exception{
		if(! ObjectUtils.isEmpty(identity)) {
			BeanWrapper wrapper = PropertyAccessorFactory.forBeanPropertyAccess(target);

			int i= 0;
			StringBuilder result = new StringBuilder();
			for(String id : identity){
				Object value =  wrapper.getPropertyValue(id);
				result.append(value);
				if( i < identity.length -1){result.append("/");}
			}
			return result.toString();
		}else{
			
			BeanWrapper wrapper = PropertyAccessorFactory.forBeanPropertyAccess(target);
			StringBuilder result = new StringBuilder();
			for(Map<String,Object> attr : getAttributes()){
				
				if("Id".equals(attr.get("persistanceType"))){
					
					Object name = attr.get("name");
					Object value = wrapper.getPropertyValue(name.toString());
					result.append(value);

					
				}else if("EmbeddedId".equals(attr.get("persistanceType"))){

					if(attr.get("relationship") != null){
						@SuppressWarnings("unchecked")
						List<Map<String,Object>> childAttrs = (List<Map<String,Object>>)attr.get("relationship");
						int size = childAttrs.size();
						int i= 0;
						for(Map<String,Object> childAttr : childAttrs){

							Object name = attr.get("name")+"."+childAttr.get("name");
							Object value = wrapper.getPropertyValue(name.toString());
							result.append(value);
							
							if( i < size -1){result.append("/");}
							i++;
						}
					}
				}
			}
			return result.toString();
		}
	}
	
	
	
	
}
