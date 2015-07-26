package com.u2ware.springfield.config;

import javax.persistence.EntityManager;

import org.springframework.data.jpa.mapping.JpaMetamodelMappingContext;
import org.springframework.data.jpa.mapping.JpaPersistentEntity;
import org.springframework.data.jpa.mapping.JpaPersistentProperty;
import org.springframework.data.mapping.PersistentProperty;
import org.springframework.data.mapping.SimplePropertyHandler;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;

import com.google.common.base.Strings;

public class RequestMappingPatternResolverBean {
	
	public JpaMetamodelMappingContext context;
	
	public RequestMappingPatternResolverBean(EntityManager em){
		if(em != null){
			this.context = new JpaMetamodelMappingContext(em.getMetamodel());
		}
	}

	public String getRequestMappingRootPatternValue(Class<?> domainClass, String value){

		String rootPatternValue = value;
		if(Strings.isNullOrEmpty(rootPatternValue)){
			rootPatternValue = ClassUtils.getPackageName(domainClass);
//					+"."+ClassUtils.getShortNameAsProperty(domainClass);
			rootPatternValue = StringUtils.replace(rootPatternValue, ".", "/");
		}
		if(rootPatternValue.startsWith("/")){
			rootPatternValue = rootPatternValue.substring(1);
		}
		return rootPatternValue;
	}
	public String getRequestMappingUniquePatternValue(Class<?> entityClass, String value){

		if(context == null){
			return "{id}";
		}
		
		if(! Strings.isNullOrEmpty(value)){
			return value;
		}
		
		final StringBuilder uniquePatternValue = new StringBuilder();
		
    	JpaPersistentEntity<?> entity = context.getPersistentEntity(entityClass);
    	JpaPersistentProperty property = entity.getIdProperty();

    	if(property.isAssociation()){

    		final JpaPersistentEntity<?> id = context.getPersistentEntity(property.getActualType());
			id.doWithProperties(new SimplePropertyHandler(){
				public void doWithPersistentProperty(PersistentProperty<?> property) {
					uniquePatternValue.append("{");
					uniquePatternValue.append(id.getName());
					uniquePatternValue.append(".");
					uniquePatternValue.append(property.getName());
					uniquePatternValue.append("}|");
				}
			});
    		
    	}else{
			uniquePatternValue.append("{");
			uniquePatternValue.append(property.getName());
			uniquePatternValue.append("}|");
    	}
		//logger.debug(uniquePatternValue);
		return uniquePatternValue.substring(0, uniquePatternValue.length()-1);
	}
}
