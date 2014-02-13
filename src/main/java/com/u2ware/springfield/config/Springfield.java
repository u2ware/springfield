package com.u2ware.springfield.config;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface Springfield {

	Strategy strategy() default Strategy.DEFAULT_STRATEGY; 

	public enum Strategy{
		DEFAULT_STRATEGY,
		DTO,
		HIBERNATE,
		HIBERNATE_REPOSITORY_ONLY,
		JPA,
		JPA_REPOSITORY_ONLY,
		MONGODB,
		MONGODB_REPOSITORY_ONLY,
		SQLSESSION,
		SQLSESSION_REPOSITORY_ONLY,
	}

	/**
	 * @return Target Entity Class for Repository Layer
	 */
	Class<?> entity() default Class.class;
	
	/**
	 * @return Controller top level mapping
	 */
	String topLevelMapping() default "";

	
	/**
	 * @return Controller method level mapping
	 */
	String[] methodLevelMapping() default {"*"};
	//String[] methodLevelMapping() default {"*","*.json","findForm.xls","read.xls","findForm.xml","read.xml"};


	/**
	 * @return Controller Restful Url
	 */
	String[] identity() default {};


	/**
	 * @return viewAttributesCSV
	 */
	String attributesCSV() default ""; //Format is: attname0={value1},attname1={value1}
	
}



/*
@return Controller Type
 */
