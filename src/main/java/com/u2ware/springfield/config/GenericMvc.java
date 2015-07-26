package com.u2ware.springfield.config;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface GenericMvc {

	/**
	 * @return entity type
	 */
	Class<?> value() default Class.class;
	
	/**
	 * @return Controller root request mapping pattern
	 */
	String requestMappingRootPatternValue() default "";

	/**
	 * @return Controller root request mapping pattern
	 */
	String requestMappingUniquePatternValue() default "";
	
	
	@Target({ElementType.TYPE})
	@Retention(RetentionPolicy.RUNTIME)
	@Documented
	public @interface Controller {

		Class<?> value() default Class.class;
		String requestMappingRootPatternValue() default "";
		String requestMappingUniquePatternValue() default "";
	}
	
	@Target({ElementType.TYPE})
	@Retention(RetentionPolicy.RUNTIME)
	@Documented
	public @interface Service {

		Class<?> value() default Class.class;

	}

	@Target({ElementType.TYPE})
	@Retention(RetentionPolicy.RUNTIME)
	@Documented
	public @interface Repository {

		Class<?> value() default Class.class;

	}

	
}