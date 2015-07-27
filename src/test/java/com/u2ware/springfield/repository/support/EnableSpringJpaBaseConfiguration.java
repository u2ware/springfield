package com.u2ware.springfield.repository.support;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.context.annotation.Import;

@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Inherited
@Import(SpringJpaBaseConfigurationRegistrar.class)
public @interface EnableSpringJpaBaseConfiguration {

	String[] value() default {}; //basepackage...
	String[] basePackages() default {};
	Class<?>[] basePackageClasses() default {};
	
}
