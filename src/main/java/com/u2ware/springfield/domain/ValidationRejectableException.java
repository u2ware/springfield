package com.u2ware.springfield.domain;

import org.springframework.core.NestedRuntimeException;

public class ValidationRejectableException extends NestedRuntimeException{

	private static final long serialVersionUID = -5254415296408548143L;

	private String field;
	private String errorCode;
	private Object[] errorArgs;
	private String defaultMessage;
	
	public ValidationRejectableException(String errorCode) {
		this(null, errorCode, null, null);
	}
	public ValidationRejectableException(String errorCode, Object[] errorArgs) {
		this(null, errorCode, errorArgs, null);
	}
	public ValidationRejectableException(String field, String errorCode) {
		this(field, errorCode, null, null);
	}
	public ValidationRejectableException(String field, String errorCode, Object[] errorArgs) {
		this(field, errorCode, errorArgs, null);
	}
	public ValidationRejectableException(String field, String errorCode, String defaultMessage) {
		this(field, errorCode, null, defaultMessage);
	}
	public ValidationRejectableException(String field, String errorCode,  Object[] errorArgs, String defaultMessage) {
		super(errorCode);
		this.field = field;
		this.errorCode = errorCode;
		this.errorArgs = errorArgs;
		this.defaultMessage = defaultMessage;
	}
	
	public String getField() {
		return field;
	}
	public String getErrorCode() {
		return errorCode;
	}
	public Object[] getErrorArgs() {
		return errorArgs;
	}
	public String getDefaultMessage() {
		return defaultMessage;
	}
}
