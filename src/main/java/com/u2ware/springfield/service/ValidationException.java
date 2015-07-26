package com.u2ware.springfield.service;

import org.springframework.core.NestedRuntimeException;

@SuppressWarnings("serial")
public class ValidationException extends NestedRuntimeException{

	private String field;
	private String errorCode;
	private Object[] errorArgs;
	private String defaultMessage;
	
	public ValidationException(String errorCode) {		
		this(null, errorCode, null, null);
	}
	public ValidationException(String field, String errorCode) {		
		this(field, errorCode, null, null);
	}
	public ValidationException(String field, String errorCode, String defaultMessage) {
		this(field, errorCode, null, defaultMessage);
	}
	public ValidationException(String field, String errorCode,  Object[] errorArgs, String defaultMessage) {
		super(errorCode);
		this.field = field;
		this.errorCode = errorCode;
		this.errorArgs = errorArgs;
		this.defaultMessage = defaultMessage;
	}
	
	public String field() {
		return field;
	}
	public String errorCode() {
		return errorCode;
	}
	public Object[] errorArgs() {
		return errorArgs;
	}
	public String defaultMessage() {
		return defaultMessage;
	}
}
