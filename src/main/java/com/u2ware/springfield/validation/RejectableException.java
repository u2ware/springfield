package com.u2ware.springfield.validation;

import org.springframework.core.NestedRuntimeException;

public class RejectableException extends NestedRuntimeException{

	private static final long serialVersionUID = -5254415296408548143L;

	private String field;
	private String errorCode;
	private Object[] errorArgs;
	private String defaultMessage;
	
	public RejectableException(String field, String errorCode) {		
		this(field, errorCode, null, null);
	}
	public RejectableException(String field, String errorCode, String defaultMessage) {
		this(field, errorCode, null, defaultMessage);
	}
	public RejectableException(String field, String errorCode,  Object[] errorArgs, String defaultMessage) {
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
