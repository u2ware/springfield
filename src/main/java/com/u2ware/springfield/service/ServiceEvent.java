package com.u2ware.springfield.service;

import java.util.EventObject;

public class ServiceEvent extends EventObject{

	private static final long serialVersionUID = 4930663828547334407L;

	private Class<?> targetClass;
	private String targetMethod;
	
	public ServiceEvent(Class<?> targetClass, String targetMethod, Object source) {
		super(source);
	}

	public Class<?> getTargetClass() {
		return targetClass;
	}

	public String getTargetMethod() {
		return targetMethod;
	}

	@Override
	public String toString() {
		return "ProcessEvent [targetClass=" + targetClass + ", targetMethod="+ targetMethod + "]";
	}
}
