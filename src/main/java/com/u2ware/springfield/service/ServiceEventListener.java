package com.u2ware.springfield.service;

import java.util.EventListener;

public interface ServiceEventListener extends EventListener{
	
	public void preHandle(ServiceEvent e);
	public void postHandle(ServiceEvent e);

}