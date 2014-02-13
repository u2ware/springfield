package com.u2ware.springfield.service;

import javax.swing.event.EventListenerList;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class ServiceEventDispatcher {

	protected final Log logger = LogFactory.getLog(getClass());

	protected EventListenerList listenerList;
	
	public void addEventListener(ServiceEventListener listener){
		if(listenerList == null) 
			listenerList = new EventListenerList();
		listenerList.add(ServiceEventListener.class, listener);
	}
	public void removeEventListener(ServiceEventListener listener){
		if(listenerList != null) 
		listenerList.remove(ServiceEventListener.class, listener);
	}
	
	public void firePreHandle(Class<?> targetClass, String targetMethod, Object source){
		if(listenerList == null) return;
		
		ServiceEventListener[] listeners = listenerList.getListeners(ServiceEventListener.class);
        ServiceEvent e = new ServiceEvent(targetClass, targetMethod, source);
        
        for (ServiceEventListener listener : listeners) {
        	listener.preHandle(e);
        }
	}
	public void firePostHandle(Class<?> targetClass, String targetMethod, Object source){
		if(listenerList == null) return;
		
		ServiceEventListener[] listeners = listenerList.getListeners(ServiceEventListener.class);
        ServiceEvent e = new ServiceEvent(targetClass, targetMethod, source);
        
        for (ServiceEventListener listener : listeners) {
        	listener.postHandle(e);
        }
	}
}
