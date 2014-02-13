package com.u2ware.springfield.support.context;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.ObjectFactoryCreatingFactoryBean;

@SuppressWarnings("unchecked")
public class SessionContextBroker extends ObjectFactoryCreatingFactoryBean implements ContextBroker{

	private static final Logger logger = LoggerFactory.getLogger(SessionContextBroker.class);

	private SessionContext sessionContext; 
	
	public void setSessionContext(SessionContext sessionContext) {
		this.sessionContext = sessionContext;
	}

	private SessionContext getSessionContext() {
		if(sessionContext != null) return sessionContext;
		
		try {
			Object obj = super.getObject().getObject();
			logger.info("current session : "+obj.hashCode());
			return  (SessionContext)obj;
		} catch (Exception e) {
			return null;
		}
	}
	

	public <O> void put(O object){
		getSessionContext().put(object);
	}

	public <O> O get(Class<O> type){
		return getSessionContext().get(type);
	}

	public <O> O get(Class<O> type, boolean throwException){
		return getSessionContext().get(type, throwException);
	}
	
	public <O> O remove(Class<O> type) {
		return getSessionContext().remove(type);
	}
}