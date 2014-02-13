package com.u2ware.springfield.security.rememberme;

import java.util.Date;
import java.util.Map;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ApplicationContextException;
import org.springframework.security.web.authentication.rememberme.PersistentRememberMeToken;
import org.springframework.security.web.authentication.rememberme.PersistentTokenRepository;

public class PersistentTokenRepositoryDetector implements PersistentTokenRepository, ApplicationContextAware, InitializingBean{

	private ApplicationContext applicationContext;
	private PersistentTokenRepository tokenRepository;
	
	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		this.applicationContext = applicationContext;
	}
	
	@Override
	public void afterPropertiesSet() throws Exception {
		try{
			Map<String,?> beans = applicationContext.getBeansOfType(PersistentTokenRepository.class);

	        if (beans.size() == 0) {
	            throw new ApplicationContextException("No PersistentTokenRepository registered.");
	        } else if (beans.size() > 1) {
	            throw new ApplicationContextException("More than one PersistentTokenRepository registered.");
	        }
	        this.tokenRepository = (PersistentTokenRepository) beans.values().toArray()[0];
		}catch(Exception e){
		}
	}


	@Override
	public void createNewToken(PersistentRememberMeToken token) {
		if(tokenRepository == null) return;
		tokenRepository.createNewToken(token);
	}

	@Override
	public void updateToken(String series, String tokenValue, Date lastUsed) {
		if(tokenRepository == null) return;
		tokenRepository.updateToken(series, tokenValue, lastUsed);
	}

	@Override
	public PersistentRememberMeToken getTokenForSeries(String seriesId) {
		if(tokenRepository == null) return null;
		return tokenRepository.getTokenForSeries(seriesId);
	}

	@Override
	public void removeUserTokens(String username) {
		if(tokenRepository == null) return;
		tokenRepository.removeUserTokens(username);
	}
}
