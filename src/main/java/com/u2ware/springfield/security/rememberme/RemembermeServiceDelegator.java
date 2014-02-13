package com.u2ware.springfield.security.rememberme;

import java.util.Date;
import java.util.Map;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ApplicationContextException;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.rememberme.PersistentRememberMeToken;
import org.springframework.security.web.authentication.rememberme.PersistentTokenBasedRememberMeServices;

public class RemembermeServiceDelegator extends PersistentTokenBasedRememberMeServices implements ApplicationContextAware, InitializingBean{

    public RemembermeServiceDelegator(String key, UserDetailsService userDetailsService) {
		super(key, userDetailsService,null);
    }

	private ApplicationContext applicationContext;
	private RemembermeService remembermeService;
	
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		this.applicationContext = applicationContext;
	}
	public void setRemembermeService(RemembermeService remembermeService) {
		this.remembermeService = remembermeService;
	}
	
	@SuppressWarnings("deprecation")
	public void afterPropertiesSet() throws Exception {
		if(remembermeService == null){
			remembermeService = autoDetectedRemembermeService();
		}
		super.setTokenRepository(remembermeService);
		super.afterPropertiesSet();
	}

	private RemembermeService autoDetectedRemembermeService() {
        
		try{
			Map<String,?> beans = applicationContext.getBeansOfType(RemembermeService.class);

	        if (beans.size() == 0) {
	            throw new ApplicationContextException("No PersistentTokenRepository registered.");
	        } else if (beans.size() > 1) {
	            throw new ApplicationContextException("More than one PersistentTokenRepository registered.");
	        }
	        return (RemembermeService) beans.values().toArray()[0];

		}catch(Exception e){
			
			return new RemembermeService(){
				public void createNewToken(PersistentRememberMeToken token) {
				}
				public void updateToken(String series, String tokenValue,Date lastUsed) {
				}
				public PersistentRememberMeToken getTokenForSeries(String seriesId) {
					return null;
				}
				public void removeUserTokens(String username) {
				}};
		}
	}
}
