package com.u2ware.springfield.security.rememberme;

import java.util.Map;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ApplicationContextException;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.rememberme.PersistentTokenBasedRememberMeServices;
import org.springframework.security.web.authentication.rememberme.PersistentTokenRepository;

public class SimpleRememberMeServices extends PersistentTokenBasedRememberMeServices implements ApplicationContextAware, InitializingBean{

    public SimpleRememberMeServices(String key, UserDetailsService userDetailsService) {
		super(key, userDetailsService,null);
    }

	private ApplicationContext applicationContext;
	private PersistentTokenRepository persistentTokenRepository;
	
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		this.applicationContext = applicationContext;
	}
	public void setPersistentTokenRepository(PersistentTokenRepository persistentTokenRepository) {
		this.persistentTokenRepository = persistentTokenRepository;
	}
	
	@SuppressWarnings("deprecation")
	public void afterPropertiesSet() throws Exception {
		if(persistentTokenRepository == null){
			persistentTokenRepository = autoPersistentTokenRepository();
		}
		setTokenRepository(persistentTokenRepository);
		super.afterPropertiesSet();
	}

	private PersistentTokenRepository autoPersistentTokenRepository() {
        Map<String,?> beans = applicationContext.getBeansOfType(PersistentTokenRepository.class);

        if (beans.size() == 0) {
            throw new ApplicationContextException("No PersistentTokenRepository registered.");
        } else if (beans.size() > 1) {
            throw new ApplicationContextException("More than one PersistentTokenRepository registered.");
        }
        return (PersistentTokenRepository) beans.values().toArray()[0];
	}
}
