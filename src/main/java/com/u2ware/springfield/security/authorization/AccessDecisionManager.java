package com.u2ware.springfield.security.authorization;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.access.AccessDecisionVoter;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.access.vote.AffirmativeBased;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.FilterInvocation;

public class AccessDecisionManager extends AffirmativeBased {

	private static final Logger logger = LoggerFactory.getLogger(AccessDecisionManager.class);

    @SuppressWarnings("rawtypes")
	public AccessDecisionManager(List<AccessDecisionVoter> decisionVoters) {
        super(decisionVoters);
    }
	
    private NavigationFactory navigationFactory;

	public void setNavigationFactory(NavigationFactory navigationFactory) {
		this.navigationFactory = navigationFactory;
	}

	@Override
	public void decide(Authentication authentication, Object object, Collection<ConfigAttribute> configAttributes)throws AccessDeniedException {


		try{
	        logger.debug("Access Decide Start : "+object);
/*			
			logger.debug("path : "+authentication);
			logger.debug("path : "+authentication.getPrincipal());
			logger.debug("path : "+object);
			for(ConfigAttribute c  : configAttributes){
				logger.debug("configAttributes : "+c);
			}
*/			
			String newAttribute = createConfigAttribute(authentication, object);
			
			Collection<ConfigAttribute> newConfigAttributes = new ArrayList<ConfigAttribute>();
			newConfigAttributes.add(new AuthorityAttribute(newAttribute));
			
			super.decide(authentication, object, newConfigAttributes);

	        logger.debug("Access Decide End : "+newAttribute);
		}catch(AccessDeniedException e){
	        logger.debug("Access Decide End : "+e.getMessage());
			logger.debug(e.getMessage(), e);
			throw e;
		}catch(Exception e){
	        logger.debug("Access Decide End : "+e.getMessage());
			logger.debug(e.getMessage(), e);
		}
	}
	
	private String createConfigAttribute(Authentication authentication, Object object) {
		
		try{
			FilterInvocation fi = (FilterInvocation)object;
			NavigationAccessor accessor = new NavigationAccessor(authentication, fi);

			Navigation navigation = navigationFactory.resolveNavigation(fi.getHttpRequest());
			navigation.travel(accessor);

			return accessor.getAttribute();
		}catch(Exception e){
			return NavigationAccessor.DEFAULT_ACCESS;
		}
	}
	


}