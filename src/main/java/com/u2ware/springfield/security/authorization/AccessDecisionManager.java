package com.u2ware.springfield.security.authorization;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.AccessDecisionVoter;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.access.vote.AffirmativeBased;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.FilterInvocation;


public class AccessDecisionManager extends AffirmativeBased {

	private final Logger logger = LoggerFactory.getLogger(getClass());

    @SuppressWarnings("rawtypes")
	public AccessDecisionManager(List<AccessDecisionVoter> decisionVoters) {
        super(decisionVoters);
    }
	
    @Autowired
    private NavigationFactory navigationFactory;

	public void setNavigationFactory(NavigationFactory navigationFactory) {
		this.navigationFactory = navigationFactory;
	}

	@Override
	public void decide(Authentication authentication, Object object, Collection<ConfigAttribute> configAttributes)throws AccessDeniedException {

		try{
	        logger.warn("AccessDecision Request : "+object);
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

	        logger.warn("AccessDecision Response : "+newAttribute);
		}catch(AccessDeniedException e){
	        logger.warn("AccessDecision Response : ", e);
			throw e;
		}catch(Exception e){
	        logger.warn("AccessDecision Response : ", e);
		}
	}
	
	private String createConfigAttribute(Authentication authentication, Object object) {
		
		try{
			FilterInvocation fi = (FilterInvocation)object;
			if(fi.getHttpRequest() == null || fi.getHttpResponse() == null){
				return NavigationAccessor.DEFAULT_ACCESS;
			}
			
	        Navigation navigation = navigationFactory.resolveNavigation(fi.getHttpRequest());
			NavigationAccessor accessor = new NavigationAccessor(authentication, fi);
			navigation.travel(accessor);
			fi.getHttpRequest().setAttribute(Navigation.OBJECT_NAME, navigation);

			return accessor.getAttribute();
		}catch(Exception e){
	        logger.warn("createConfigAttribute : ", e);
			return NavigationAccessor.DEFAULT_ACCESS;
		}
	}
	


}