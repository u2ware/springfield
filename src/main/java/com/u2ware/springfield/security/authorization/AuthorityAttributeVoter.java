package com.u2ware.springfield.security.authorization;

import java.util.Collection;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.access.AccessDecisionVoter;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.FilterInvocation;
//import org.springframework.security.web.access.expression.WebExpressionConfigAttribute;


public class AuthorityAttributeVoter implements AccessDecisionVoter<FilterInvocation> {

	//private static final Logger logger = LoggerFactory.getLogger(AuthorityAttributeVoter.class);


    public int vote(Authentication authentication, FilterInvocation fi, Collection<ConfigAttribute> attributes) {
        assert authentication != null;
        assert fi != null;
        assert attributes != null;

        AuthorityAttribute attr = findAccessAttribute(attributes);
  
        if (attr == null) {
            return ACCESS_ABSTAIN;
        }
        
        AuthorityExpressionRoot sec = new AuthorityExpressionRoot(authentication, fi);
        int vote = sec.evaluateAsBoolean(attr) ? ACCESS_GRANTED : ACCESS_DENIED;
        
        //logger.debug("vote result is "+vote +" by "+attr.getAttribute());
        
        return vote;
    }
    
    private AuthorityAttribute findAccessAttribute(Collection<ConfigAttribute> attributes) {
        for (ConfigAttribute attribute : attributes) {
            if (attribute instanceof AuthorityAttribute) {
                return (AuthorityAttribute)attribute;
            }
        }
        return null;
    }
    public boolean supports(ConfigAttribute attribute) {
        return true;
    }
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(FilterInvocation.class);
    }
}


