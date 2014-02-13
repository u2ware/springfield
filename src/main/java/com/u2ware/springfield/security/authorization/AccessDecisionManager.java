package com.u2ware.springfield.security.authorization;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.servlet.ServletContext;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.security.access.AccessDecisionVoter;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.access.vote.AffirmativeBased;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.FilterInvocation;
import org.springframework.security.web.authentication.SavedRequestAwareAuthenticationSuccessHandler;
import org.springframework.util.AntPathMatcher;
import org.springframework.util.StringUtils;
import org.springframework.web.context.ServletContextAware;

import com.u2ware.springfield.security.Navigation;
import com.u2ware.springfield.security.NavigationVisitor;


public class AccessDecisionManager extends AffirmativeBased implements ServletContextAware {

	protected final Log logger = LogFactory.getLog(getClass());

    @SuppressWarnings("rawtypes")
	public AccessDecisionManager(List<AccessDecisionVoter> decisionVoters) {
        super(decisionVoters);
    }
	
    private ServletContext servletContext;
    
	public void setServletContext(ServletContext servletContext) {
		this.servletContext = servletContext;
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

	        logger.debug("Access Decide End : "+object);
		}catch(AccessDeniedException e){
			logger.debug(e.getMessage());
	        logger.debug("Access Decide End : "+object);
			throw e;
		}
	}
	
	private String createConfigAttribute(Authentication authentication, Object object){
	
		FilterInvocation fi = (FilterInvocation)object;
		Visitor visitor = new Visitor(fi);

		Navigation navigation = (Navigation)servletContext.getAttribute(Navigation.OBJECT_NAME);
		navigation.travel(visitor);
		
		return visitor.getAttribute();
	}
	
	///////////////////////////////////////////
	//
	///////////////////////////////////////////
	private class Visitor implements NavigationVisitor{

		private AntPathMatcher antPathMatcher = new AntPathMatcher();
		private String attribute = "hasRole('ROLE_ANONYMOUS')";
		private String requestPath;

		public Visitor(FilterInvocation fi){
			
			String url = fi.getRequestUrl();
			logger.debug(url);
			int q = url.indexOf("?");
			url = q > 0 ? url.substring(0, q) : url;
			logger.debug(url);
			
			this.requestPath = url;
		}
		
		@Override
		public void visit(Navigation n) {
			if(n.getPattern() == null) return;
			
			if(antPathMatcher.match(n.getPattern(), requestPath)){
				if(StringUtils.hasText(n.getAccess())){
					logger.debug("\t"+ n);
					attribute = n.getAccess();
				}
			}
		}
		public String getAttribute() {
			return attribute;
		}
	}
	
	SavedRequestAwareAuthenticationSuccessHandler j;
}