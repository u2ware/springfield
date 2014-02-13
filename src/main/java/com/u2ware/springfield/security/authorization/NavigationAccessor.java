package com.u2ware.springfield.security.authorization;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.FilterInvocation;
import org.springframework.util.AntPathMatcher;
import org.springframework.util.StringUtils;

public class NavigationAccessor {

	private static final Logger logger = LoggerFactory.getLogger(NavigationAccessor.class);
	

	public static final String DEFAULT_ACCESS = "hasRole('ROLE_ANONYMOUS')";

	private AntPathMatcher antPathMatcher = new AntPathMatcher();
	private AuthorityExpressionRoot accessExpressionRoot;
	private String attribute = DEFAULT_ACCESS;
	private String requestPath;

	public NavigationAccessor(Authentication authentication, FilterInvocation fi){
		if(authentication != null){
			this.accessExpressionRoot = new AuthorityExpressionRoot(authentication, fi.getHttpRequest(), fi.getHttpResponse());
		}
		String url = fi.getRequestUrl();
		int q = url.indexOf("?");
		url = q > 0 ? url.substring(0, q) : url;
		this.requestPath = url;
	}
	
	public void visit(Navigation n) {
		updateSelected(n);
		updateHide(n);
		updateAccess(n);
	}
	
	private void updateSelected(Navigation n) {
		
		if(n.getPattern() == null) return;
		
		//logger.debug("\t"+n);
		if(antPathMatcher.match(n.getPattern(), requestPath)){
			n.setSelected(true);
			logger.debug("\t"+ n);
		}else{
			n.setSelected(false);
		}
	}
	private void updateHide(Navigation n) {

		if(accessExpressionRoot == null) return;
		if(! n.isAccessible()) return;
		
		boolean visible = accessExpressionRoot.evaluateAsBoolean(n.getAccess());
		n.setHide( !visible);
		logger.debug("\t"+ n);
	}
	private void updateAccess(Navigation n) {
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
