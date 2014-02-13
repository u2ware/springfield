package com.u2ware.springfield.security.authorization;

import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.expression.EvaluationContext;
import org.springframework.expression.Expression;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.security.access.expression.ExpressionUtils;
import org.springframework.security.access.expression.SecurityExpressionHandler;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.FilterInvocation;
import org.springframework.security.web.access.expression.DefaultWebSecurityExpressionHandler;
import org.springframework.security.web.access.expression.WebSecurityExpressionRoot;


public class AuthorityExpressionRoot extends WebSecurityExpressionRoot {

	private final SecurityExpressionHandler<FilterInvocation> expressionHandler = new DefaultWebSecurityExpressionHandler();
	private final SpelExpressionParser parser = new SpelExpressionParser();
	private final EvaluationContext ctx;

	public AuthorityExpressionRoot(Authentication authentication, HttpServletRequest request, HttpServletResponse response) {
		this(authentication, new FilterInvocation(request, response, new FilterChain(){
			public void doFilter(ServletRequest request, ServletResponse response) throws IOException, ServletException{
				throw new UnsupportedOperationException();
			}
		}));
	}
	public AuthorityExpressionRoot(Authentication authentication, FilterInvocation fi) {
		super(authentication, fi);
        ctx = expressionHandler.createEvaluationContext(authentication, fi);
	}
	
	
	public boolean evaluateAsBoolean(AuthorityAttribute config){
        return evaluateAsBoolean(config.getAttribute());
	}

	public boolean evaluateAsBoolean(String authorizeExpressionString){
        Expression authorizeExpression = parser.parseExpression(authorizeExpressionString);
        return ExpressionUtils.evaluateAsBoolean(authorizeExpression, ctx);
	}
	
	/*

	public static AuthorizationExpressionRoot newInstance(FilterInvocation filterInvocation) { 
		AuthorizationExpressionRoot root = null;
		Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if(authentication != null){
            root = new AuthorizationExpressionRoot(authentication, filterInvocation);
            root.setTrustResolver(new AuthenticationTrustResolverImpl());
        }
        return root;
	}
	public static AuthorizationExpressionRoot newInstance(HttpServletRequest request, HttpServletResponse response) { 
		final ServletRequest req = (ServletRequest)request;
		final ServletResponse res = (ServletResponse)response;
		return newInstance(new FilterInvocation(req, res, new FilterChain(){
				public void doFilter(ServletRequest request, ServletResponse response) throws IOException, ServletException{
					throw new UnsupportedOperationException();
				}
			}));
	}
	
	*/
	
	//sec.isAnonymous();
	//sec.isAuthenticated();
	//sec.isFullyAuthenticated();
	//sec.isRememberMe();
	//sec.hasAnyRole(roles);

}
