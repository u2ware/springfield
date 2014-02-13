package com.u2ware.springfield.security.authorization;

import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.security.authentication.AuthenticationTrustResolverImpl;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.FilterInvocation;
import org.springframework.security.web.access.expression.WebSecurityExpressionRoot;
import org.springframework.util.AntPathMatcher;
import org.springframework.web.context.ServletContextAware;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;

import com.u2ware.springfield.security.Navigation;
import com.u2ware.springfield.security.NavigationVisitor;

public class AccessDecisionInterceptor implements HandlerInterceptor, ServletContextAware {

	protected final Log logger = LogFactory.getLog(getClass());

	protected final String name = getClass().getName();

    private ServletContext servletContext;
    
	public void setServletContext(ServletContext servletContext) {
		this.servletContext = servletContext;
	}

	/////////////////////////////////////////////////////////////////
	//
	/////////////////////////////////////////////////////////////////
	@Override
	public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
		return true;
	}

	@Override
	public void postHandle(HttpServletRequest request, HttpServletResponse response, Object handler, ModelAndView modelAndView) throws Exception {
    	if(modelAndView == null) return;
    	//if(modelAndView.getViewName().startsWith("redirect:")) return;
    	if(modelAndView.getModel().containsKey(name)) return;
 
		logger.debug("Navigate Decide Start "+ request.getServletPath()+ " "+request.getMethod());

    	WebSecurityExpressionRoot sec = createWebSecurityExpressionRoot(request, response);
    	Navigation navigation = createNavigation(request, response);

    	modelAndView.getModel().put("sec", sec);
    	modelAndView.getModel().put("navigation", navigation);
        
        logger.debug("Navigate Decide End "+ request.getServletPath()+ " "+request.getMethod());
	}

	@Override
	public void afterCompletion(HttpServletRequest request,HttpServletResponse response, Object handler, Exception ex)throws Exception {

	}
	
	///////////////////////////////////////////
	//
	///////////////////////////////////////////
	private Navigation createNavigation(HttpServletRequest request, HttpServletResponse response){

		Navigation target = new Navigation();
		Navigation source = (Navigation)servletContext.getAttribute(Navigation.OBJECT_NAME);
		BeanUtils.copyProperties(source, target);
    	Authentication auth = SecurityContextHolder.getContext().getAuthentication();

    	Visitor visitor = new Visitor(auth, request, response);
    	target.travel(visitor);
    	
    	return target;
	}
	
	private WebSecurityExpressionRoot createWebSecurityExpressionRoot(HttpServletRequest request, HttpServletResponse response){
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication(); 
        
        if(authentication != null){
            FilterInvocation filterInvocation = new FilterInvocation(request, response, new FilterChain() { 
                public void doFilter(ServletRequest request, ServletResponse response) throws IOException, ServletException { 
                    throw new UnsupportedOperationException(); 
                } 
            }); 
            WebSecurityExpressionRoot sec = new WebSecurityExpressionRoot(authentication, filterInvocation); 
            sec.setTrustResolver(new AuthenticationTrustResolverImpl()); 
            return sec;
        }
        return null;
	}
	
	
	
	
	///////////////////////////////////////////
	//
	///////////////////////////////////////////
	private class Visitor implements NavigationVisitor{

		private AntPathMatcher antPathMatcher = new AntPathMatcher();
		
		private AuthorityExpressionRoot accessExpressionRoot;
		private String requestPath;
		
		public Visitor(Authentication authentication, HttpServletRequest request, HttpServletResponse response){
			if(authentication != null){
				this.accessExpressionRoot = new AuthorityExpressionRoot(authentication, request, response);
			}
			this.requestPath = request.getServletPath();
		}

		@Override
		public void visit(Navigation n) {
			updateSelected(n);
			updateHide(n);
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
	}
	
}

