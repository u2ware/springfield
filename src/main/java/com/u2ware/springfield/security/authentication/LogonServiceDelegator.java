package com.u2ware.springfield.security.authentication;

import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ApplicationContextException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.web.authentication.AuthenticationFailureHandler;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.security.web.authentication.SavedRequestAwareAuthenticationSuccessHandler;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationFailureHandler;

public class LogonServiceDelegator implements UserDetailsService, AuthenticationSuccessHandler, AuthenticationFailureHandler,  ApplicationContextAware, InitializingBean{

	//private static final Logger logger = LoggerFactory.getLogger(LogonServiceDelegator.class);

	private ApplicationContext applicationContext;
	private String defaultTargetUrl;
	private String defaultFailureUrl;
	private LogonService logonService;
	private AuthenticationSuccessHandler successHandler ;
	private AuthenticationFailureHandler failureHandler;
	
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		this.applicationContext = applicationContext;
	}
	public void setLogonService(LogonService logonService) {
		this.logonService = logonService;
	}
	public String getDefaultTargetUrl() {
		return defaultTargetUrl;
	}
	public void setDefaultTargetUrl(String defaultTargetUrl) {
		this.defaultTargetUrl = defaultTargetUrl;
	}
	public void setDefaultFailureUrl(String defaultFailureUrl) {
		this.defaultFailureUrl = defaultFailureUrl;
	}
	/////////////////////////////////////////////////
	//
	/////////////////////////////////////////////////
	@Override
	public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
		try{
			return logonService.loadUserByUsername(username);
		}catch(Throwable e){
            throw new UsernameNotFoundException("Username "+username+" not found", e);
		}
	}
	
	@Override
	public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response, Authentication authentication) throws IOException, ServletException {
		try{
			successHandler.onAuthenticationSuccess(request, response, authentication);
			logonService.onAuthenticationSuccess(request, response, authentication);
		}catch(Throwable e){
            throw new ServletException("on Authentication Success " );
		}
	}

	@Override
	public void onAuthenticationFailure(HttpServletRequest request, HttpServletResponse response, AuthenticationException exception) throws IOException, ServletException {
		try{
			failureHandler.onAuthenticationFailure(request, response, exception);
			logonService.onAuthenticationFailure(request, response, exception);
		}catch(Throwable e){
            throw new ServletException("on Authentication Failure");
		}
	}
	
	
	/////////////////////////////////////////////////
	//
	/////////////////////////////////////////////////
	public void afterPropertiesSet() throws Exception {
		if(logonService == null){
			logonService = autoDetectedLogonService();
		}
		
		SavedRequestAwareAuthenticationSuccessHandler h1 = new SavedRequestAwareAuthenticationSuccessHandler();
		h1.setDefaultTargetUrl(defaultTargetUrl);
		this.successHandler = h1;
		
		SimpleUrlAuthenticationFailureHandler h2 = new SimpleUrlAuthenticationFailureHandler();
		h2.setDefaultFailureUrl(defaultFailureUrl);
		this.failureHandler = h2;
	}

	/////////////////////////////////////////////////
	//
	/////////////////////////////////////////////////
	private LogonService autoDetectedLogonService() {
        try{
    		Map<String,?> beans = applicationContext.getBeansOfType(LogonService.class);

            if (beans.size() == 0) {
                throw new ApplicationContextException("No UserDetailsRepository registered.");
            } else if (beans.size() > 1) {
                throw new ApplicationContextException("More than one UserDetailsRepository registered.");
            }
            return (LogonService) beans.values().toArray()[0];

        }catch(Exception e){
        	return new LogonService(){
				public UserDetails loadUserByUsername(String username) throws Exception {
					
					Collection<GrantedAuthority> authorities = new HashSet<GrantedAuthority>();
					authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
					//authorities.add(new SimpleGrantedAuthority("ROLE_ANONYMOUS"));
					//authorities.add(new SimpleGrantedAuthority("ROLE_ADMIN"));
					return new User(username, "password", authorities);
				}
				public void onAuthenticationSuccess(HttpServletRequest request,
						HttpServletResponse response,
						Authentication authentication) throws Exception {
					//Do nothing..
					
				}
				public void onAuthenticationFailure(HttpServletRequest request,
						HttpServletResponse response,
						AuthenticationException exception) throws Exception {
					//Do nothing..
					
				}};
        }
	}

	
	
}
