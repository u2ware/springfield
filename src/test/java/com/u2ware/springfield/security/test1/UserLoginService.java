package com.u2ware.springfield.security.test1;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.web.authentication.AuthenticationFailureHandler;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.repository.EntityRepository;


@Component
public class UserLoginService implements UserDetailsService , AuthenticationFailureHandler, AuthenticationSuccessHandler{

	private final Logger logger = LoggerFactory.getLogger(getClass());

	@Autowired @Qualifier("userRepository")
	private EntityRepository<User, String> userRepository;

	
	@Override
	@Transactional
	public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
		logger.debug("loadUserByUsername : "+username);
		logger.debug("loadUserByUsername : "+username);
		User user = userRepository.findOne(username);
		if (user == null) {
			throw new DataRetrievalFailureException("Query returned no results for user '" + username + "'");
		}
		logger.debug("loadUserByUsername : "+user);
		return user;
	}


	@Override
	public void onAuthenticationSuccess(HttpServletRequest request,
			HttpServletResponse response, Authentication authentication)
			throws IOException, ServletException {
		logger.debug("onAuthenticationSuccess : ");
		logger.debug("onAuthenticationSuccess : ");
		logger.debug("onAuthenticationSuccess : ");
	}


	@Override
	public void onAuthenticationFailure(HttpServletRequest request,
			HttpServletResponse response, AuthenticationException exception)
			throws IOException, ServletException {
		logger.debug("onAuthenticationFailure");
		logger.debug("onAuthenticationFailure");
		logger.debug("onAuthenticationFailure");
		logger.debug("onAuthenticationFailure");
	}
}
