package com.u2ware.springfield.sample.security.user.login;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.sample.security.Authorities;
import com.u2ware.springfield.sample.security.Groups;
import com.u2ware.springfield.sample.security.Users;
import com.u2ware.springfield.security.authentication.LogonService;

@Service
public class LoginUserDetailsRepository implements LogonService{

	private static final Logger logger = LoggerFactory.getLogger(LoginUserDetailsRepository.class);

	@Autowired @Qualifier("usersRepository")
	private EntityRepository<Users, String> usersRepository;

	@Autowired @Qualifier("authoritiesRepository")
	private EntityRepository<Authorities, Integer> authoritiesRepository;

	@Autowired @Qualifier("groupsRepository")
	private EntityRepository<Groups, Integer> groupsRepository;
	
	@Override
	@Transactional
	public UserDetails loadUserByUsername(String username) throws DataAccessException{
		logger.debug("loadUserByUsername : "+username);
		Users user = usersRepository.read(username);
		if (user == null) {
			throw new DataRetrievalFailureException("Query returned no results for user '" + username + "'");
		}

		//List<Authorities> userAuthorities = authoritiesRepository.findAll(new FindByPrimary_Username_Username(username));
		//List<Authorities> groupAuthorities = authoritiesRepository.findAll(new FindByPrimary_Username_Username(username));
		//List<GrantedAuthority> combinedAuthorities = new ArrayList<GrantedAuthority>();
		//combinedAuthorities.addAll(userAuthorities);
		//user.setAuthorities(combinedAuthorities);
		
		logger.debug("loadUserByUsername : "+user);
		return user;
	}
	
	private @AllArgsConstructor class FindByPrimary_Username_Username{
		@Getter @Setter private String primary;
	}

	@Override
	public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response, Authentication authentication) throws Exception {
		logger.debug("onAuthenticationSuccess : ");
		
	}

	@Override
	public void onAuthenticationFailure(HttpServletRequest request, HttpServletResponse response, AuthenticationException exception) throws Exception {
		logger.debug("onAuthenticationFailure : ");
		
	}
}
