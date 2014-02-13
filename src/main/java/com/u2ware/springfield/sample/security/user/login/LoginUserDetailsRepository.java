package com.u2ware.springfield.sample.security.user.login;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.sample.security.Authorities;
import com.u2ware.springfield.sample.security.Groups;
import com.u2ware.springfield.sample.security.Users;
import com.u2ware.springfield.security.authentication.UserDetailsRepository;

@Service
public class LoginUserDetailsRepository implements UserDetailsRepository{

	protected final Log logger = LogFactory.getLog(getClass());

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
}
