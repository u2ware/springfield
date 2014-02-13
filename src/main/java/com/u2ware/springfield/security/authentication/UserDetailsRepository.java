package com.u2ware.springfield.security.authentication;

import org.springframework.dao.DataAccessException;
import org.springframework.security.core.userdetails.UserDetails;

public interface UserDetailsRepository {

	public UserDetails loadUserByUsername(String username) throws DataAccessException;
	
}
