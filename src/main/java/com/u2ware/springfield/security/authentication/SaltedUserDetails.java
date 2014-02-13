package com.u2ware.springfield.security.authentication;

import org.springframework.security.core.userdetails.UserDetails;

public interface SaltedUserDetails extends UserDetails{

	public Object getSalt();

}
