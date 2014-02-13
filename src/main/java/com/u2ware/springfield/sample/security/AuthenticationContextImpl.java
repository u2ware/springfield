package com.u2ware.springfield.sample.security;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.authentication.dao.SaltSource;
import org.springframework.security.authentication.encoding.PasswordEncoder;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

@Component
@SuppressWarnings("deprecation")
public class AuthenticationContextImpl implements AuthenticationContext{
	
	@Autowired(required=false)
	private AuthenticationManager authenticationManager;

	@Autowired(required=false)
	private PasswordEncoder passwordEncoder; 
	
	@Autowired(required=false)
	private SaltSource saltSource; 
	
	/////////////////////////////////////////////////
	//
	/////////////////////////////////////////////////
	public String getPasswordSalt() {
		return ""+System.currentTimeMillis();
	}
	public String getPassword(String password, Object salt) {
		if(passwordEncoder == null) return password;
		return passwordEncoder.encodePassword(password, salt);
	}

	/////////////////////////////////////////////////
	//
	/////////////////////////////////////////////////
	public boolean hasAuthentication()  {
		return SecurityContextHolder.getContext().getAuthentication() != null;
	}

	public Authentication getAuthentication()  {
        return SecurityContextHolder.getContext().getAuthentication();
	}
	
	public Authentication getAuthentication(String password)  {
		if(authenticationManager == null) return null;
		Authentication a = getAuthentication();
        if(a == null) return null;
        authenticationManager.authenticate(new UsernamePasswordAuthenticationToken(a.getName(), password));
        return a;
	}
	public String getUsername() {
		Authentication a = getAuthentication();
		if(a == null) return null;
		return a.getName();
	}
	@Override
	public String getUsername(String password) {
		Authentication a = getAuthentication(password);
		if(a == null) return null;
		return a.getName();
	}
	
	public void logoff()  {
        SecurityContextHolder.clearContext();
	}
}
