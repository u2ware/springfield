package com.u2ware.springfield.sample.security;

import java.util.Collection;
import java.util.HashSet;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;


public enum Role {

	USER(new SimpleGrantedAuthority("ROLE_ANONYMOUS"), new SimpleGrantedAuthority("ROLE_USER")),
	ADMIN(new SimpleGrantedAuthority("ROLE_ANONYMOUS"), new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN"));
	
	private Collection<GrantedAuthority> grantedAuthorities = new HashSet<GrantedAuthority>();

	Role(GrantedAuthority... authorities){
		for(GrantedAuthority authority : authorities){
			grantedAuthorities.add(authority);
		}
	}
	public Collection<? extends GrantedAuthority> getAuthorities(){
		return grantedAuthorities;
	}
}
