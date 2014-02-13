package com.u2ware.springfield.sample.security;


public interface AuthenticationContext {

	public String getPasswordSalt();
	public String getPassword(String password, Object salt);

	public String getUsername() ;
	public String getUsername(String password) ;
	public void logoff() ;

	
	//public boolean hasAuthentication() ;
	//public Authentication getAuthentication() ;
	//public Authentication getAuthentication(String password) ;

	
	
	
}
