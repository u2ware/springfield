package com.u2ware.springfield;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.Filter;
import javax.servlet.ServletException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.mock.web.MockFilterConfig;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.u2ware.springfield.support.i18n.LocaleChangeFilter;

public class AbstractContextWebmvcSecurityTestRoot extends AbstractContextWebmvcTestRoot{

	@Autowired @Qualifier("springSecurityFilterChain")
	protected Filter springSecurityFilterChain;
	
	
	@Override
	protected MockMvc createMockMvc(){
		
		return MockMvcBuilders.webAppContextSetup(applicationContext)
				.addFilters(createLocaleChangeFilter())
				.addFilters(springSecurityFilterChain)
				.build();
	}
	
	protected Filter createLocaleChangeFilter() {
		try{
			LocaleChangeFilter localeChangeFilter = new LocaleChangeFilter();
			localeChangeFilter.init(new MockFilterConfig(applicationContext.getServletContext()));
			return localeChangeFilter;
		}catch(ServletException e){
			e.printStackTrace();
			return null;
		}
	}
	
	 
	
	protected MockHttpSession login() throws Exception {
		MockHttpSession httpSession = new MockHttpSession(); 
		
 		String principal = "a";
		String credentials = "a";
		List<GrantedAuthority> authorities = new ArrayList<GrantedAuthority>();
		authorities.add(new SimpleGrantedAuthority("ROLE_ANONYMOUS"));
		authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
		authorities.add(new SimpleGrantedAuthority("ROLE_ADMIN"));
		
		
		UsernamePasswordAuthenticationToken authentication = new UsernamePasswordAuthenticationToken(principal, credentials, authorities);
		httpSession.setAttribute(HttpSessionSecurityContextRepository.SPRING_SECURITY_CONTEXT_KEY,  SecurityContextHolder.getContext());
		SecurityContextHolder.getContext().setAuthentication(authentication);
		logger.debug("===========================================================");
		logger.debug(""+SecurityContextHolder.getContext().getAuthentication());
		logger.debug("===========================================================");
		
		return httpSession;
	}
	

	
}
