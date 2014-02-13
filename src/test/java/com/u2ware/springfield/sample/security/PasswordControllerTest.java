package com.u2ware.springfield.sample.security;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;

import com.u2ware.springfield.sample.ApplicationContextTestRoot;
import com.u2ware.springfield.sample.security.user.register.UserRegister;
import com.u2ware.springfield.service.EntityService;

public class PasswordControllerTest extends ApplicationContextTestRoot{

	MockHttpSession httpSession = new MockHttpSession(); 

	@Before
	public void login() throws Exception {
		//this.mockMvc = MockMvcBuilders.webAppContextSetup(applicationContext).addFilters(springSecurityFilterChain).build();
	
 		String principal = "1";
		String credentials = "1";
		List<GrantedAuthority> authorities = new ArrayList<GrantedAuthority>();
		authorities.add(new SimpleGrantedAuthority("ROLE_ANONYMOUS"));
		authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
		authorities.add(new SimpleGrantedAuthority("ROLE_ADMIN"));
		
		
		UsernamePasswordAuthenticationToken authentication = new UsernamePasswordAuthenticationToken(principal, credentials, authorities);
		httpSession.setAttribute(HttpSessionSecurityContextRepository.SPRING_SECURITY_CONTEXT_KEY,  SecurityContextHolder.getContext());
		SecurityContextHolder.getContext().setAuthentication(authentication);
		logger.debug("===========================================================");
		logger.debug("===========================================================");
		logger.debug("===========================================================");
		logger.debug(""+SecurityContextHolder.getContext().getAuthentication());
		logger.debug("===========================================================");
	}

	protected @Autowired @Qualifier("userRegisterService") EntityService<UserRegister,UserRegister> userRegisterService;

	@Test
	public void testChange() throws Exception{
		
		this.mockMvc.perform(
				get("/security/member/password/new").session(httpSession))
			//.andDo(print())
			.andExpect(status().isOk());
		

		this.mockMvc.perform(
				post("/security/member/password/new").session(httpSession)
				.param("username", "1")
				.param("oldPassword", "1")
				.param("newPassword1", "b")
				.param("newPassword2", "b"))
				//.andDo(print())
				.andExpect(status().isOk());
		
	}
}


/*
		
		this.mockMvc.perform(
				post("/security/member/password/new").session(httpSession)
				.param("username", "1")
				.param("oldPassword", "1")
				.param("newPassword1", "b")
				.param("newPassword2", "b"))
        

UserRegister entity = new UserRegister();
entity.setUsername("1");
entity.setPassword1("1");
entity.setPassword2("1");
entity.setAuthorityGroup(AuthoritySet.USER);
entity.setDescription("description");

userRegisterService.create(entity);

UsernamePasswordAuthenticationToken authentication = new UsernamePasswordAuthenticationToken(user, user.getPassword(), user.getAuthorities());
httpSession.setAttribute(HttpSessionSecurityContextRepository.SPRING_SECURITY_CONTEXT_KEY,  SecurityContextHolder.getContext());
SecurityContextHolder.getContext().setAuthentication(authentication);
logger.debug("===========================================================");
*/