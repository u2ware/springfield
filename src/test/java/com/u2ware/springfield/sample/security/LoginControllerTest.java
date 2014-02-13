package com.u2ware.springfield.sample.security;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import com.u2ware.springfield.sample.ApplicationContextTestRoot;


public class LoginControllerTest extends ApplicationContextTestRoot{

	@Test
	public void testLogin() throws Exception{

		this.mockMvc.perform(
				post("/security/user/register/new")
				.param("username", "a")
				.param("password1", "a")
				.param("password2", "a")
				.param("role", "USER")
				.param("description", "a")
				)
			.andExpect(status().isOk());
		
		this.mockMvc.perform(
				post("/j_spring_security_check")
				.param("j_username", "a")
				.param("j_password", "a")
				.param("_spring_security_remember_me", "true"))
			.andDo(print())
			.andExpect(MockMvcResultMatchers.redirectedUrl("/security/user/loginForm"));
	
	}
	
	
	
	
}
