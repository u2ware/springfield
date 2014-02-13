package com.u2ware.springfield.security.test1;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import com.u2ware.springfield.AbstractContextWebmvcSecurityTestRoot;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@WebAppConfiguration
public class UserLoginTest extends AbstractContextWebmvcSecurityTestRoot{
	
	@Test
	public void test() throws Exception{
		
		this.mockMvc.perform(post("/security/test2/new")
				.param("username", "username")
				.param("password", "password")

		)
		.andExpect(status().isOk());

	
		this.mockMvc.perform(
				post("/j_spring_security_check")
				.param("j_username", "username")
				.param("j_password", "password")
				.param("_spring_security_remember_me", "true"))
			.andDo(print())
			.andExpect(MockMvcResultMatchers.redirectedUrl("/security/user/loginForm.html"));

	
		this.mockMvc.perform(
				post("/j_spring_security_check")
				.param("j_username", "username")
				.param("j_password", "password2")
				.param("_spring_security_remember_me", "true"))
			.andDo(print())
			.andExpect(MockMvcResultMatchers.redirectedUrl("/security/user/loginForm.html?errorCode=authentication-failure"));
	}

}
