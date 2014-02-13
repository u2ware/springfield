package com.u2ware.springfield.sample.security.user.forgot;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.validation.Errors;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.sample.security.Users;
import com.u2ware.springfield.validation.EntityValidatorImpl;


@Service
public class UserForgotValidator extends EntityValidatorImpl<UserForgot, UserForgot>{

	@Autowired @Qualifier("usersRepository")
	private EntityRepository<Users, String> usersRepository;
	
	
	@Transactional
	public void create(UserForgot target, Errors errors) {
		super.create(target, errors);
		
		if(! usersRepository.exists(target.getUsername())){
			errors.rejectValue("username", "errorCode" , "사용중인 아이디가 아닙니다.");
		}
	}
	
}
