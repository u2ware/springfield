package com.u2ware.springfield.sample.security.user.register;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.validation.Errors;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.sample.security.Users;
import com.u2ware.springfield.validation.EntityValidatorImpl;


@Service
public class UserRegisterValidator extends EntityValidatorImpl<UserRegister, UserRegister>{

	@Autowired @Qualifier("usersRepository")
	private EntityRepository<Users, String> usersRepository;
	
	@Transactional
	public void create(UserRegister target, Errors errors) {
		super.create(target, errors);
		
		if(errors.hasErrors()) return;
		
		if(usersRepository.exists(target.getUsername())){
			errors.rejectValue("username", "errorCode" , "사용중인 아이디 입니다");
		}
		
		if(! target.getPassword1().equals(target.getPassword2())){
			errors.rejectValue("password1", "errorCode" , "비밀번호가 일치하지 않습니다.");
			errors.rejectValue("password2", "errorCode" , "비밀번호가 일치하지 않습니다.");
			
		}
		
	}
	
}
