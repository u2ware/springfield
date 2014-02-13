package com.u2ware.springfield.sample.security.member.password;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.sample.security.AuthenticationContext;
import com.u2ware.springfield.sample.security.Users;
import com.u2ware.springfield.validation.EntityValidatorImpl;


@Component
public class MemberPasswordValidator extends EntityValidatorImpl<MemberPassword, MemberPassword>{

	@Autowired
	protected AuthenticationContext authenticationContext;

	@Autowired @Qualifier("usersRepository")
	private EntityRepository<Users, String> usersRepository;

	@Override
	public void create(MemberPassword target, Errors errors) {
		super.create(target, errors);
		if(errors.hasErrors()) return;
		
		
		try{
			authenticationContext.getUsername(target.getOldPassword());
		}catch(Exception e){
			e.printStackTrace();
			errors.rejectValue("oldPassword", "errorCode" , "비밀번호가 틀렸습니다.");
		}
		
		if(! target.getNewPassword1().equals(target.getNewPassword2())){
			errors.rejectValue("newPassword1", "errorCode" , "새로운 비밀번호가 일치하지 않습니다.");
			errors.rejectValue("newPassword2", "errorCode" , "새로운 비밀번호가 일치하지 않습니다.");
		}

		if(errors.hasErrors()) return;

		if(! usersRepository.exists(target.getUsername())){
			errors.rejectValue("username", "errorCode" , "등록된 사용자가 아닙니다.");
		}
	}
	
}
