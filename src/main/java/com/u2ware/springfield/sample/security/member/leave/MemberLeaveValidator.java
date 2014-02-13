package com.u2ware.springfield.sample.security.member.leave;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.sample.security.Users;
import com.u2ware.springfield.validation.EntityValidatorImpl;


@Component
public class MemberLeaveValidator extends EntityValidatorImpl<MemberLeave, MemberLeave>{

	@Autowired @Qualifier("usersRepository")
	private EntityRepository<Users, String> usersRepository;

	@Override
	public void create(MemberLeave target, Errors errors) {
		super.create(target, errors);
		if(errors.hasErrors()) return;
		
		if(! usersRepository.exists(target.getUsername())){
			errors.rejectValue("username", "errorCode" , "등록된 사용자가 아닙니다.");
		}
	}	
}
