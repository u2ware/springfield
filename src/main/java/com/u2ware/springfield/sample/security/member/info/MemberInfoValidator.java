package com.u2ware.springfield.sample.security.member.info;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.sample.security.Users;
import com.u2ware.springfield.validation.EntityValidatorImpl;


@Component
public class MemberInfoValidator extends EntityValidatorImpl<MemberInfo, MemberInfo>{

	@Autowired @Qualifier("usersRepository")
	private EntityRepository<Users, String> usersRepository;

	@Override
	public void create(MemberInfo target, Errors errors) {
		super.create(target, errors);
		if(errors.hasErrors()) return;
		
		
		logger.debug(usersRepository.exists(target.getUsername()));
		if(! usersRepository.exists(target.getUsername())){
			errors.rejectValue("username", "errorCode" , "등록된 사용자가 아닙니다.");
		}
	}	
}
