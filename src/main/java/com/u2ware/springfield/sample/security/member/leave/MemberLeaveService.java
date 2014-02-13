package com.u2ware.springfield.sample.security.member.leave;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.sample.security.AuthenticationContext;
import com.u2ware.springfield.sample.security.Users;
import com.u2ware.springfield.service.EntityServiceImpl;


@Service
public class MemberLeaveService extends EntityServiceImpl<MemberLeave, MemberLeave>{

	@Autowired
	protected AuthenticationContext authenticationContext;

	@Autowired @Qualifier("usersRepository")
	private EntityRepository<Users, String> usersRepository;

	@Override
	public MemberLeave createForm(MemberLeave entity) {
		String username = authenticationContext.getUsername();
		entity.setUsername(username);
		return entity;
	}	
	
	@Override
	@Transactional
	public MemberLeave create(MemberLeave entity) {

		String username = authenticationContext.getUsername();
		Users user = usersRepository.read(username);
		user.setEnabled(false);
		
		authenticationContext.logoff();
		
		return entity;
	}
}
