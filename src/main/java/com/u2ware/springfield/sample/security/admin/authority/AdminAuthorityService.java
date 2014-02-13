package com.u2ware.springfield.sample.security.admin.authority;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.sample.security.Authorities;
import com.u2ware.springfield.sample.security.Users;
import com.u2ware.springfield.service.EntityServiceImpl;


@Service
public class AdminAuthorityService extends EntityServiceImpl<AdminAuthority, AdminAuthority>{

	@Autowired @Qualifier("usersRepository")
	private EntityRepository<Users, String> usersRepository;
	
	@Autowired @Qualifier("authoritiesRepository")
	private EntityRepository<Authorities, Integer> authoritiesRepository;


		
}
