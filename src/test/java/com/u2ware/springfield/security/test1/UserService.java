package com.u2ware.springfield.security.test1;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.dao.SaltSource;
import org.springframework.security.authentication.encoding.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.service.AbstractEntityService;


@SuppressWarnings("deprecation")
@Service("userService")
public class UserService extends AbstractEntityService<User, User>{

	@Autowired
	private AuthenticationManager authenticationManager;

	@Autowired
	private PasswordEncoder passwordEncoder; 
	
	@Autowired
	private SaltSource saltSource; 

	@Autowired @Qualifier("userRepository")
	private EntityRepository<User, String> userRepository;

	@Autowired 
	private TransactionTemplate transactionTemplate;
	
	@Override
	protected EntityRepository<User, String> getRepository() {
		return userRepository;
	}

	@Override
	protected TransactionTemplate getTransactionTemplate() {
		return transactionTemplate;
	}
	
	
	@Override
	@Transactional
	public User create(User entity) {

		String salt = ""+System.currentTimeMillis();
		String password = passwordEncoder.encodePassword("password", salt);
		
		entity.setSalt(salt);
		entity.setPassword(password);
		entity.setRole("USER");
		
		return super.create(entity);
	}
	
}
