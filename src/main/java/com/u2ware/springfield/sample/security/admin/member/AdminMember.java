package com.u2ware.springfield.sample.security.admin.member;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;
import com.u2ware.springfield.sample.security.Users;

@Springfield(
	strategy=Strategy.JPA,
	entity=Users.class
)
public class AdminMember {

	
}
