package com.u2ware.springfield.sample.security.admin.authority;

import java.util.Collection;

import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.springframework.security.core.GrantedAuthority;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;
import com.u2ware.springfield.repository.QueryMethod;
import com.u2ware.springfield.sample.security.Role;

@Springfield(
	strategy=Strategy.DTO,
	methodLevelMapping={"findForm","updateForm","update"},
	identity="username"
)
@QueryMethod("findBy")
public class AdminAuthority {

	@Getter @Setter private @NotNull String username;
	@Getter @Setter private @NotNull Role authorityGroup;

	public AdminAuthority() {

	}

	public AdminAuthority(String username, Collection<? extends GrantedAuthority> authorities) {
		this.username = username;
	}
}