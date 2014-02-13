package com.u2ware.springfield.sample.security.user.register;

import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;
import com.u2ware.springfield.sample.security.Role;

@Springfield(
	strategy=Strategy.DTO,
	methodLevelMapping={"createForm","create"},
	attributesCSV="webmvc.view.method.create={read}",
	identity="username"
)
public class UserRegister {

	@Getter @Setter private @NotNull String username;
	@Getter @Setter private @NotNull String password1;
	@Getter @Setter private @NotNull String password2;
	@Getter @Setter private @NotNull String description;
	@Getter @Setter private @NotNull Role role;
	
	
}
