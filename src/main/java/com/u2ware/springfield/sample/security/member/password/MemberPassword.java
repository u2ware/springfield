package com.u2ware.springfield.sample.security.member.password;

import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

@Springfield(
	strategy=Strategy.DTO,
	methodLevelMapping={"createForm","create"},
	attributesCSV="webmvc.view.method.create={read}",
	identity="username"
)
public class MemberPassword {

	@Getter @Setter private @NotNull String username;
	
	@Getter @Setter private @NotNull String oldPassword;
	@Getter @Setter private @NotNull String newPassword1;
	@Getter @Setter private @NotNull String newPassword2;
	
}
