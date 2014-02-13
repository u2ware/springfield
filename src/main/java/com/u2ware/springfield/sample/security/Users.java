package com.u2ware.springfield.sample.security;

import java.util.Collection;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.springframework.security.core.GrantedAuthority;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;
import com.u2ware.springfield.security.authentication.SaltedUserDetails;



@Springfield(strategy=Strategy.JPA_REPOSITORY_ONLY)
@Entity
public class Users implements SaltedUserDetails{

	private static final long serialVersionUID = 5482145308642802692L;

	public Users(){
	}
	public Users(String username){
		this.username = username;
	}
	
	@Id 
	@Getter @Setter private @NotNull String username;
	@Getter @Setter private @NotNull String password;
	@Getter @Setter private @NotNull boolean enabled = true;
	@Getter @Setter private boolean accountNonExpired = true;
	@Getter @Setter private boolean accountNonLocked = true;
	@Getter @Setter private boolean credentialsNonExpired = true;
	@Getter @Setter private @NotNull String salt;
	@Getter @Setter private @NotNull String description;
	@Getter @Setter private @NotNull String role;

	@Transient
	public Collection<? extends GrantedAuthority> getAuthorities() {
		return Role.valueOf(role).getAuthorities();
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((username == null) ? 0 : username.hashCode());
		return result;
	}
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Users other = (Users) obj;
		if (username == null) {
			if (other.username != null)
				return false;
		} else if (!username.equals(other.username))
			return false;
		return true;
	}
	
	@Override
	public String toString() {
		return "Users [username=" + username 
				+ ", password=" + password
				+ ", description=" + description
				+ ", enabled=" + enabled 
				+ ", role=" + role 
				+ ", salt=" + salt + "]";
	}
	
	
	
}
