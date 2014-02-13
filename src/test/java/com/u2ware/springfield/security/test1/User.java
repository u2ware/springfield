package com.u2ware.springfield.security.test1;

import java.util.Collection;
import java.util.HashSet;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import com.u2ware.springfield.config.Springfield;

@Springfield(topLevelMapping="/security/test2")
@Entity
public class User implements UserDetails{

	private static final long serialVersionUID = 6489945196607986006L;

	@Id 
	@Getter @Setter private @NotNull String username;
	@Getter @Setter private @NotNull String password;
	@Getter @Setter private boolean enabled = true;
	@Getter @Setter private boolean accountNonExpired = true;
	@Getter @Setter private boolean accountNonLocked = true;
	@Getter @Setter private boolean credentialsNonExpired = true;
	@Getter @Setter private String salt;
	@Getter @Setter private String role;

	@Transient
	public Collection<? extends GrantedAuthority> getAuthorities() {
		return Role.valueOf(role).getAuthorities();
	}
	
	public enum Role {

		USER(new SimpleGrantedAuthority("ROLE_ANONYMOUS"), new SimpleGrantedAuthority("ROLE_USER")),
		ADMIN(new SimpleGrantedAuthority("ROLE_ANONYMOUS"), new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN"));
		
		private Collection<GrantedAuthority> grantedAuthorities = new HashSet<GrantedAuthority>();

		Role(GrantedAuthority... authorities){
			for(GrantedAuthority authority : authorities){
				grantedAuthorities.add(authority);
			}
		}
		public Collection<? extends GrantedAuthority> getAuthorities(){
			return grantedAuthorities;
		}
	}
	
}
