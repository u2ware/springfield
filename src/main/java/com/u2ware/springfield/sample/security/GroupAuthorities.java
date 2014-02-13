package com.u2ware.springfield.sample.security;

import java.io.Serializable;

import javax.persistence.Embeddable;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.springframework.security.core.GrantedAuthority;

@Entity
@SuppressWarnings("serial")
public class GroupAuthorities implements GrantedAuthority{

	public GroupAuthorities(){
	}
	public GroupAuthorities(String authority){
		this.primary = new Primary(authority);
	}

	@EmbeddedId 
	@Getter @Setter private Primary primary;

	@Transient
	public String getAuthority() {
		return primary != null ? primary.getAuthority() : null;
	}


	@Embeddable
	public static class Primary implements Serializable {
		public Primary(){
		}
		public Primary(String authority){
			this.authority =authority;
		}
		@Getter @Setter private @NotNull String groupId;
		@Getter @Setter private @NotNull String authority;
	}
}