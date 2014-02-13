package com.u2ware.springfield.sample.security;

import java.io.Serializable;

import javax.persistence.Embeddable;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import org.springframework.security.core.GrantedAuthority;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

@Springfield(strategy=Strategy.JPA_REPOSITORY_ONLY)
@Entity
public class Authorities implements GrantedAuthority{

	private static final long serialVersionUID = -5573779386357203026L;

	public Authorities(){
	}
	public Authorities(String username, String authority){
		this.primary = new Primary(new Users(username), authority);
	}
	
	@EmbeddedId 
	@Getter @Setter private Primary primary;

	@Transient
	public String getAuthority() {
		return primary != null ? primary.getAuthority() : null;
	}

	@Override
	public String toString() {
		return primary.toString();
	}



	@Embeddable 
	public static class Primary implements Serializable {

		private static final long serialVersionUID = -6949463592639319376L;

		public Primary(){
		}
		public Primary(Users username, String authority){
			this.username = username;
			this.authority = authority;
		}
		
		@ManyToOne(fetch=FetchType.EAGER)
		@JoinColumn(name="username")
		private @Getter @Setter Users username;
		private @Getter @Setter String authority ;
		@Override
		public String toString() {
			return authority;
		}
		
	}

}
