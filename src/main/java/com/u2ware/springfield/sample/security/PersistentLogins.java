package com.u2ware.springfield.sample.security;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.hibernate.annotations.Type;
import org.joda.time.DateTime;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;


@Springfield(strategy=Strategy.JPA_REPOSITORY_ONLY)
@Entity
public class PersistentLogins implements Serializable{

	private static final long serialVersionUID = 7976777798641755651L;

	@Id 
	@Getter @Setter private @NotNull String series;

	@Getter @Setter private @NotNull String username;
	@Getter @Setter private @NotNull String token;
	@Type(type="org.joda.time.contrib.hibernate.PersistentDateTime")
	@Getter @Setter private @NotNull DateTime lastUsed;
	
	/////////////////////////////////
	//
	////////////////////////////////
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((series == null) ? 0 : series.hashCode());
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
		PersistentLogins other = (PersistentLogins) obj;
		if (series == null) {
			if (other.series != null)
				return false;
		} else if (!series.equals(other.series))
			return false;
		return true;
	}
	
	@Override
	public String toString() {
		return "PersistentLogins [series=" + series + ", username=" + username
				+ ", token=" + token + ", lastUsed=" + lastUsed + "]";
	}
}
