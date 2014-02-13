package com.u2ware.springfield.sample.security;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;


@Springfield(strategy=Strategy.JPA_REPOSITORY_ONLY)
@Entity
@SuppressWarnings("serial")
public class Groups implements Serializable{

	@Id @GeneratedValue
	@Getter @Setter private Long id;
	@Getter @Setter private @NotNull String groupName;

	@OneToMany(fetch=FetchType.EAGER,cascade=CascadeType.ALL,orphanRemoval=true,mappedBy="primary.groupId")
	@Fetch(FetchMode.SUBSELECT)
	@Getter @Setter private List<GroupAuthorities> authorities;
	
	@Transient
	public void addAuthority(GroupAuthorities authority) {
		if(authorities == null)
			authorities = new ArrayList<GroupAuthorities>();
		authorities.add(authority);
	}
	
}
