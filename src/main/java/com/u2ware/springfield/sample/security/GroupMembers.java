package com.u2ware.springfield.sample.security;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.validation.constraints.NotNull;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

import lombok.Getter;
import lombok.Setter;

@Springfield(strategy=Strategy.JPA_REPOSITORY_ONLY)
@Entity
@SuppressWarnings("serial")
public class GroupMembers implements Serializable{

	@Id @GeneratedValue
	@Getter @Setter private Long id;
	
	@Getter @Setter private @NotNull String username;

	@ManyToOne(fetch=FetchType.EAGER) 
	@Getter @Setter private @NotNull Groups group;
}