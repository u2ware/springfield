package com.u2ware.springfield.sample.home.manager;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import org.hibernate.annotations.Type;
import org.joda.time.DateTime;
import org.springframework.format.annotation.DateTimeFormat;

import lombok.Getter;
import lombok.Setter;

@Entity
public class Project {

	@EmbeddedId @Valid
	private @Getter @Setter ProjectId projectId;

	@NotNull
	private @Getter @Setter String description;
	
	@Type(type="org.joda.time.contrib.hibernate.PersistentDateTime")
	@Getter @Setter private @NotNull @DateTimeFormat(pattern="yyyy-MM-dd")  DateTime start;	
	

	@Type(type="org.joda.time.contrib.hibernate.PersistentDateTime")
	@Getter @Setter private @NotNull @DateTimeFormat(pattern="yyyy-MM-dd")  DateTime end;	

	@Getter @Setter private @NotNull State state;	
	
}
