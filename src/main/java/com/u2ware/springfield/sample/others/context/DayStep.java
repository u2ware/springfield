package com.u2ware.springfield.sample.others.context;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.hibernate.annotations.Type;
import org.joda.time.DateTime;
import org.springframework.format.annotation.DateTimeFormat;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

@Springfield(
	strategy=Strategy.JPA
)
@Entity
public class DayStep {

	@Id
	@Getter @Setter private @NotNull String name;

	@Type(type="org.joda.time.contrib.hibernate.PersistentDateTime")
	@Getter @Setter private @NotNull @DateTimeFormat(pattern="yyyy-MM-dd")  DateTime step;	
}
