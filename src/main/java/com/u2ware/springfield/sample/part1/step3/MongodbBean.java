package com.u2ware.springfield.sample.part1.step3;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

@Springfield(
	strategy=Strategy.MONGODB
)
@Document
public @ToString class MongodbBean {

	@Id
	@Getter @Setter private String id;
	@Getter @Setter private String password;
	@Getter @Setter private String name;
	@Getter @Setter private Integer age;

	public MongodbBean() {

	}
	public MongodbBean(String id) {
		this.id = id;
	}
	public MongodbBean(String id, String password, String name, Integer age) {
		this.id = id;
		this.password = password;
		this.name = name;
		this.age = age;
	}
}
