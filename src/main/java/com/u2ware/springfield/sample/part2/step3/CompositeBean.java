package com.u2ware.springfield.sample.part2.step3;

import java.io.Serializable;

import javax.persistence.Embeddable;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

@Springfield(
	strategy=Strategy.JPA
)
@Entity
public @ToString class CompositeBean {

	@EmbeddedId
	@Getter @Setter private ID id = new ID();
	@Getter @Setter private String content;

	public CompositeBean() {

	}
	
	@Embeddable
	public @EqualsAndHashCode static class ID implements Serializable{

		private static final long serialVersionUID = 1L;
		
		@Getter @Setter private String foo;
		@Getter @Setter private String bar;
		@Getter @Setter private String baz;
	}

}