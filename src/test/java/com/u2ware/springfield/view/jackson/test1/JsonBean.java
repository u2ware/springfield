package com.u2ware.springfield.view.jackson.test1;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.validation.constraints.NotNull;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import org.joda.time.DateTime;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.format.annotation.NumberFormat;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.u2ware.springfield.config.Springfield;

@Springfield(topLevelMapping="/view/jackson", methodLevelMapping={"*.json"})
@Entity
public @AllArgsConstructor @NoArgsConstructor @ToString class JsonBean {

	@Id
	private @Getter @Setter Integer intValue;

	@NotNull @JsonProperty("newPropertyName한글")
	private @Getter @Setter String stringValue;	

	@NotNull @NumberFormat(pattern="0.0000") @JsonIgnore
	private @Getter @Setter Float floatValue;	
	
	@NotNull @DateTimeFormat(pattern="yyyy-MM-dd")
	@org.hibernate.annotations.Type(type="org.joda.time.contrib.hibernate.PersistentDateTime")
	private @Getter @Setter DateTime dateTimeValue;	
	
}
