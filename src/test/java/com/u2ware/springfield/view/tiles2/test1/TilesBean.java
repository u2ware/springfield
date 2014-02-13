package com.u2ware.springfield.view.tiles2.test1;

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

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.view.spreadsheet.XlsIgnore;
import com.u2ware.springfield.view.spreadsheet.XlsProperty;

@Springfield(topLevelMapping="/view/tiles2", methodLevelMapping={"*.tiles"})
@Entity
public @AllArgsConstructor @NoArgsConstructor @ToString class TilesBean {

	@Id
	private @Getter @Setter Integer intValue;

	@NotNull @XlsProperty("newPropertyName")
	private @Getter @Setter String stringValue;	

	@NotNull @NumberFormat(pattern="0.0000") @XlsIgnore
	private @Getter @Setter Float floatValue;	
	
	@NotNull @DateTimeFormat(pattern="yyyy-MM-dd")
	@org.hibernate.annotations.Type(type="org.joda.time.contrib.hibernate.PersistentDateTime")
	private @Getter @Setter DateTime dateTimeValue;	
	
	
	
}
