package example.u2ware.springfield.part4.step1;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.hibernate.annotations.Type;
import org.joda.time.DateTime;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.format.annotation.NumberFormat;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

@Springfield(
	strategy=Strategy.JPA
)
@Entity
public class ValidationBean {

	@NotNull @Id
	private @Getter @Setter Integer intValue;

	private @Getter @Setter String stringValue;	

	@NotNull @NumberFormat(pattern="0.0000")
	private @Getter @Setter Float floatValue;	
	
	@NotNull @DateTimeFormat(pattern="yyyy-MM-dd")
	@Type(type="org.joda.time.contrib.hibernate.PersistentDateTime")
	private @Getter @Setter DateTime dateTimeValue;	

	@NotNull
	private @Getter @Setter EnumValue enumValue;	
}