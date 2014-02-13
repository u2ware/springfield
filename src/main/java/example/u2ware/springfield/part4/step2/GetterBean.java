package example.u2ware.springfield.part4.step2;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

@Springfield(
	strategy=Strategy.JPA,
	topLevelMapping="/part4/getter"
)
@Entity
public class GetterBean {

	@Id
	@NotNull
	private @Getter @Setter Integer code;	
	@NotNull
	private @Getter @Setter String name;
}
