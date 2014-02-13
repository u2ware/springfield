package example.u2ware.springfield.part3.step2;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;
import com.u2ware.springfield.repository.QueryMethod;

import example.u2ware.springfield.part1.step2.JpaBean;

@Springfield(
	strategy=Strategy.JPA, 
	entity=JpaBean.class
)
@QueryMethod("findByIdAndPasswordOrderByNameDesc")
public @ToString class Custom {
	
	private @Getter @Setter Integer id;
	private @Getter @Setter String password;
}