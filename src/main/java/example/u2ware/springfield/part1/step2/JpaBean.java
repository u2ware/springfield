package example.u2ware.springfield.part1.step2;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

@Springfield(
	strategy=Strategy.JPA
)
@Entity
public @ToString class JpaBean {

	@Id
	@GeneratedValue
	private @Getter @Setter Integer id;
	private @Getter @Setter String password;
	private @Getter @Setter String name;
	private @Getter @Setter String address;

	public JpaBean() {

	}
	public JpaBean(Integer id) {
		this.id = id;
	}
	public JpaBean(String password, String name, String address) {
		this.password = password;
		this.name = name;
		this.address = address;
	}
}