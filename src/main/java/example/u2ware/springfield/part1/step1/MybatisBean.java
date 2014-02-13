package example.u2ware.springfield.part1.step1;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

@Springfield(
	strategy=Strategy.SQLSESSION,
	identity={"id"}
)
public @ToString class MybatisBean {

	private @Getter @Setter Integer id;
	private @Getter @Setter String password;
	private @Getter @Setter String name;
	private @Getter @Setter String address;

	public MybatisBean() {

	}
	public MybatisBean(Integer id) {
		this.id = id;
	}
	public MybatisBean(Integer id, String password, String name, String address) {
		this.id = id;
		this.password = password;
		this.name = name;
		this.address = address;
	}
}
