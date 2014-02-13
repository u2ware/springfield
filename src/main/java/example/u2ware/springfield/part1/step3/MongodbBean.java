package example.u2ware.springfield.part1.step3;


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
	private @Getter @Setter Integer id;
	private @Getter @Setter String password;
	private @Getter @Setter String name;
	private @Getter @Setter String address;

	public MongodbBean() {

	}
	public MongodbBean(Integer id) {
		this.id = id;
	}
	public MongodbBean(Integer id, String password, String name, String address) {
		this.id = id;
		this.password = password;
		this.name = name;
		this.address = address;
	}
}
