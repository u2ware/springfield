package example.u2ware.springfield.part1;

import com.u2ware.springfield.repository.QueryMethod;

import lombok.Getter;
import lombok.Setter;

@QueryMethod("findByIdAndPasswordAndNameAndAddressOrderByNameDesc")
public class MyQuery {

	public @Getter @Setter Integer id;
	public @Getter @Setter String password;
	public @Getter @Setter String address;
	public @Getter @Setter String name;
	
}
