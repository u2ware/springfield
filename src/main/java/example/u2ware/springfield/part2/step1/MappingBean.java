package example.u2ware.springfield.part2.step1;

import lombok.Getter;
import lombok.Setter;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

@Springfield(
	strategy=Strategy.DTO, 
	methodLevelMapping={
		"*","*.do",
		"findForm.json","findForm.xml","findForm.xls",
		"read.json","read.xml","read.xls"
	},
	identity={"code"}
)
public class MappingBean {

	private @Getter @Setter Integer code;
	private @Getter @Setter String name;
}
