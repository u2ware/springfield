package example.u2ware.springfield.part3.step3;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

import example.u2ware.springfield.part1.step2.JpaBean;

@Springfield(
	strategy=Strategy.DTO,
	identity={"id"}
)
public class Form extends JpaBean{


}