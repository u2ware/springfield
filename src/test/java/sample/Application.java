package sample;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class Application {

	protected Log logger = LogFactory.getLog(getClass());

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}
}