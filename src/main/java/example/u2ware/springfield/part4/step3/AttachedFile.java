package example.u2ware.springfield.part4.step3;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import org.springframework.web.multipart.MultipartFile;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;
import com.u2ware.springfield.support.multipart.UploadFile;

@Springfield(
	strategy=Strategy.JPA,
	methodLevelMapping={"*","read.stream","read.download"}
)
@Entity
public class AttachedFile implements UploadFile{
	
	@Transient 
	private @Getter @Setter MultipartFile multipartFile;
	
	@Id @GeneratedValue
	private @Getter @Setter Integer id;
	private @Getter @Setter String contentFile;
	private @Getter @Setter String contentName;
	private @Getter @Setter String contentType;
	private @Getter @Setter long contentSize;
}