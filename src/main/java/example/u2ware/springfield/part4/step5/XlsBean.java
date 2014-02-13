package example.u2ware.springfield.part4.step5;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.joda.time.DateTime;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.format.annotation.NumberFormat;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;
import com.u2ware.springfield.support.multipart.DownloadFile;

import example.u2ware.springfield.part4.step1.EnumValue;


@Springfield(
	strategy=Strategy.JPA,
	methodLevelMapping={"findForm"},
	attributesCSV="webmvc.view.extension.none={xlsView}"
)
@Entity
public class XlsBean implements DownloadFile{

	
	@NotNull @Id
	private @Getter @Setter Integer intValue;

	@NotNull
	private @Getter @Setter String stringValue;	

	@NotNull @NumberFormat(pattern="0.0000")
	private @Getter @Setter Float floatValue;	
	
	@NotNull @DateTimeFormat(pattern="yyyy-MM-dd")
	@org.hibernate.annotations.Type(type="org.joda.time.contrib.hibernate.PersistentDateTime")
	private @Getter @Setter DateTime dateTimeValue;	

	@NotNull
	private @Getter @Setter EnumValue enumValue;

	public String getContentName() {
		return "사용자_"+dateTimeValue+".xls";
	}

	public String getContentFile() {
		return null;
	}

	public String getContentType() {
		return null;
	}

	public long getContentSize() {
		return 0;
	}
}
