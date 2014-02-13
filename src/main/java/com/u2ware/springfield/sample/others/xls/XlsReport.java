package com.u2ware.springfield.sample.others.xls;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.joda.time.DateTime;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.format.annotation.NumberFormat;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;
import com.u2ware.springfield.sample.part1.step2.JpaBean;
import com.u2ware.springfield.view.multipart.MultipartFileBean;



@Springfield(
	strategy=Strategy.JPA,
	entity=JpaBean.class,
	methodLevelMapping={"find"},
	attributesCSV="webmvc.view.extension.none={xlsView}"
)
@Entity
public class XlsReport implements MultipartFileBean{

	
	@NotNull @Id
	private @Getter @Setter Integer intValue;

	@NotNull
	private @Getter @Setter String stringValue;	

	@NotNull @NumberFormat(pattern="0.0000")
	private @Getter @Setter Float floatValue;	
	
	@NotNull @DateTimeFormat(pattern="yyyy-MM-dd")
	@org.hibernate.annotations.Type(type="org.joda.time.contrib.hibernate.PersistentDateTime")
	private @Getter @Setter DateTime dateTimeValue;	

	public String getContentFile() {
		return null;
	}

	public String getContentName() {
		return "사용자_"+new DateTime().toString("yyyy년MM월dd일HH시mm분ss초")+".xls";
	}

	public String getContentType() {
		return null;
	}

	public Long getContentSize() {
		return new Long(0);
	}
	
	@Transient 
	private @Getter @Setter boolean download;
}
