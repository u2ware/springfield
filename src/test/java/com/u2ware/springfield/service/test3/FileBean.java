package com.u2ware.springfield.service.test3;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import org.springframework.web.multipart.MultipartFile;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.view.multipart.MultipartFileBean;

@Springfield(topLevelMapping="/service/test3",
		methodLevelMapping={"find", "createForm", "create", "read.download","read.stream"})
@Entity
public class FileBean implements MultipartFileBean{

	@Transient 
	public @Getter @Setter MultipartFile uploadFile;
	
	@Id @GeneratedValue
	private @Getter @Setter Integer id;
	private @Getter @Setter String contentFile;
	private @Getter @Setter String contentName;
	private @Getter @Setter String contentType;
	private @Getter @Setter Long contentSize;
}
