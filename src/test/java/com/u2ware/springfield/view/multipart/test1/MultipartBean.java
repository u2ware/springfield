package com.u2ware.springfield.view.multipart.test1;

import lombok.Getter;
import lombok.Setter;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;
import com.u2ware.springfield.view.multipart.MultipartFileBean;


@Springfield(
	strategy=Strategy.DTO,
	identity={"contentFile"},
	topLevelMapping="/view/multipart",
	methodLevelMapping={"read.download","read.stream"})
public class MultipartBean implements MultipartFileBean{

	private @Getter @Setter String contentFile;
	private @Getter @Setter String contentName;
	private @Getter @Setter String contentType;
	private @Getter @Setter Long contentSize;
	
}
