package com.u2ware.springfield.sample.others.mail;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.MailSender;
import org.springframework.mail.SimpleMailMessage;

import com.u2ware.springfield.sample.ApplicationContextTestRoot;

public class MailSenderTest extends ApplicationContextTestRoot{
	
	@Autowired
	private MailSender mailSender;

	@Test
	public void testSendMail(){

		String from = "api.ibtk.kr@gmail.com";
		String to = "u2ware@orgos.net";
		String subject = "Test";
		String msg = "msg";
		
		
		SimpleMailMessage message = new SimpleMailMessage();

		message.setFrom(from);
		message.setTo(to);
		message.setSubject(subject);
		message.setText(msg);
		mailSender.send(message);	
/*
mail.sender.host=smtp.gmail.com
mail.sender.port=587
mail.sender.username=api.ibtk.kr@gmail.com
mail.sender.password=api.ibtk.kr@01!
*/
		
		
	}
}
