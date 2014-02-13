package com.u2ware.springfield.config.test2;

import java.io.File;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.InitializingBean;


public class Configurer extends Properties implements InitializingBean{
	
	private static final long serialVersionUID = -8550095308895282801L;

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	
	public static String BEAN_NAME     = "springfield";

	public static String BASE_PACKAGE = BEAN_NAME+".basePackage";
	public static String BASE_LOCALE   = BEAN_NAME+".baseLocale";
	
	public static String MULTIPART_SIZE     = BEAN_NAME+".multipart.size";
	public static String MULTIPART_LOCATION = BEAN_NAME+".multipart.location";
	
	public static String SECURITY_FORM_URL        = BEAN_NAME+".security.formPage";
	public static String SECURITY_FORM_USERNAME   = BEAN_NAME+".security.formUsername";
	public static String SECURITY_FORM_PASSWORD   = BEAN_NAME+".security.formPassword";
	public static String SECURITY_FORM_REMEMBERME = BEAN_NAME+".security.formRememberme";
	public static String SECURITY_LOGIN_URL       = BEAN_NAME+".security.loginUrl";
	public static String SECURITY_LOGOUT_URL      = BEAN_NAME+".security.logoutUrl";
	
	private Properties properties;
	private String basePackage;

	public void setProperties(Properties properties) {
		this.properties = properties;
	}
	public void setBasePackage(String basePackage) {
		this.basePackage = basePackage;
	}
	
	@Override
	public void afterPropertiesSet() throws Exception {
		if(properties != null){
			super.putAll(properties);
		}
		super.put(BASE_PACKAGE, basePackage);
		

		if(! super.containsKey(BASE_LOCALE)) {super.put(BASE_LOCALE, "ko");}

		if(! super.containsKey(MULTIPART_SIZE)) {super.put(MULTIPART_SIZE, 11111000l);}
		if(! super.containsKey(MULTIPART_LOCATION)) {super.put(MULTIPART_LOCATION, new File(System.getProperty("java.io.tmpdir"), "/springfield").getAbsolutePath());}
		
		if(! super.containsKey(SECURITY_FORM_URL)) {super.put(SECURITY_FORM_URL, "/security/user/loginForm.html");}
		if(! super.containsKey(SECURITY_FORM_USERNAME)) {super.put(SECURITY_FORM_USERNAME, "j_username");}
		if(! super.containsKey(SECURITY_FORM_PASSWORD)) {super.put(SECURITY_FORM_PASSWORD, "j_password");}
		if(! super.containsKey(SECURITY_FORM_REMEMBERME)) {super.put(SECURITY_FORM_REMEMBERME, "_spring_security_remember_me");}
		if(! super.containsKey(SECURITY_LOGIN_URL)) {super.put(SECURITY_LOGIN_URL, "/j_spring_security_check");}
		if(! super.containsKey(SECURITY_LOGOUT_URL)) {super.put(SECURITY_LOGOUT_URL, "/j_spring_security_logout");}
	}
	
	
	
	
	
	
	
	
	
	
	/*
	private Properties properties;
	private String basePackage;
	private String baseLocale = "ko";
	private File uploadDirectory = ;
	private long uploadSize = 11111000l;

	public Properties getBaseProperties() {
		return properties;
	}
	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	public String getBasePackage() {
		
		String f = null;
		if(properties != null && properties.containsKey(SPRINGFIELD_BASE_PACKAGE)){
			f = properties.getProperty(SPRINGFIELD_BASE_PACKAGE);
		}else{
			f =  basePackage;
		}
		logger.warn("springfieldBasePackage: "+f);
		
		return f;
	}
	public String getBaseLocale() {
		String f = null;
		if(properties != null && properties.containsKey(SPRINGFIELD_BASE_LOCALE)){
			f = properties.getProperty(SPRINGFIELD_BASE_LOCALE);
		}else{
			f = baseLocale;
		}
		logger.warn("springfieldBaseLocale: "+f);
		return f;
	}
	
	public File getUploadDirectory() {
		
		File f = null;
		if(properties != null && properties.containsKey(SPRINGFIELD_MULTIPART_LOCATION)){
			f = new File(properties.getProperty(SPRINGFIELD_MULTIPART_LOCATION));
		}else{
			f =  uploadDirectory;
		}
		logger.warn("springfieldUploadDirectory: "+f);
		
		return f;
	}
	
	public Long getUploadSize() {
		Long f = null;
		if(properties != null && properties.containsKey(SPRINGFIELD_MULTIPART_SIZE)){
			f = Long.parseLong(properties.getProperty(SPRINGFIELD_MULTIPART_SIZE));
		}else{
			f =  uploadSize;
		}
		logger.warn("springfieldUploadSize: "+f);
		
		return 11111000l;
	}	
*/


	
}
