package com.u2ware.springfield.config.test2;

import java.util.List;

import javax.validation.MessageInterpolator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.MessageSourceAccessor;
import org.springframework.context.support.ReloadableResourceBundleMessageSource;
import org.springframework.core.Ordered;
import org.springframework.core.io.Resource;
import org.springframework.data.web.PageableHandlerMethodArgumentResolver;
import org.springframework.data.web.SortHandlerMethodArgumentResolver;
import org.springframework.format.FormatterRegistry;
import org.springframework.oxm.xstream.XStreamMarshaller;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;
import org.springframework.validation.Validator;
import org.springframework.validation.beanvalidation.LocalValidatorFactoryBean;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;
import org.springframework.web.servlet.handler.MappedInterceptor;
import org.springframework.web.servlet.i18n.CookieLocaleResolver;
import org.springframework.web.servlet.i18n.LocaleChangeInterceptor;
import org.springframework.web.servlet.view.InternalResourceViewResolver;
import org.springframework.web.servlet.view.UrlBasedViewResolver;
import org.springframework.web.servlet.view.tiles2.TilesView;
import org.thymeleaf.extras.springsecurity3.dialect.SpringSecurityDialect;
import org.thymeleaf.spring3.SpringTemplateEngine;
import org.thymeleaf.spring3.view.ThymeleafViewResolver;

import com.u2ware.springfield.controller.HandlerInterceptor;
import com.u2ware.springfield.controller.HandlerMapping;
import com.u2ware.springfield.security.authorization.NavigationFactory;
import com.u2ware.springfield.security.authorization.NavigationInterceptor;
import com.u2ware.springfield.support.conversion.StringTrimFormatter;
import com.u2ware.springfield.support.resource.ResourcePatternResolverBean;
import com.u2ware.springfield.validation.ValidationMessageInterpolator;
import com.u2ware.springfield.view.EntityViewResolver;
import com.u2ware.springfield.view.EntityViewResolver.ViewResolverSupport;
import com.u2ware.springfield.view.ModelFilter;
import com.u2ware.springfield.view.ModelFilterImpl;
import com.u2ware.springfield.view.jackson.JsonView;
import com.u2ware.springfield.view.multipart.MultipartFileBeanView;
import com.u2ware.springfield.view.spreadsheet.CsvView;
import com.u2ware.springfield.view.spreadsheet.XlsView;
import com.u2ware.springfield.view.thymeleaf.ResourceBasedTemplateResolver;
import com.u2ware.springfield.view.tiles2.TilesConfigurer;
import com.u2ware.springfield.view.xstream.XmlView;

@Configuration
@EnableWebMvc
public class ContextWebmvcBase extends WebMvcConfigurerAdapter{

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	private Configurer configurer;
	public void setConfigurer(Configurer configurer) {
		this.configurer = configurer;
	}

	
	@Override
	public void addArgumentResolvers(List<HandlerMethodArgumentResolver> argumentResolvers) {
		
		SortHandlerMethodArgumentResolver r1 = new SortHandlerMethodArgumentResolver();
		r1.setSortParameter("model_query_pageable.sort");

		PageableHandlerMethodArgumentResolver r2 = new PageableHandlerMethodArgumentResolver(r1);
		r2.setPageParameterName("model_query_pageable.pageNumber");
		r2.setSizeParameterName("model_query_pageable.pageSize");
		
		argumentResolvers.add(r2);
	}
	
		
	@Override
	public void addResourceHandlers(ResourceHandlerRegistry registry) {
		 registry.addResourceHandler("/resources/**").addResourceLocations("/resources/");
	}

	@Override
	public void addFormatters(FormatterRegistry registry) {
		registry.addFormatter(new StringTrimFormatter());
	}
	
	@Override
	public Validator getValidator() {
		return mvcValidator();
	}

	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	@Bean
	public MappedInterceptor springfieldMappedInterceptor1(){
		MappedInterceptor b = new MappedInterceptor(null, springfieldHandlerInterceptor1());
		return b;
	}
	@Bean
	public MappedInterceptor springfieldMappedInterceptor2(){
		MappedInterceptor b = new MappedInterceptor(null, springfieldHandlerInterceptor2());
		return b;
	}
	@Bean
	public MappedInterceptor springfieldMappedInterceptor3(){
		MappedInterceptor b = new MappedInterceptor(null, springfieldHandlerInterceptor3());
		return b;
	}
	@Bean
	public LocaleChangeInterceptor springfieldHandlerInterceptor1(){
		LocaleChangeInterceptor b = new LocaleChangeInterceptor();
		return b;
	}
	@Bean
	public HandlerInterceptor springfieldHandlerInterceptor2(){
		HandlerInterceptor b = new HandlerInterceptor();
		return b;
	}
	@Bean
	public NavigationInterceptor springfieldHandlerInterceptor3(){
		NavigationInterceptor b = new NavigationInterceptor();
		return b;
	}
	
	@Bean
	public LocalValidatorFactoryBean mvcValidator() {
		LocalValidatorFactoryBean v = new LocalValidatorFactoryBean();
		v.setMessageInterpolator(mvcValidatorMessageInterpolator());
		return v;
	}
	@Bean
	public MessageInterpolator mvcValidatorMessageInterpolator() {
		return new ValidationMessageInterpolator();
	}

	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	@Bean
	public HandlerMapping springfieldHandlerMapping() {
		HandlerMapping b = new HandlerMapping();
		return b;
	}

	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	@Bean
	public ResourcePatternResolverBean springfieldNavigationFactoryConfigLocations(){
		ResourcePatternResolverBean b = new ResourcePatternResolverBean();
		b.setPackagesToScan(configurer.getProperty(Configurer.BASE_PACKAGE));
		b.setResourcePatterns("/**/navigation.xml");
		return b;
	}

	@Bean
	public NavigationFactory springfieldNavigationFactory(){
		NavigationFactory b = new NavigationFactory();
		Resource[] r = springfieldNavigationFactoryConfigLocations().getResources();
		if(r != null && r.length > 0){
			b.setConfigLocation(r[0]);
		}
		return b;
	}
	
	
	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	/*
	@Bean
	public CommonsMultipartResolver filterMultipartResolver()  throws Exception{
		CommonsMultipartResolver bean = new CommonsMultipartResolver();
		//bean.setMaxUploadSize(Long.parseLong(configurer.getProperty(SpringfieldConfigurer.MULTIPART_SIZE)));
		bean.setMaxUploadSize(Long.parseLong("11111111"));
		return bean;
	}

	@Bean
	public MultipartFileHandlerImpl springfieldMultipartFileHandler()  throws Exception{
		MultipartFileHandlerImpl bean = new MultipartFileHandlerImpl();
		bean.setDirectory(new File(configurer.getProperty(Configurer.MULTIPART_LOCATION)));
		return bean;
	}
	*/
	
	
	
	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	@Bean
	public ResourcePatternResolverBean messageSourceBasenames()  throws Exception{
		ResourcePatternResolverBean bean = new ResourcePatternResolverBean();
		bean.setPackagesToScan(configurer.getProperty(Configurer.BASE_PACKAGE));
		bean.setResourcePatterns("/**/messages.xml");
		return bean;
	}
	@Bean
	public ReloadableResourceBundleMessageSource messageSource()  throws Exception{
		ReloadableResourceBundleMessageSource bean = new ReloadableResourceBundleMessageSource();
		bean.setDefaultEncoding("UTF-8");
		String[] basenames = messageSourceBasenames().getFilenames();
		if(basenames != null){
			bean.setBasenames(basenames);
		}else{
			String path = "classpath:com/u2ware/springfield/view/thymeleaf/messages";
			bean.setBasenames(path);
		}
		return bean;
	}
	@Bean
	public MessageSourceAccessor messageSourceAccessor()  throws Exception{
		MessageSourceAccessor bean = new MessageSourceAccessor(messageSource(), 
				StringUtils.parseLocaleString(configurer.getProperty(Configurer.BASE_LOCALE))
				);
		return bean;
	}
	
	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	public CookieLocaleResolver springfieldCookieLocaleResolver(){
		CookieLocaleResolver b = new CookieLocaleResolver();
		b.setDefaultLocale(StringUtils.parseLocaleString(configurer.getProperty(Configurer.BASE_LOCALE)));
		return b;
	}
	
	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	@Bean
	public ModelFilter springfieldModelFilter() throws Exception{
		return new ModelFilterImpl();
	}
	
	@Bean
	public MultipartFileBeanView springfieldDownloadView()  throws Exception{
		MultipartFileBeanView bean = new MultipartFileBeanView();
		bean.setModelFilter(springfieldModelFilter());
		bean.setDownload(true);
		return bean;
	}
	
	@Bean
	public ViewResolverSupport springfieldDownloadViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldDownloadView());
		bean.setBaseExtensions("download");
		bean.setExtensions("download");
		return bean;
	}
	
	@Bean
	public MultipartFileBeanView springfieldStreamView()  throws Exception{
		MultipartFileBeanView bean = new MultipartFileBeanView();
		bean.setModelFilter(springfieldModelFilter());
		bean.setDownload(false);
		return bean;
	}
	
	@Bean
	public ViewResolverSupport springfieldStreamViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldStreamView());
		bean.setBaseExtensions("stream");
		bean.setExtensions("stream");
		return bean;
	}
	
	
	@Bean
	public JsonView springfieldJsonView()  throws Exception{
		JsonView bean = new JsonView();
		bean.setModelFilter(springfieldModelFilter());
		bean.setContentType("application/json;charset=UTF-8");
		return bean;
	}
	
	@Bean
	public ViewResolverSupport springfieldJsonViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldJsonView());
		bean.setBaseExtensions("json");
		bean.setExtensions("json");
		return bean;
	}
	
	@Bean
	public XStreamMarshaller springfieldXStreamMarshaller(){
		XStreamMarshaller b = new XStreamMarshaller();
		b.setAutodetectAnnotations(true);
		return b;
	}
	@Bean
	public XmlView springfieldXmlView() throws Exception{
		XmlView bean = new XmlView();
		bean.setModelFilter(springfieldModelFilter());
		bean.setContentType("application/xml;charset=UTF-8");
		bean.setMarshaller(springfieldXStreamMarshaller());
		return bean;
	}
	
	@Bean
	public ViewResolverSupport springfieldXmlViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldXmlView());
		bean.setBaseExtensions("xml");
		bean.setExtensions("xml");
		return bean;
	}
	
	@Bean
	public XlsView springfieldXlsView() throws Exception{
		XlsView bean = new XlsView();
		bean.setModelFilter(springfieldModelFilter());
		bean.setContentType("application/vnd.ms-excel");
		return bean;
	}
	
	@Bean
	public ViewResolverSupport springfieldXlsViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldXlsView());
		bean.setBaseExtensions("xls");
		bean.setExtensions("xls");
		bean.setResourceRequired(true);
		return bean;
	}
	
	@Bean
	public CsvView springfieldCsvView() throws Exception{
		CsvView bean = new CsvView();
		bean.setModelFilter(springfieldModelFilter());
		bean.setContentType("text/csv");
		return bean;
	}
	
	@Bean
	public ViewResolverSupport springfieldCsvViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldCsvView());
		bean.setBaseExtensions("csv");
		bean.setExtensions("csv");
		bean.setResourceRequired(false);
		return bean;
	}
	
	
	
	@Bean
	public InternalResourceViewResolver springfieldJstlView() throws Exception{
		InternalResourceViewResolver bean = new InternalResourceViewResolver();
		bean.setOrder(Ordered.LOWEST_PRECEDENCE);
		return bean;
	}

	@Bean
	public ViewResolverSupport springfieldJstlViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldJstlView());
		bean.setBaseExtensions("jsp");
		bean.setExtensions("jstl");
		bean.setResourceRequired(true);
		return bean;
	}

	@Bean
	public ResourcePatternResolverBean springfieldTilesConfigurerDefinitions()throws Exception{
		ResourcePatternResolverBean bean = new ResourcePatternResolverBean();
		bean.setPackagesToScan(configurer.getProperty(Configurer.BASE_PACKAGE));
		bean.setResourcePatterns("/**/tiles-definitions.xml");
		return bean;
	}
	
	
	@Bean
	public TilesConfigurer springfieldTilesConfigurer()throws Exception{
		TilesConfigurer b = new TilesConfigurer();
		b.setDefinitions(springfieldTilesConfigurerDefinitions().getLocations());
		return b;
	}
	
	@Bean
	public UrlBasedViewResolver springfieldTilesView()throws Exception{
		UrlBasedViewResolver bean = new UrlBasedViewResolver();
		bean.setViewClass(TilesView.class);
		bean.setOrder(Ordered.LOWEST_PRECEDENCE);
		return bean;
	}
	
	@Bean
	public ViewResolverSupport springfieldTilesViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldTilesView());
		bean.setBaseExtensions("jsp");
		bean.setExtensions("tiles");
		bean.setResourceRequired(true);
		return bean;
	}
	
	@Bean
	public ResourceBasedTemplateResolver springfieldThymeleafTemplateEngineResolver(){
		ResourceBasedTemplateResolver bean = new ResourceBasedTemplateResolver();
		bean.setPrefix("");
		bean.setSuffix("");
		bean.setCharacterEncoding("UTF-8");
		bean.setTemplateMode("HTML5");
		bean.setCacheable(false);
		return bean;
	}
	
	
	@Bean
	public SpringTemplateEngine springfieldThymeleafTemplateEngine() throws Exception{
		SpringTemplateEngine bean = new SpringTemplateEngine();
		bean.setTemplateResolver(springfieldThymeleafTemplateEngineResolver());
		bean.addDialect(new SpringSecurityDialect());
		return bean;
	}
	
	@Bean
	public ThymeleafViewResolver springfieldThymeleafView() throws Exception{
		ThymeleafViewResolver bean = new ThymeleafViewResolver();
		bean.setTemplateEngine(springfieldThymeleafTemplateEngine());
		bean.setCharacterEncoding("UTF-8");
		bean.setViewNames(new String[]{"*.html"});
		bean.setContentType("text/html");
		bean.setCache(false);
		return bean;
	}

	@Bean
	public ViewResolverSupport springfieldThymeleafViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldThymeleafView());
		bean.setBaseExtensions("html");
		bean.setExtensions("none","html","thymeleaf");
		bean.setResourceRequired(true);
		return bean;
	}
	
	@Bean
	public EntityViewResolver springfieldViewResolver() throws Exception{

		String[] locations = new String[3];
		locations[0] = "/WEB-INF/"+ClassUtils.convertClassNameToResourcePath(configurer.getProperty(Configurer.BASE_PACKAGE));
		locations[1] = "classpath:"+ClassUtils.convertClassNameToResourcePath(configurer.getProperty(Configurer.BASE_PACKAGE));
		locations[2] = "classpath:com/u2ware/springfield/view/thymeleaf";
		
		EntityViewResolver bean = new EntityViewResolver();
		bean.setProperties(configurer);
		bean.setLocations(locations);
		bean.addViewResolver(springfieldDownloadViewResolver());
		bean.addViewResolver(springfieldStreamViewResolver());
		bean.addViewResolver(springfieldJsonViewResolver());
		bean.addViewResolver(springfieldXmlViewResolver());
		bean.addViewResolver(springfieldXlsViewResolver());
		bean.addViewResolver(springfieldCsvViewResolver());
		bean.addViewResolver(springfieldJstlViewResolver());
		bean.addViewResolver(springfieldTilesViewResolver());
		bean.addViewResolver(springfieldThymeleafViewResolver());
			
		return bean;
	}
	
	
	
	
	
	
	
	
}
