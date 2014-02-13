package com.u2ware.springfield.config.support;

import java.util.Properties;

import org.springframework.context.EnvironmentAware;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.core.env.Environment;
import org.springframework.oxm.xstream.XStreamMarshaller;
import org.springframework.util.ClassUtils;
import org.springframework.web.servlet.view.InternalResourceViewResolver;
import org.springframework.web.servlet.view.UrlBasedViewResolver;
import org.springframework.web.servlet.view.tiles2.TilesView;
import org.thymeleaf.extras.springsecurity3.dialect.SpringSecurityDialect;
import org.thymeleaf.spring3.SpringTemplateEngine;
import org.thymeleaf.spring3.view.ThymeleafViewResolver;

import com.u2ware.springfield.support.resource.ResourcePatternResolverBean;
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
public class WebmvcRenderConfiguration implements EnvironmentAware{

	private Environment env;
	public String basePackage;

	public void setBasePackage(String basePackage) {
		this.basePackage = basePackage;
	}

	@Override
	public void setEnvironment(Environment env) {
		this.env = env;
	}

	@Bean
	public Properties springfieldWebmvcRenderProperties(){
		Properties m = new Properties();
		
		m.put("springfield.view.method.home", env.getProperty("springfield.view.method.home", "home"));
		m.put("springfield.view.method.find", env.getProperty("springfield.view.method.find", "list"));
		m.put("springfield.view.method.createForm", env.getProperty("springfield.view.method.createForm", "edit"));
		m.put("springfield.view.method.create", env.getProperty("springfield.view.method.create", "refresh"));
		m.put("springfield.view.method.read", env.getProperty("springfield.view.method.read", "edit"));
		m.put("springfield.view.method.updateForm", env.getProperty("springfield.view.method.updateForm", "edit"));
		m.put("springfield.view.method.update", env.getProperty("springfield.view.method.update", "refresh"));
		m.put("springfield.view.method.delete", env.getProperty("springfield.view.method.delete", "refresh"));
		
		return m;
	}
	
	
	/////////////////////////////////////////////
	//
	/////////////////////////////////////////////
	@Bean
	public ModelFilter springfieldWebmvcRenderModelFilter() throws Exception{
		return new ModelFilterImpl();
	}
	
	@Bean
	public MultipartFileBeanView springfieldWebmvcRenderDownloadView()  throws Exception{
		MultipartFileBeanView bean = new MultipartFileBeanView();
		bean.setModelFilter(springfieldWebmvcRenderModelFilter());
		bean.setDownload(true);
		return bean;
	}
	
	@Bean
	public ViewResolverSupport springfieldWebmvcRenderDownloadViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldWebmvcRenderDownloadView());
		bean.setBaseExtensions("download");
		bean.setExtensions("download");
		return bean;
	}
	
	@Bean
	public MultipartFileBeanView springfieldWebmvcRenderStreamView()  throws Exception{
		MultipartFileBeanView bean = new MultipartFileBeanView();
		bean.setModelFilter(springfieldWebmvcRenderModelFilter());
		bean.setDownload(false);
		return bean;
	}
	
	@Bean
	public ViewResolverSupport springfieldWebmvcRenderStreamViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldWebmvcRenderStreamView());
		bean.setBaseExtensions("stream");
		bean.setExtensions("stream");
		return bean;
	}
	
	
	@Bean
	public JsonView springfieldWebmvcRenderJsonView()  throws Exception{
		JsonView bean = new JsonView();
		bean.setModelFilter(springfieldWebmvcRenderModelFilter());
		bean.setContentType("application/json;charset=UTF-8");
		return bean;
	}
	
	@Bean
	public ViewResolverSupport springfieldWebmvcRenderJsonViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldWebmvcRenderJsonView());
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
	public XmlView springfieldWebmvcRenderXmlView() throws Exception{
		XmlView bean = new XmlView();
		bean.setModelFilter(springfieldWebmvcRenderModelFilter());
		bean.setContentType("application/xml;charset=UTF-8");
		bean.setMarshaller(springfieldXStreamMarshaller());
		return bean;
	}
	
	@Bean
	public ViewResolverSupport springfieldWebmvcRenderXmlViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldWebmvcRenderXmlView());
		bean.setBaseExtensions("xml");
		bean.setExtensions("xml");
		return bean;
	}
	
	@Bean
	public XlsView springfieldWebmvcRenderXlsView() throws Exception{
		XlsView bean = new XlsView();
		bean.setModelFilter(springfieldWebmvcRenderModelFilter());
		bean.setContentType("application/vnd.ms-excel");
		return bean;
	}
	
	@Bean
	public ViewResolverSupport springfieldWebmvcRenderXlsViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldWebmvcRenderXlsView());
		bean.setBaseExtensions("xls");
		bean.setExtensions("xls");
		bean.setResourceRequired(true);
		return bean;
	}
	
	@Bean
	public CsvView springfieldWebmvcRenderCsvView() throws Exception{
		CsvView bean = new CsvView();
		bean.setModelFilter(springfieldWebmvcRenderModelFilter());
		bean.setContentType("text/csv");
		return bean;
	}
	
	@Bean
	public ViewResolverSupport springfieldWebmvcRenderCsvViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldWebmvcRenderCsvView());
		bean.setBaseExtensions("csv");
		bean.setExtensions("csv");
		bean.setResourceRequired(false);
		return bean;
	}
	
	
	
	@Bean
	public InternalResourceViewResolver springfieldWebmvcRenderJstlView() throws Exception{
		InternalResourceViewResolver bean = new InternalResourceViewResolver();
		bean.setOrder(Ordered.LOWEST_PRECEDENCE);
		return bean;
	}

	@Bean
	public ViewResolverSupport springfieldWebmvcRenderJstlViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldWebmvcRenderJstlView());
		bean.setBaseExtensions("jsp");
		bean.setExtensions("jstl");
		bean.setResourceRequired(true);
		return bean;
	}

	@Bean
	public ResourcePatternResolverBean springfieldTilesConfigurerDefinitions()throws Exception{
		ResourcePatternResolverBean bean = new ResourcePatternResolverBean();
		bean.setPackagesToScan(basePackage);
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
	public UrlBasedViewResolver springfieldWebmvcRenderTilesView()throws Exception{
		UrlBasedViewResolver bean = new UrlBasedViewResolver();
		bean.setViewClass(TilesView.class);
		bean.setOrder(Ordered.LOWEST_PRECEDENCE);
		return bean;
	}
	
	@Bean
	public ViewResolverSupport springfieldWebmvcRenderTilesViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldWebmvcRenderTilesView());
		bean.setBaseExtensions("jsp");
		bean.setExtensions("tiles");
		bean.setResourceRequired(false);
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
	public ThymeleafViewResolver springfieldWebmvcRenderThymeleafView() throws Exception{
		ThymeleafViewResolver bean = new ThymeleafViewResolver();
		bean.setTemplateEngine(springfieldThymeleafTemplateEngine());
		bean.setCharacterEncoding("UTF-8");
		bean.setViewNames(new String[]{"*.html"});
		bean.setContentType("text/html");
		bean.setCache(false);
		return bean;
	}

	@Bean
	public ViewResolverSupport springfieldWebmvcRenderThymeleafViewResolver()  throws Exception{
		ViewResolverSupport bean = new ViewResolverSupport(springfieldWebmvcRenderThymeleafView());
		bean.setBaseExtensions("html");
		bean.setExtensions("none","html","thymeleaf");
		bean.setResourceRequired(true);
		return bean;
	}
	
	@Bean
	public EntityViewResolver springfieldViewResolver() throws Exception{

		String[] locations = new String[3];
		locations[0] = "/WEB-INF/"+ClassUtils.convertClassNameToResourcePath(basePackage);
		locations[1] = "classpath:"+ClassUtils.convertClassNameToResourcePath(basePackage);
		locations[2] = "classpath:com/u2ware/springfield/view/thymeleaf";
		
		EntityViewResolver bean = new EntityViewResolver();
		bean.setProperties(springfieldWebmvcRenderProperties());
		bean.setLocations(locations);
		bean.addViewResolver(springfieldWebmvcRenderDownloadViewResolver());
		bean.addViewResolver(springfieldWebmvcRenderStreamViewResolver());
		bean.addViewResolver(springfieldWebmvcRenderJsonViewResolver());
		bean.addViewResolver(springfieldWebmvcRenderXmlViewResolver());
		bean.addViewResolver(springfieldWebmvcRenderXlsViewResolver());
		bean.addViewResolver(springfieldWebmvcRenderCsvViewResolver());
		bean.addViewResolver(springfieldWebmvcRenderJstlViewResolver());
		bean.addViewResolver(springfieldWebmvcRenderTilesViewResolver());
		bean.addViewResolver(springfieldWebmvcRenderThymeleafViewResolver());
			
		return bean;
	}
	
	
}
