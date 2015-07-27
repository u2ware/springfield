package com.u2ware.springfield.config;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.validation.MessageInterpolator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.modelmapper.ModelMapper;
import org.springframework.context.MessageSource;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.ReloadableResourceBundleMessageSource;
import org.springframework.http.MediaType;
import org.springframework.orm.jpa.SharedEntityManagerCreator;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.TransactionManagementConfigurer;
import org.springframework.web.accept.ContentNegotiationManager;
import org.springframework.web.servlet.ViewResolver;
import org.springframework.web.servlet.config.annotation.ContentNegotiationConfigurer;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;
import org.springframework.web.servlet.view.ContentNegotiatingViewResolver;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.u2ware.springfield.controller.GenericHandlerMapping;
import com.u2ware.springfield.service.ValidationMessageInterpolator;
import com.u2ware.springfield.view.GenericView;
import com.u2ware.springfield.view.GenericViewResolver;
import com.u2ware.springfield.view.download.DownloadView;
import com.u2ware.springfield.view.html.HtmlView;
import com.u2ware.springfield.view.json.JsonView;
import com.u2ware.springfield.view.upload.UploadView;
import com.u2ware.springfield.view.xls.XlsView;
import com.u2ware.springfield.view.xml.XmlView;

@Configuration
abstract class GenericMvcConfiguration extends WebMvcConfigurerAdapter implements TransactionManagementConfigurer{
	
    protected Log logger = LogFactory.getLog(getClass());

	private Iterable<String> basePackages;
	private EntityManagerFactory entityManagerFactory;
	private PlatformTransactionManager transactionManager;
	
	public GenericMvcConfiguration(Iterable<String> basePackages, EntityManagerFactory emf, PlatformTransactionManager tx){
		this.basePackages = basePackages;
		this.entityManagerFactory = emf;
		this.transactionManager = tx;
	}

	@Override
	public PlatformTransactionManager annotationDrivenTransactionManager() {
		return transactionManager;
	}

	@Bean(name="com_u2ware_springfield_config_RequestMappingPatternResolverBean")
	public RequestMappingPatternResolverBean requestMappingPatternResolverBean(){
		EntityManager em = SharedEntityManagerCreator.createSharedEntityManager(entityManagerFactory);
		return new RequestMappingPatternResolverBean(em);
	}
	
	
	@Bean(name="com_u2ware_springfield_config_ObjectMapper")
	public ObjectMapper objectMapper(){
		return new ObjectMapper();
	}
	
	@Bean(name="com_u2ware_springfield_config_ModelMapper")
	public ModelMapper modelMapper(){
		return new ModelMapper();
	}
	
	@Bean(name="com_u2ware_springfield_config_MessageInterpolator")
	public MessageInterpolator messageInterpolator(){
		return new ValidationMessageInterpolator();
	}
	
	@Bean
	public ResourcePatternResolverBean messageSourceBasenames(){
		return new ResourcePatternResolverBean(basePackages, "/**/messages.xml");
	}
	
	@Bean
	public MessageSource messageSource(){
		ReloadableResourceBundleMessageSource r =  new ReloadableResourceBundleMessageSource();
		r.setBasenames(messageSourceBasenames().getBasenames());
		return r;
	}	
	
	@Bean(name="com_u2ware_springfield_config_HandlerMapping")
	public GenericHandlerMapping handlerMapping(){
		return new GenericHandlerMapping();
	}
	
    @Bean(name="com_u2ware_springfield_view_GenericView")
    public GenericView genericView() {
        return new GenericView(objectMapper());
    }
	
    @Bean(name="com_u2ware_springfield_view_HtmlView")
    public HtmlView htmlView() {
        return new HtmlView(genericView());
    }

    @Bean(name="com_u2ware_springfield_view_JsonView")
    public JsonView jsonView() {
        return new JsonView(genericView());
    }
    
    @Bean(name="com_u2ware_springfield_view_XlsView")
    public XlsView xlsView() {
        return new XlsView(genericView());
    }

    @Bean(name="com_u2ware_springfield_view_XmlView")
    public XmlView xmlView() {
        return new XmlView(genericView());
    }

    @Bean(name="com_u2ware_springfield_view_UploadView")
    public UploadView uploadView() {
        return new UploadView(genericView());
    }

    @Bean(name="com_u2ware_springfield_view_DownloadView")
    public DownloadView downloadView() {
        return new DownloadView(genericView());
    }

    @Bean(name="com_u2ware_springfield_view_ContentNegotiatingViewResolver")
    public ViewResolver contentNegotiatingViewResolver(ContentNegotiationManager manager) {
        List<ViewResolver> viewResolvers = new ArrayList<ViewResolver>();
        viewResolvers.add(new GenericViewResolver(htmlView()));
        viewResolvers.add(new GenericViewResolver(jsonView()));
        viewResolvers.add(new GenericViewResolver(xlsView()));
        viewResolvers.add(new GenericViewResolver(xmlView()));
        viewResolvers.add(new GenericViewResolver(uploadView()));
        viewResolvers.add(new GenericViewResolver(downloadView()));

        ContentNegotiatingViewResolver resolver = new ContentNegotiatingViewResolver();
        resolver.setContentNegotiationManager(manager);
        resolver.setViewResolvers(viewResolvers);

        return resolver;
	}
	
    @Override
    public void configureContentNegotiation(ContentNegotiationConfigurer configurer) {
        configurer.ignoreAcceptHeader(false);

        configurer.mediaType("html", MediaType.TEXT_HTML);
        configurer.mediaType("json", MediaType.APPLICATION_JSON);
        configurer.mediaType("xls", MediaType.valueOf("application/vnd.ms-excel"));
        configurer.mediaType("xml", MediaType.APPLICATION_XML);
        configurer.mediaType("upload", MediaType.MULTIPART_FORM_DATA);
        configurer.mediaType("download", MediaType.APPLICATION_OCTET_STREAM);
    }
    
//	@Bean(name="com_u2ware_springfield_config_MultipartResolver")
//	public MultipartResolver multipartResolver(){
//    	return new CommonsMultipartResolver();
//	}	


	
}
