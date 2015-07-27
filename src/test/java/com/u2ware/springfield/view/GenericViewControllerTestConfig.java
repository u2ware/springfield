package com.u2ware.springfield.view;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.data.web.config.EnableSpringDataWebSupport;
import org.springframework.http.MediaType;
import org.springframework.web.accept.ContentNegotiationManager;
import org.springframework.web.servlet.ViewResolver;
import org.springframework.web.servlet.config.annotation.ContentNegotiationConfigurer;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;
import org.springframework.web.servlet.view.ContentNegotiatingViewResolver;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.u2ware.springfield.controller.GenericControllerTestConfig;
import com.u2ware.springfield.view.download.DownloadView;
import com.u2ware.springfield.view.html.HtmlView;
import com.u2ware.springfield.view.json.JsonView;
import com.u2ware.springfield.view.upload.UploadView;
import com.u2ware.springfield.view.xls.XlsView;
import com.u2ware.springfield.view.xml.XmlView;


@Configuration
@Import(GenericControllerTestConfig.class)
@EnableWebMvc
@EnableSpringDataWebSupport
public class GenericViewControllerTestConfig extends WebMvcConfigurerAdapter{

    protected Log logger = LogFactory.getLog(getClass());
	
    @Bean
    public ObjectMapper objectMapper() {
        return new ObjectMapper();
    }
    
    @Bean
    public GenericView genericView() {
        return new GenericView(objectMapper());
    }
	
    @Bean
    public HtmlView htmlView() {
        return new HtmlView(genericView());
    }
    @Bean
    public JsonView jsonView() {
        return new JsonView(genericView());
    }
    @Bean
    public XlsView xlsView() {
        return new XlsView(genericView());
    }
    @Bean
    public XmlView xmlView() {
        return new XmlView(genericView());
    }
    @Bean
    public UploadView uploadView() {
        return new UploadView(genericView());
    }
    @Bean
    public DownloadView downloadView() {
        return new DownloadView(genericView());
    }

	@Bean
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
        configurer.mediaType("html", MediaType.TEXT_HTML);
        configurer.mediaType("json", MediaType.APPLICATION_JSON);
        configurer.mediaType("xls", MediaType.valueOf("application/vnd.ms-excel"));
        configurer.mediaType("xml", MediaType.APPLICATION_XML);
        configurer.mediaType("upload", MediaType.MULTIPART_FORM_DATA);
        configurer.mediaType("download", MediaType.APPLICATION_OCTET_STREAM);
        
        configurer.ignoreAcceptHeader(false);
    }
}
