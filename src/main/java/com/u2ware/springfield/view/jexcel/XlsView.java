package com.u2ware.springfield.view.jexcel;

import java.beans.PropertyDescriptor;
import java.io.OutputStream;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import jxl.Workbook;
import jxl.WorkbookSettings;
import jxl.write.Label;
import jxl.write.WritableCell;
import jxl.write.WritableSheet;
import jxl.write.WritableWorkbook;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanWrapper;
import org.springframework.beans.BeanWrapperImpl;
import org.springframework.context.ResourceLoaderAware;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.expression.Expression;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.common.TemplateParserContext;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.servlet.view.AbstractView;

import com.u2ware.springfield.view.ViewResolverSupport;
import com.u2ware.springfield.view.multipart.MultipartFileBean;

public class XlsView extends AbstractView implements ResourceLoaderAware{

	private static final Logger logger = LoggerFactory.getLogger(XlsView.class);

	private static final String CONTENT_TYPE = "application/vnd.ms-excel";
	
	private ResourceLoader resourceLoader;
	private String url;
	private String name;
	
	public XlsView() {
		setContentType(CONTENT_TYPE);
	}
	public void setUrl(String url) {
		this.url = url;
	}
	public void setName(String name) {
		this.name = name;
	}
	public void setResourceLoader(ResourceLoader resourceLoader) {
		this.resourceLoader = resourceLoader;
	}
	
	@Override
	protected boolean generatesDownloadContent() {
		return true;
	}

	private String resolveUrl(Map<String, Object> model) throws Exception{
		return (String)model.get("webmvc.view.uri");
	}
	private String resolveName(Map<String, Object> model) throws Exception{
		try{
			MultipartFileBean d = (MultipartFileBean)ViewResolverSupport.getRequestModel(model);
			return d.getContentName();

		}catch(Exception e){
			String url =  (String)model.get("webmvc.view.uri");
			return StringUtils.getFilename(url); 
		}
	}
	
	@Override
	protected void renderMergedOutputModel(Map<String, Object> model,HttpServletRequest request, HttpServletResponse response) throws Exception {
		
		response.setContentType(getContentType());
		OutputStream out = response.getOutputStream();

		
		Object rootObject = ViewResolverSupport.getRequestModel(model);
		
		if(this.url == null){
			this.url = resolveUrl(model);
		}
		if(this.name == null){
			this.name = resolveName(model);
		}

		WorkbookSettings setting = new WorkbookSettings();
		setting.setWriteAccess(null);		

		WritableWorkbook workbook = null;	
		if (this.url != null) {
			Resource inputFile = resourceLoader.getResource(url);
			logger.debug("Searching Excel workbook from " + inputFile);
			if(inputFile.exists()){
				Workbook template = Workbook.getWorkbook(inputFile.getInputStream());
				workbook = Workbook.createWorkbook(out, template, setting) ;
				logger.debug("Loading Excel workbook from " + inputFile);
				buildTemplatedExcelDocument(rootObject, workbook, request, response);
			}else{
				workbook = Workbook.createWorkbook(out, setting);
				logger.debug("Creating Excel Workbook from scratch");
				buildExcelDocument(rootObject, workbook, request, response);
			}
		}else {
			workbook = Workbook.createWorkbook(out, setting);
			logger.debug("Creating Excel Workbook from scratch");
			buildExcelDocument(rootObject, workbook, request, response);
		}

		if(this.name != null){
			//String encodedFilename = new String(this.name.getBytes("euc-kr") , "8859_1");
			//String encodedFilename = new String("한글.xls".getBytes("euc-kr") , "8859_1"); 
			String encodedFilename = new String(this.name.getBytes("euc-kr") , "8859_1"); 
			response.setHeader("Content-Disposition", "attachment; filename=\""+encodedFilename+"\"");
			logger.info("Dowloading Excel workbook name is " + encodedFilename);
		}

		workbook.write();
		out.flush();
		workbook.close();
		
	}

	private ExpressionParser parser = new SpelExpressionParser();
	private TemplateParserContext parserContext = new TemplateParserContext();

	protected void buildTemplatedExcelDocument(Object rootObject, WritableWorkbook workbook, HttpServletRequest request, HttpServletResponse response) throws Exception{
		

		WritableSheet sheet = null;
		WritableCell cell = null;
		for(int s = 0 ; s < workbook.getNumberOfSheets(); s++){
			sheet = workbook.getSheet(s);
			for(int r = 0 ; r < sheet.getRows(); r++){
				for(int c = 0 ; c < sheet.getColumns(); c++){
					cell = sheet.getWritableCell(c, r);
					
					String contents = cell.getContents();
					if(StringUtils.hasText(contents)){
						try{
							Expression exp = parser.parseExpression(contents, parserContext);
							Object value = exp.getValue(rootObject);
							String newContents = (value!= null ? value.toString() : "");
							Label label = new Label(c, r, newContents ,cell.getCellFormat()); 
							sheet.addCell(label); 
							
							if(! contents.equals(newContents)){
								logger.trace("["+r+","+c+"]"+contents+"->"+newContents);
							}
						}catch(Exception e){
							logger.trace("["+r+","+c+"]"+contents+"->"+e);
						}
					}else{
						logger.trace("["+r+","+c+"]"+cell.getType());
					}
				}
			}
		}
	}
	
	protected void buildExcelDocument(Object rootObject, WritableWorkbook workbook,HttpServletRequest request, HttpServletResponse response) throws Exception{
		
		String name = ClassUtils.getShortNameAsProperty(rootObject.getClass());
		
		WritableSheet sheet = workbook.createSheet(name, 0);

		
		if(ClassUtils.isAssignableValue(Iterable.class, rootObject)){
			Iterable<?> iterable = (Iterable<?>)rootObject;

			int row = 0;
			for(Object object : iterable){

				BeanWrapper w = new BeanWrapperImpl(object);
				PropertyDescriptor[] pds = w.getPropertyDescriptors();

				int column = 0;
				if(row == 0){
					for(PropertyDescriptor pd : pds){
						String title = pd.getName();
						sheet.addCell(new Label(column, row, title)); 
						column++;
					}
				}else{
					for(PropertyDescriptor pd : pds){
						String content = w.getPropertyValue(pd.getName()).toString();
						sheet.addCell(new Label(column, row, content)); 
						column++;
					}
				}
				row++;
			}
		}else{
			int row = 0;
			BeanWrapper w = new BeanWrapperImpl(rootObject);
			PropertyDescriptor[] pds = w.getPropertyDescriptors();
			for(PropertyDescriptor pd : pds){

				String title = pd.getName();
				sheet.addCell(new Label(0, row, title)); 
				
				Object obj = w.getPropertyValue(title);
				String content = obj == null ? "" : obj.toString();
				sheet.addCell(new Label(1, row, content)); 
				
				row++;
			}
		}
	}
}
