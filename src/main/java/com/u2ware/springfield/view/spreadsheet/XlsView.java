package com.u2ware.springfield.view.spreadsheet;

import java.io.OutputStream;
import java.lang.reflect.Field;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import jxl.Workbook;
import jxl.WorkbookSettings;
import jxl.write.Label;
import jxl.write.WritableCell;
import jxl.write.WritableSheet;
import jxl.write.WritableWorkbook;
import jxl.write.WriteException;
import jxl.write.biff.RowsExceededException;

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
import org.springframework.util.ReflectionUtils;
import org.springframework.util.ReflectionUtils.FieldCallback;
import org.springframework.util.ReflectionUtils.FieldFilter;
import org.springframework.util.StringUtils;
import org.springframework.web.servlet.view.AbstractView;

import com.u2ware.springfield.view.ModelFilter;
import com.u2ware.springfield.view.multipart.DownloadBean;
import com.u2ware.springfield.view.multipart.MultipartFileBean;

public class XlsView extends AbstractView implements ResourceLoaderAware{

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	private static final String CONTENT_TYPE = "application/vnd.ms-excel";
	
	protected ResourceLoader resourceLoader;
	protected ModelFilter modelFilter;
	
	public XlsView() {
		setContentType(CONTENT_TYPE);
	}
	public void setResourceLoader(ResourceLoader resourceLoader) {
		this.resourceLoader = resourceLoader;
	}
	public void setModelFilter(ModelFilter modelFilter) {
		this.modelFilter = modelFilter;
	}
	@Override
	protected boolean generatesDownloadContent() {
		return true;
	}

	protected Object filterModel(Map<String, Object> model) {
		if(modelFilter != null){
			return modelFilter.extractOutputModel(model);
		}
		return model;
	}
	
	
	@Override
	protected void renderMergedOutputModel(Map<String, Object> model, HttpServletRequest request, HttpServletResponse response) throws Exception {
		
		response.setContentType(getContentType());
		
		Object rootObject = filterModel(model);
		if(modelFilter != null){
			rootObject = modelFilter.extractOutputModel(model);
		}
		
		String url = (String)model.get("springfield.response.viewName");
		String name = guessFileName(rootObject);
		if(name == null){
			name = url != null ? StringUtils.getFilename(url) : "XlsView";
		}
		
		//String encodedFilename = new String(this.name.getBytes("euc-kr") , "8859_1");
		//String encodedFilename = new String("한글.xls".getBytes("euc-kr") , "8859_1"); 
		String encodedFilename = new String(name.getBytes("euc-kr") , "8859_1"); 
		response.setHeader("Content-Disposition", "attachment; filename=\""+encodedFilename+".xls\"");
		logger.warn("Dowloading Excel workbook name is " + encodedFilename);

		
		OutputStream out = response.getOutputStream();

		WorkbookSettings setting = new WorkbookSettings();
		setting.setWriteAccess(null);		
		WritableWorkbook workbook = null;	
		if (url != null) {
			Resource inputFile = resourceLoader.getResource(url);
			if(inputFile.exists()){
				Workbook template = Workbook.getWorkbook(inputFile.getInputStream());
				workbook = Workbook.createWorkbook(out, template, setting) ;
				logger.warn("Loading Excel workbook from " + inputFile);
				buildTemplatedExcelDocument(workbook, rootObject);
			}else{
				workbook = Workbook.createWorkbook(out, setting);
				logger.warn("Creating Excel Workbook from scratch");
				buildEmptyExcelDocument(workbook, rootObject);
			}
		}else {
			workbook = Workbook.createWorkbook(out, setting);
			logger.warn("Creating Excel Workbook from scratch");
			buildEmptyExcelDocument(workbook, rootObject);
		}
		
		workbook.write();
		out.flush();
		workbook.close();
	}


	protected String guessFileName(Object rootObject) {
		if(ClassUtils.isAssignableValue(Iterable.class, rootObject)){
			Iterable<?> i = (Iterable<?>)rootObject;
			try{
				Object r = i.iterator().next();
				if(ClassUtils.isAssignableValue(DownloadBean.class, r)){
					DownloadBean multipartFileBean = (DownloadBean)r;
					return multipartFileBean.getContentName();
				}
			}catch(Exception e){
				
			}
		}else if(ClassUtils.isAssignableValue(MultipartFileBean.class, rootObject)){
			MultipartFileBean multipartFileBean = (MultipartFileBean)rootObject;
			return multipartFileBean.getContentName();
		}
		return null;
	}
	

	private ExpressionParser parser = new SpelExpressionParser();
	private TemplateParserContext parserContext = new TemplateParserContext();

	private void buildTemplatedExcelDocument(WritableWorkbook workbook, Object rootObject) throws Exception{
		

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
								logger.debug("["+r+","+c+"]"+contents+"->"+newContents);
							}else{
								logger.debug("["+r+","+c+"]"+contents);
							}
						}catch(Exception e){
							logger.debug("["+r+","+c+"]"+contents+"->"+e);
							Label label = new Label(c, r, "" ,cell.getCellFormat()); 
							sheet.addCell(label); 
						}
					}else{
						logger.debug("["+r+","+c+"]"+cell.getType());
					}
				}
			}
		}
	}
	
	private void buildEmptyExcelDocument(WritableWorkbook workbook, Object rootObject) throws Exception{
		
		String name = ClassUtils.getShortNameAsProperty(rootObject.getClass());		
		WritableSheet sheet = workbook.createSheet(name, 0);
		
		if(ClassUtils.isAssignableValue(Iterable.class, rootObject)){
			Iterable<?> iterable = (Iterable<?>)rootObject;

			int row = 0;
			for(Object object : iterable){
				BeanWrapper beanWrapper = new BeanWrapperImpl(object);
				if(row == 0){
					buildEmptyExcelDocumentRow(sheet, row, beanWrapper);
					row++;
				}
				buildEmptyExcelDocumentRow(sheet, row, beanWrapper);
				row++;
			}
		}else{

			BeanWrapper beanWrapper = new BeanWrapperImpl(rootObject);
			buildEmptyExcelDocumentRow(sheet, 0, beanWrapper);
			buildEmptyExcelDocumentRow(sheet, 1, beanWrapper);
		}
	}
	
	private void buildEmptyExcelDocumentRow(final WritableSheet sheet, int row, BeanWrapper b) throws RowsExceededException, WriteException{
		doWithSpringfieldPropertyCallback(row, b, new SpringfieldPropertyCallback(){
			public void render(int column, int row, String content) throws Exception{
				sheet.addCell(new Label(column, row, content)); 
			}});
	}


	
	
	///////////////////////////////////////////////////////////////
	//
	///////////////////////////////////////////////////////////////
	public interface SpringfieldPropertyCallback{
		public void render(int column, int row, String content) throws Exception;
	}
	
	
	protected void doWithSpringfieldPropertyCallback(final int row, final BeanWrapper b, final SpringfieldPropertyCallback callback){

		ReflectionUtils.doWithFields(b.getWrappedClass()
			, new FieldCallback() {
			
				private int column = 0;
			
				public void doWith(Field field) throws IllegalArgumentException, IllegalAccessException {
	
					String property = field.getName();
					
					String content = null;
					if(row == 0){
						
						XlsProperty propertyAnnotation = field.getAnnotation(XlsProperty.class);
						if(propertyAnnotation != null){
							content = propertyAnnotation.value();
						}
					
						if(! StringUtils.hasText(content)){
							content = field.getName();
						}
					
					
					}else{
						Object value = b.getPropertyValue(property);
						content = value == null ? "" : value.toString();
					}

					
					
					
					try{
						callback.render(column, row, content);
					}catch(Exception e){
					}
					
					column++;
				}

			}, new FieldFilter() {
				public boolean matches(Field field) {
					XlsIgnore ignoreAnnotation = field.getAnnotation(XlsIgnore.class);
					return  ignoreAnnotation == null; 
				}
			}
		);
	}
	
		
	
}
