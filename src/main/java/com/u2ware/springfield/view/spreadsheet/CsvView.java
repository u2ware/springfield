package com.u2ware.springfield.view.spreadsheet;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.BeanWrapper;
import org.springframework.beans.BeanWrapperImpl;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;

import au.com.bytecode.opencsv.CSVWriter;



public class CsvView extends XlsView {

	private static final String CONTENT_TYPE = "text/csv";
	
	public CsvView() {
		setContentType(CONTENT_TYPE);
	}


	@Override
	protected void renderMergedOutputModel(Map<String, Object> model, HttpServletRequest request, HttpServletResponse response) throws Exception {
		
		response.setContentType(CONTENT_TYPE);
		response.setCharacterEncoding("euc-kr");


		Object rootObject = filterModel(model);
		if(modelFilter != null){
			rootObject = modelFilter.extractOutputModel(model);
		}
		
		String url = (String)model.get("springfield.response.viewName");
		String name = guessFileName(rootObject);
		if(name == null){
			name = url != null ? StringUtils.getFilename(url) : "CsvView";
		}
		
		
		String encodedFilename = new String(name.getBytes("euc-kr") , "8859_1"); 
		response.setHeader("Content-Disposition", "attachment; filename=\""+encodedFilename+".csv\"");
		logger.warn("Dowloading CSV name is " + encodedFilename);

		
		PrintWriter out = response.getWriter();
		CSVWriter writer = new CSVWriter(out);
		buildEmptyCSVDocument(writer, rootObject);
		writer.flush();
		writer.close();

	}
	
	private void buildEmptyCSVDocument(CSVWriter writer, Object rootObject) throws Exception{
		
		if(ClassUtils.isAssignableValue(Iterable.class, rootObject)){
			Iterable<?> iterable = (Iterable<?>)rootObject;

			int row = 0;
			for(Object object : iterable){
				BeanWrapper beanWrapper = new BeanWrapperImpl(object);
				if(row == 0){
					buildEmptyCSVDocumentRow(writer, row, beanWrapper);
					row++;
				}
				buildEmptyCSVDocumentRow(writer, row, beanWrapper);
				row++;
			}
		}else{
			BeanWrapper beanWrapper = new BeanWrapperImpl(rootObject);
			buildEmptyCSVDocumentRow(writer, 0, beanWrapper);
			buildEmptyCSVDocumentRow(writer, 1, beanWrapper);
		}
	}



	private void buildEmptyCSVDocumentRow(CSVWriter writer, int row, BeanWrapper b) throws Exception{
		final List<String> contents = new ArrayList<String>();

		doWithSpringfieldPropertyCallback(row, b, new SpringfieldPropertyCallback(){
			public void render(int column, int row, String content) throws Exception{
				contents.add(content);
			}});
		
		String[] entries = new String[contents.size()];
		contents.toArray(entries);
		
		writer.writeNext(entries);
	}
	
	
}
