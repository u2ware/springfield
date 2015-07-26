package com.u2ware.springfield.view.xls;

import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import net.sf.jxls.transformer.XLSTransformer;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.LocalizedResourceHelper;
import org.springframework.web.servlet.support.RequestContextUtils;
import org.springframework.web.servlet.view.document.AbstractExcelView;

import com.u2ware.springfield.view.GenericView;

public class XlsView extends AbstractExcelView{

    protected Log logger = LogFactory.getLog(getClass());
	
    private GenericView genericView;

    public XlsView(GenericView genericView){
    	this.genericView = genericView;
    }

    private XLSTransformer transformer = new XLSTransformer();
	
	public void render(Map<String, ?> model, HttpServletRequest request, HttpServletResponse response) throws Exception {
		try{
			logger.debug(getClass());
			super.render(model, request, response);
		}catch(Exception e){
			genericView.render(e, request, response);
		}
	}
	
	@Override
	protected void buildExcelDocument(Map<String, Object> model, HSSFWorkbook workbook, HttpServletRequest request, HttpServletResponse response) throws Exception{

		/*
		String encodedFilename = new String(name.getBytes("euc-kr") , "8859_1"); 
		response.setHeader("Content-Disposition", "attachment; filename=\""+encodedFilename+".xls\"");
		logger.warn("Dowloading Excel workbook name is " + encodedFilename);
		response.setContentType("application/x-msexcel");
		*/		
		transformer.transformWorkbook(workbook, model);
	}
	
	protected HSSFWorkbook getTemplateSource(String url, HttpServletRequest request) throws Exception {
	
		logger.debug("getTemplateSource 1: "+url);

		LocalizedResourceHelper helper = new LocalizedResourceHelper(getApplicationContext());
		Locale userLocale = RequestContextUtils.getLocale(request);

		logger.debug("getTemplateSource 2: "+url);
		
		Resource inputFile = helper.findLocalizedResource(url, ".xls", userLocale);
		logger.debug("getTemplateSource 3: "+inputFile);
		
		//Resource inputFile = helper.findLocalizedResource(url, ".xls", userLocale);
		//logger.debug("getTemplateSource2: "+inputFile);

		
		return super.getTemplateSource(url, request);
		
	}
		
}