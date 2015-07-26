package com.u2ware.springfield.controller;

import java.util.Map;

import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;

/**
 * 
 * application/json
 * 
 * 		Create     : POST /{topLevelMapping}              
 * 		Read        : GET /{topLevelMapping}/{id}
 * 		Update      : PUT /{topLevelMapping}/{id}
 * 		Delete   : DELETE /{topLevelMapping}/{id}
 * 		Find        : GET /{topLevelMapping}
 * 		Find        : GET /{topLevelMapping}/findBy{query}
 * 
 * application/xls....
 * 
 * 		Read        : GET /{topLevelMapping}/xls/{id}
 * 		Find        : GET /{topLevelMapping}/xls
 * 		Find        : GET /{topLevelMapping}/xls/findBy{query}
 * 
 * multipart/form-data
 * 
 * 		Upload     : POST /{topLevelMapping}/multipart
 * 		Download   : GET  /{topLevelMapping}/multipart/{id}
 * 
 */

public class GenericControllerImpl<D> extends GenericController<D> {

//	@InitBinder
//	public void initBinder(WebDataBinder binder){
//		smartValidator = beanFactory.getBean(SmartValidator.class);
//	}
//	@Override
//	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
//		this.beanFactory = beanFactory;
//	}
	
	@ModelAttribute("requestDto")
	public D formObject(){
		try {
			return domainClass.newInstance();
		} catch (Exception e) {
			return null;
		}
	}
	
	private D formObject(Map<String,?> body, D dto){
		logger.debug("request body: "+body);
		try{
			modelMapper.map(body, dto);
		}catch(Exception e){
		}
		return dto;
	}
	
	
	//////////////////////
	//
	//////////////////////
	@RequestMapping(method=RequestMethod.POST, value="/<root>", consumes=MediaType.APPLICATION_JSON_VALUE, produces=MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<?> createEntity(
			@RequestBody(required=false) Map<String,?> body, 
			Model model, 
			@ModelAttribute("requestDto") D dto, 
			BindingResult errors) {
		
		return super.create(formObject(body, dto), errors);
	}

	@RequestMapping(method=RequestMethod.GET, value="/<root>/<unique>", consumes=MediaType.APPLICATION_JSON_VALUE, produces=MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<?> readEntity(
			Model model, 
			@ModelAttribute("requestDto") D dto, 
			BindingResult errors) {
		
		return super.read(dto, errors);
	}

	@RequestMapping(method={RequestMethod.PUT, RequestMethod.PATCH}, consumes=MediaType.APPLICATION_JSON_VALUE, value="/<root>/<unique>", produces=MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<?> updateEntity(
			@RequestBody(required=false) Map<String,?> body, 
			Model model, 
			@ModelAttribute("requestDto") D dto, 
			BindingResult errors) {

		return super.update(formObject(body, dto), errors);
	}

	@RequestMapping(method=RequestMethod.DELETE, value="/<root>/<unique>", consumes=MediaType.APPLICATION_JSON_VALUE, produces=MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<?> deleteEntity(
			Model model, 
			@ModelAttribute("requestDto") D dto, 
			BindingResult errors) {
		
		return super.delete(dto, errors);
	}

	@RequestMapping(method=RequestMethod.GET, value="/<root>", consumes=MediaType.APPLICATION_JSON_VALUE, produces=MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<?> findEntity(
			@RequestBody(required=false) Map<String,?> body, 
			Model model, 
			@ModelAttribute("requestDto") D dto, 
			Pageable pageable, 
			BindingResult errors) {

		return super.find("", formObject(body, dto), pageable, errors);
	}

	@RequestMapping(method=RequestMethod.GET, value="/<root>/findBy{query}", consumes=MediaType.APPLICATION_JSON_VALUE, produces=MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<?> findEntity(
			@PathVariable("query") String query,
			@RequestBody(required=false) Map<String,?> body, 
			Model model, 
			@ModelAttribute("requestDto") D dto, 
			Pageable pageable, 
			BindingResult errors) {

		return super.find(query, formObject(body, dto), pageable, errors);
	}
	
	
	///////////////////////////
	//
	///////////////////////////
	@RequestMapping(method=RequestMethod.POST, value="/<root>.{extension}")
	public String createView(
			@PathVariable("extension")String extension,
			Model model, 
			@ModelAttribute("requestDto") D dto, 
			BindingResult errors) {

		ResponseEntity<?> res = super.create(dto, errors);
		String view = "/"+getRequestMappingRootPatternValue()+"/create."+extension;

		model.addAttribute("responseStatus" , res.getStatusCode());
		model.addAttribute("responseDto" , res.getBody());
		model.addAttribute("responseView" , view);
		return view;
	}

	@RequestMapping(method=RequestMethod.GET, value="/<root>/<unique>.{extension}")
	public String readView(
			@PathVariable("extension")String extension,
			Model model, 
			@ModelAttribute("requestDto") D dto, 
			BindingResult errors) {
		
		ResponseEntity<?> res = super.read(dto, errors);
		String view = "/"+getRequestMappingRootPatternValue()+"/read."+extension;

		model.addAttribute("responseStatus" , res.getStatusCode());
		model.addAttribute("responseDto" , res.getBody());
		model.addAttribute("responseView" , view);
		return view;
	}

	@RequestMapping(method={RequestMethod.PUT, RequestMethod.PATCH}, value="/<root>/<unique>.{extension}")
	public String updateView(
			@PathVariable("extension")String extension,
			Model model, 
			@ModelAttribute("requestDto") D dto, 
			BindingResult errors) {
		
		ResponseEntity<?> res = super.update(dto, errors);
		String view = "/"+getRequestMappingRootPatternValue()+"/update."+extension;

		model.addAttribute("responseStatus" , res.getStatusCode());
		model.addAttribute("responseDto" , res.getBody());
		model.addAttribute("responseView" , view);
		return view;
	}

	@RequestMapping(method=RequestMethod.DELETE, value="/<root>/<unique>.{extension}")
	public String deleteView(
			@PathVariable("extension")String extension,
			Model model, 
			@ModelAttribute("requestDto") D dto, 
			BindingResult errors) {

		ResponseEntity<?> res = super.delete(dto, errors);
		String view = "/"+getRequestMappingRootPatternValue()+"/delete."+extension;

		model.addAttribute("responseStatus" , res.getStatusCode());
		model.addAttribute("responseDto" , res.getBody());
		model.addAttribute("responseView" , view);
		return view;
	}

	@RequestMapping(method=RequestMethod.GET, value="/<root>.{extension}")
	public String findView(
			@PathVariable("extension")String extension,
			Model model, 
			@ModelAttribute("requestDto") D dto, 
			Pageable pageable, 
			BindingResult errors) {

		ResponseEntity<?> res = super.find("", dto, pageable, errors);
		String view = "/"+getRequestMappingRootPatternValue()+"/find."+extension;

		model.addAttribute("responseStatus" , res.getStatusCode());
		model.addAttribute("responseDto" , res.getBody());
		model.addAttribute("responseView" , view);
		return view;
	}

	@RequestMapping(method=RequestMethod.GET, value="/<root>/findBy{query}.{extension}")
	public String findView(
			@PathVariable("query") String query,
			@PathVariable("extension")String extension,
			Model model, 
			@ModelAttribute("requestDto") D dto, 
			Pageable pageable, 
			BindingResult errors) {

		ResponseEntity<?> res = super.find(query, dto, pageable, errors);
		String view = "/"+getRequestMappingRootPatternValue()+"/find."+extension;

		model.addAttribute("responseStatus" , res.getStatusCode());
		model.addAttribute("responseDto" , res.getBody());
		model.addAttribute("responseView" , view);
		return view;
	}
	
	///////////////////////////////
	//
	///////////////////////////////
	@RequestMapping(method=RequestMethod.POST, value={"/<root>.upload", "/<root>.merge"}, consumes=MediaType.MULTIPART_FORM_DATA_VALUE)
	public String upload(
			@RequestParam(value="multipartFile", required=false) MultipartFile multipartFile, 
			Model model) {

		HttpStatus status = HttpStatus.OK;			
		String view = "/"+getRequestMappingRootPatternValue();

		model.addAttribute("responseStatus" , status);
		model.addAttribute("responseDto" , multipartFile);
		model.addAttribute("responseView" , view);		
		return view;
	}

	@RequestMapping(method=RequestMethod.GET, value="/<root>/{unique}.download")
	public String download(
			@PathVariable("unique") String unique,
			Model model
			) {
	
		HttpStatus status = HttpStatus.OK;			
		String view = "/"+getRequestMappingRootPatternValue();

		model.addAttribute("responseStatus" , status);
		model.addAttribute("responseDto" , unique);
		model.addAttribute("responseView" , view);		
		return view;
	}	
}