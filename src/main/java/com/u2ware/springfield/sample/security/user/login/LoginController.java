package com.u2ware.springfield.sample.security.user.login;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;



@Controller
public class LoginController {
	
	private static final Logger logger = LoggerFactory.getLogger(LoginController.class);
	
	
	@RequestMapping(value="/security/user/loginForm",method=RequestMethod.GET)
	public String loginForm(@RequestParam(value="errorCode",required=false)String errorCode, Model model){

		Object principal = null;

		Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
		if(authentication != null){

			logger.debug(""+authentication.getCredentials());
			logger.debug(""+authentication.getDetails());
			logger.debug(""+authentication.getAuthorities());
			
			Object p = authentication.getPrincipal();
			logger.debug(""+p.getClass());
			
			if(! "anonymousUser".equals(p)){
				principal = p;
			}
		}

		
		model.addAttribute("principal", principal);
		model.addAttribute("errorCode", errorCode);
		
		
		logger.debug("principal : "+ principal);
		logger.debug("errorCode : "+ errorCode);
		
		return "loginForm";
	}
	
/*	
	@Autowired(required=false)
	private AuthenticationUserService<Users> userService;
	
	@RequestMapping(value="/security/user/loginForm",method=RequestMethod.GET)
	public ModelAndView loginForm(HttpSession session, @RequestParam(value="errorCode",required=false)String errorCode, Model model){

		if(userService.hasPrinciple()){
			//RedirectView rv = new RedirectView("/security/member/username/current/edit", true);
			//rv.setExposeModelAttributes(false);
			//return new ModelAndView(rv);
			UserDetails user = userService.getPrincipal();
			model.addAttribute("user", user);
			return new ModelAndView("loginForm");
		}
		
	}
*/
}
