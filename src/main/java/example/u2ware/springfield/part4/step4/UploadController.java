package example.u2ware.springfield.part4.step4;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.u2ware.springfield.support.multipart.MultipartFileHandler;
import com.u2ware.springfield.view.multipart.MultipartFileController;

@Controller
@RequestMapping("/part4/step4/")
public class UploadController extends MultipartFileController{
	
	@Autowired
	private MultipartFileHandler multipartFileHandler;
	
	public MultipartFileHandler getMultipartFileHandler() {
		return multipartFileHandler;
	}

	@RequestMapping(value = "/upload", method = RequestMethod.GET)
	public String uploadForm() throws Exception {
		return "/part4/step4/upload";
	}
}
