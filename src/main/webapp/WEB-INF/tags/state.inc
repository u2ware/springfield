
<hr/>
<xmp>
sessionScope
	<c:forEach var="item" items="${sessionScope}">${item.key}=${item.value}
	</c:forEach>
requestScope
	<c:forEach var="item" items="${requestScope}">${item.key}=${item.value}
	</c:forEach>
applicationScope
	<c:forEach var="item" items="${applicationScope}">${item.key}=${item.value}
	</c:forEach>
pageScope
	<c:forEach var="item" items="${pageScope}">${item.key}=${item.value}
	</c:forEach>
param
	<c:forEach var="item" items="${param}">${item.key}=${item.value}
	</c:forEach>
paramValues
	<c:forEach var="item" items="${paramValues}">${item.key}=${item.value}
	</c:forEach>
paths
	Request URL : <%=request.getRequestURL()%>
	Request URI : <%=request.getRequestURI()%>
	Context Path : <%=request.getContextPath()%>
	Servlet Path : <%=request.getServletPath()%>
encoding
	request  : <%=request.getCharacterEncoding()%>
	response : <%=response.getCharacterEncoding()%>
header
	<c:forEach var="item" items="${header}">${item.key}=${item.value}
	</c:forEach>
headerValues
	<c:forEach var="item" items="${headerValues}">${item.key}=${item.value}
	</c:forEach>
cookie
	<c:forEach var="item" items="${cookie}">${item.key}=${item.value.value}
	</c:forEach>
initParam
	<c:forEach var="item" items="${initParam}">${item.key}=${item.value}
	</c:forEach>
</xmp>

