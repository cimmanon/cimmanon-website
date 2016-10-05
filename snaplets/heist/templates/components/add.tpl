<bind tag="pageTitle">Add <name>Project Name</name> Component</bind>
<apply template="/_admin">
	<h1><pageTitle /></h1>

	<dfForm class="simplified">
		<dfChildErrorList class="dialog error" />

		<apply template="_form" />
		<input type="submit" value="Add" />
	</dfForm>
</apply>