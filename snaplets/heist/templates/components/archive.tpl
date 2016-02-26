<bind tag="bodyId">archive</bind>
<bind tag="pageTitle">Project Archive: <name>Project Name</name></bind>
<apply template="_layout_header_only">
<main>
	<h1><name>Project Name</name></h1>

	<archived><iframe src="${href}" /></archived>

	<footer>
		<a href="/projects/${slug}/"><name>Project Name</name></a>, as it appeared <date format="%B %Y">February 2016</date>
	</footer>
</main>
</apply>