<bind tag="pageTitle">Projects by Tag</bind>
<apply template="_project_list">
	<h1><pageTitle /></h1>

	<ul class="nested-keywords">
		<category><li><name>Language</name>
			<ul>
				<tag><li><a href="/projects/tags/${name}"><name>Web</name></a></li></tag>
			</ul>
		</li></category>
	</ul>
</apply>
