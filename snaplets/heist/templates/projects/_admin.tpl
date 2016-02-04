<bind tag="pageTitle"><name>Project Name</name> Administration <pageSubTitle /></bind>
<apply template="/_admin">
<header>
	<h1><name>Project Name</name></h1>

	<nav>
		<ul>
			<li><a href="/projects/${slug}/">View</a></li>
			<li><a href="/admin/projects/${slug}/">Edit</a></li>
			<li><a href="/admin/projects/${slug}/components/">Components</a>
				<!--<ul>
					<isComponent><li><type>Component</type> (<date>2016-01-10</date>)
						<ul>
							<li><a href="/admin/projects/${slug}/components/${type}/${date}/">Edit</a></li>
							<li><a href="/admin/projects/${slug}/components/${type}/${date}/images">Images</a></li>
						</ul></li></isComponent>
					<li><a href="/admin/projects/${slug}/components/Design/">Add Design</a></li>
					<li><a href="/admin/projects/${slug}/components/Development/">Add Development</a></li>
					<li><a href="/admin/projects/${slug}/components/Flash/">Add Flash</a></li>
				</ul>--></li>
		</ul>
	</nav>
</header>

<section>
	<apply-content />
</section>
</apply>